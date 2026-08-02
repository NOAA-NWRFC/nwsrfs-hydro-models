#!/usr/bin/env bash
set -euo pipefail

# ==============================================================================
# PyPI Release Staging Script
# ==============================================================================
#
# Description:
#   Creates a clean staging copy of nwsrfs_py, materializes symlinked source
#   files, cleans compiled artifacts, and builds the source distribution (.tar.gz).
#
# Complete Release Workflow:
#   1. Stage source distribution (.tar.gz) and materialize symlinks:
#        pixi run stage-py-release
#        (or: ./nwsrfs_py/scripts/release_stage.sh)
#
#   2. Download compiled wheel artifacts (.whl) from GitHub Actions and copy
#      them into the staging dist directory:
#        unzip each wheel-[system]-latest.zip
#        cp ~/Downloads/wheels-*/*.whl ~/tmp/nwsrfs_py_release/nwsrfs_py/dist/
#
#   3. Upload to TestPyPI (handled by publish_pypi.sh):
#        pixi run publish-py-testpypi
#        (or: ./nwsrfs_py/scripts/publish_pypi.sh testpypi)
#
#   4. Verify package on TestPyPI:
#        pixi run verify-py-testpypi
#        (or: ./nwsrfs_py/scripts/verify_testpypi.sh)
#
#   5. Upload to production PyPI (handled by publish_pypi.sh):
#        pixi run publish-py-pypi
#        (or: ./nwsrfs_py/scripts/publish_pypi.sh pypi)
#
# ==============================================================================

usage() {
  cat <<'EOF'
Usage:
  nwsrfs_py/scripts/release_stage.sh [PIXI_ENV] [STAGE_DIR]

Description:
  Creates a clean staging copy of nwsrfs_py, materializes symlinked source files,
  removes compiled artifacts, and builds the source distribution (.tar.gz).

Complete Release Workflow:
  1. Run this script to generate sdist (.tar.gz):
       pixi run stage-py-release

  2. Download GitHub Actions wheels (.whl) into the staging dist directory:
       cp ~/Downloads/wheels-*/*.whl ~/tmp/nwsrfs_py_release/nwsrfs_py/dist/

  3. Test upload via publish_pypi.sh:
       pixi run publish-py-testpypi

  4. Verify TestPyPI installation:
       pip install --index-url https://test.pypi.org/simple/ \
                   --extra-index-url https://pypi.org/simple nwsrfspy

  5. Production upload via publish_pypi.sh:
       pixi run publish-py-pypi

Arguments:
  PIXI_ENV    Optional pixi environment name (default, py310, py311, py312).
              Default: default
  STAGE_DIR   Optional destination directory.
              Default: $HOME/tmp/nwsrfs_py_release
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_ROOT="$(cd "${PKG_DIR}/.." && pwd)"

PIXI_ENV="${1:-default}"
DEFAULT_TMP_BASE="$HOME/tmp"
STAGE_DIR="${2:-${DEFAULT_TMP_BASE}/nwsrfs_py_release}"
RELEASE_DIR="${STAGE_DIR}/nwsrfs_py"

# Verify pixi is available in PATH
if ! command -v pixi >/dev/null 2>&1; then
  echo "ERROR: 'pixi' command not found in PATH."
  echo "Please install pixi (https://pixi.sh) before running this script."
  exit 1
fi

echo "Using Pixi environment: ${PIXI_ENV}"

# Verify build and twine tools are available in the target pixi environment
if ! pixi run -e "${PIXI_ENV}" --manifest-path "${REPO_ROOT}/pixi.toml" python -c "import build, twine" >/dev/null 2>&1; then
  echo "ERROR: Missing required build tools ('build' and/or 'twine') in Pixi environment '${PIXI_ENV}'."
  echo "Ensure 'python-build' and 'twine' are specified in pixi.toml."
  exit 1
fi

echo "Preparing staging directory: ${STAGE_DIR}"
rm -rf "${STAGE_DIR}"
git clone --local "${REPO_ROOT}" "${STAGE_DIR}"

echo "Materializing symlinked build inputs"
if [[ -L "${RELEASE_DIR}/src" || ! -d "${RELEASE_DIR}/src" ]]; then
  rm -rf "${RELEASE_DIR}/src"
  cp -R "${REPO_ROOT}/model_source" "${RELEASE_DIR}/src"
fi

if [[ -L "${RELEASE_DIR}/LICENSE" || ! -f "${RELEASE_DIR}/LICENSE" ]]; then
  rm -f "${RELEASE_DIR}/LICENSE"
  cp "${REPO_ROOT}/LICENSE" "${RELEASE_DIR}/LICENSE"
fi

if [[ -L "${RELEASE_DIR}/NOTICE" || ! -f "${RELEASE_DIR}/NOTICE" ]]; then
  rm -f "${RELEASE_DIR}/NOTICE"
  cp "${REPO_ROOT}/NOTICE" "${RELEASE_DIR}/NOTICE"
fi

echo "Removing compiled artifacts from staged src/"
find "${RELEASE_DIR}/src" -type f \
  \( -name '*.o' -o -name '*.so' -o -name '*.dylib' -o -name '*.a' -o -name '*.mod' -o -name '*.smod' \) \
  -delete

echo "Committing staged materialization for meson dist"
(
  cd "${RELEASE_DIR}"
  git config user.name "release-stage"
  git config user.email "release-stage@local"
  git add -A src LICENSE NOTICE
  if ! git diff --cached --quiet; then
    git commit -q -m "Temporary release materialization"
  fi
)

echo "Building distribution artifacts via Pixi"
(
  cd "${RELEASE_DIR}"
  pixi run -e "${PIXI_ENV}" --manifest-path "${STAGE_DIR}/pixi.toml" python -m build
  pixi run -e "${PIXI_ENV}" --manifest-path "${STAGE_DIR}/pixi.toml" python -m twine check dist/*
)

echo "Staging build complete"
echo "Source distribution artifacts are in: ${RELEASE_DIR}/dist"
echo ""
echo "Next Steps:"
echo "1. Copy downloaded wheels from GitHub Actions into: ${RELEASE_DIR}/dist/"
echo "2. Run 'pixi run publish-py-testpypi' (or publish_pypi.sh testpypi) to test upload."