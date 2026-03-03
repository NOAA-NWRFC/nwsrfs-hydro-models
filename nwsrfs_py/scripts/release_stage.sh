#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/release_stage.sh [CONDA_ENV_PREFIX] [STAGE_DIR]

Description:
  Creates a clean staging copy of nwsrfs_py, materializes symlinked source files,
  removes compiled artifacts, then runs:
    python -m build
    python -m twine check dist/*

Arguments:
  CONDA_ENV_PREFIX
              Optional conda environment prefix path.
              Example: /Users/you/miniconda3/envs/nwsrfs-pypi
              If omitted, uses current PATH python.
  STAGE_DIR   Optional destination directory.
              Default: /tmp/nwsrfs_py_release
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_ROOT="$(cd "${PKG_DIR}/.." && pwd)"
CONDA_ENV_PREFIX="${1:-}"
STAGE_DIR="${2:-/tmp/nwsrfs_py_release}"
RELEASE_DIR="${STAGE_DIR}/nwsrfs_py"

if [[ -n "${CONDA_ENV_PREFIX}" ]]; then
  PYTHON_BIN="${CONDA_ENV_PREFIX}/bin/python"
  if [[ ! -x "${PYTHON_BIN}" ]]; then
    echo "ERROR: Python not found at ${PYTHON_BIN}"
    exit 1
  fi
else
  PYTHON_BIN="python"
fi

echo "Using Python: ${PYTHON_BIN}"
if ! "${PYTHON_BIN}" -m pip show build twine >/dev/null 2>&1; then
  echo "ERROR: Missing required tools in selected environment: build and/or twine"
  echo "Install with:"
  echo "  ${PYTHON_BIN} -m pip install -U build twine"
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

echo "Building distribution artifacts"
(
  cd "${RELEASE_DIR}"
  "${PYTHON_BIN}" -m build
  "${PYTHON_BIN}" -m twine check dist/*
)

echo "Staging build complete"
echo "Artifacts are in: ${RELEASE_DIR}/dist"
