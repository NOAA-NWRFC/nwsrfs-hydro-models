#!/usr/bin/env bash
set -euo pipefail

# ==============================================================================
# PyPI Artifact Publishing Script
# ==============================================================================
#
# Description:
#   Verifies and uploads staged Python package artifacts (.tar.gz and .whl)
#   to TestPyPI or production PyPI using Twine.
#
# Complete Release Workflow:
#   1. Stage source distribution (.tar.gz) and materialize symlinks:
#        pixi run stage-py-release
#        (or: ./nwsrfs_py/scripts/release_stage.sh)
#
#   2. Download compiled wheel artifacts (.whl) from GitHub Actions and copy
#      them into the staging dist directory:
#.       unzip each wheel-[system]-latest.zip
#        cp ~/Downloads/wheels-*/*.whl ~/tmp/nwsrfs_py_release/nwsrfs_py/dist/
#
#   3. Upload to TestPyPI:
#        pixi run publish-py-testpypi
#        (or: ./nwsrfs_py/scripts/publish_pypi.sh testpypi)
#
#   4. Verify package on TestPyPI:
#        pixi run verify-py-testpypi
#        (or: ./nwsrfs_py/scripts/verify_testpypi.sh)
#
#   5. Upload to production PyPI (requires typing 'PUBLISH'):
#        pixi run publish-py-pypi
#        (or: ./nwsrfs_py/scripts/publish_pypi.sh pypi)
#
# ==============================================================================

usage() {
  cat <<'EOF'
Usage:
  nwsrfs_py/scripts/publish_pypi.sh [TARGET] [STAGE_DIR]

Description:
  Verifies that both a source tarball (.tar.gz) and binary wheel(s) (.whl) exist
  in the staging directory, then uploads them to TestPyPI or PyPI via twine.

Arguments:
  TARGET      Upload target environment: 'testpypi' or 'pypi'.
              Default: testpypi
  STAGE_DIR   Directory containing staged build artifacts (.tar.gz and .whl).
              Default: $HOME/tmp/nwsrfs_py_release/nwsrfs_py/dist

Options:
  -h, --help  Show this help message and exit.

Examples:
  # Upload to TestPyPI (default):
  ./nwsrfs_py/scripts/publish_pypi.sh

  # Upload to production PyPI:
  ./nwsrfs_py/scripts/publish_pypi.sh pypi

  # Upload from a custom staging directory:
  ./nwsrfs_py/scripts/publish_pypi.sh testpypi /path/to/custom/dist
EOF
}

# Check for help flags
if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

TARGET="${1:-testpypi}"
STAGE_DIR="${2:-$HOME/tmp/nwsrfs_py_release/nwsrfs_py/dist}"

# 1. Verify staging directory exists
if [ ! -d "$STAGE_DIR" ]; then
  echo "ERROR: Staging dist directory does not exist!"
  echo "Expected path: $STAGE_DIR"
  echo "Ensure you ran 'pixi run stage-py-release' first."
  echo ""
  usage
  exit 1
fi

# 2. Check for required artifact types (.tar.gz and .whl)
shopt -s nullglob
SDISTS=("$STAGE_DIR"/*.tar.gz)
WHEELS=("$STAGE_DIR"/*.whl)
shopt -u nullglob

if [ ${#SDISTS[@]} -eq 0 ]; then
  echo "ERROR: Missing source tarball (.tar.gz) in staging directory!"
  echo "Expected path: $STAGE_DIR"
  echo "Run 'pixi run stage-py-release' to generate the source distribution."
  exit 1
fi

if [ ${#WHEELS[@]} -eq 0 ]; then
  echo "ERROR: Missing binary wheels (.whl) in staging directory!"
  echo "Expected path: $STAGE_DIR"
  echo "Download the wheel artifacts from GitHub Actions and copy them into dist/:"
  echo "  cp ~/Downloads/wheels-*/*.whl $STAGE_DIR/"
  exit 1
fi

echo "Staged artifacts ready for upload (${#SDISTS[@]} sdist, ${#WHEELS[@]} wheel(s)):"
ls -lh "$STAGE_DIR"
echo ""

# 3. Target verification & upload prompts
if [ "$TARGET" = "pypi" ]; then
  echo "========================================================"
  echo "  WARNING: YOU ARE ABOUT TO PUBLISH TO PRODUCTION PyPI  "
  echo "========================================================"
  read -r -p "Type 'PUBLISH' to confirm production release: " response
  if [ "$response" = "PUBLISH" ]; then
    twine upload "$STAGE_DIR"/*
    echo "!!! CONGRATULATIONS 'nwsrfs_py' as been uploaded to PyPi !!!"
  else
    echo "Production PyPI release aborted."
    exit 1
  fi
elif [ "$TARGET" = "testpypi" ]; then
  read -r -p "Upload these artifacts to TestPyPI? (y/N): " response
  if [[ "$response" =~ ^[yY] ]]; then
    twine upload --repository testpypi "$STAGE_DIR"/*
    echo "!!! CONGRATULATIONS 'nwsrfs_py' as been uploaded to TestPyPI !!!"
    echo "" 
    echo "Next Steps:"
    echo "Verify TestPyPI installation of nwsrfspy. Run 'pixi run publish-py-testpypi'"
  else
    echo "TestPyPI upload cancelled."
    exit 1
  fi
else
  echo "ERROR: Unknown target '$TARGET'. Must be 'testpypi' or 'pypi'."
  echo ""
  usage
  exit 1
fi