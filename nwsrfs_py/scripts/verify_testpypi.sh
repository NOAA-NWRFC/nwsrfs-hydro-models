#!/usr/bin/env bash
set -euo pipefail

# ==============================================================================
# TestPyPI Installation Verification Script
# ==============================================================================
#
# Description:
#   Creates an isolated temporary virtual environment, installs 'nwsrfspy'
#   from TestPyPI, verifies package import, and automatically cleans up.
#
# Complete Release Workflow:
#   1. Stage source distribution (.tar.gz) and materialize symlinks:
#        pixi run stage-py-release
#        (or: ./nwsrfs_py/scripts/release_stage.sh)
#
#   2. Download compiled wheel artifacts (.whl) from GitHub Actions and copy
#      them into the staging dist directory:
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
#   5. Upload to production PyPI (handled by publish_pypi.sh):
#        pixi run publish-py-pypi
#        (or: ./nwsrfs_py/scripts/publish_pypi.sh pypi)
#
# ==============================================================================

usage() {
  cat <<'EOF'
Usage:
  nwsrfs_py/scripts/verify_testpypi.sh [PKG_NAME] [MODULE_NAME]

Description:
  Creates an isolated temporary virtual environment, installs the package
  from TestPyPI, verifies basic package import, and automatically removes
  the temporary environment on completion.

Arguments:
  PKG_NAME     Optional PyPI package distribution name to install.
               Default: nwsrfspy
  MODULE_NAME  Optional Python module name to test importing.
               Default: nwsrfs_py

Options:
  -h, --help   Show this help message and exit.

Examples:
  # Verify default package (nwsrfspy / import nwsrfs_py):
  ./nwsrfs_py/scripts/verify_testpypi.sh

  # Verify custom distribution and module names:
  ./nwsrfs_py/scripts/verify_testpypi.sh nwsrfspy nwsrfs_py
EOF
}

# Check for help flags
if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

PKG_NAME="${1:-nwsrfspy}"
MODULE_NAME="${2:-nwsrfs_py}"

echo "========================================================"
echo "  Verifying TestPyPI Installation: ${PKG_NAME}"
echo "  Testing Module Import: ${MODULE_NAME}"
echo "========================================================"

# Create a unique temporary directory
TMP_DIR=$(mktemp -d -t nwsrfspy_verify_XXXXXX)

# Trap EXIT signal to guarantee cleanup even if script fails or is aborted
trap 'echo ""; echo "Cleaning up temporary environment: $TMP_DIR"; rm -rf "$TMP_DIR"' EXIT

echo "Creating temporary virtual environment in: $TMP_DIR"
python -m venv "$TMP_DIR/venv"

# Activate temporary environment (supports macOS/Linux and Windows Git Bash)
if [[ -f "$TMP_DIR/venv/bin/activate" ]]; then
  source "$TMP_DIR/venv/bin/activate"
elif [[ -f "$TMP_DIR/venv/Scripts/activate" ]]; then
  source "$TMP_DIR/venv/Scripts/activate"
else
  echo "ERROR: Could not find virtual environment activation script."
  exit 1
fi

echo "Upgrading pip in temporary environment..."
pip install --quiet --upgrade pip

echo "Installing ${PKG_NAME} from TestPyPI..."
pip install --no-cache-dir \
  --index-url https://test.pypi.org/simple/ \
  --extra-index-url https://pypi.org/simple \
  "${PKG_NAME}"

echo "Testing package import: import ${MODULE_NAME}..."
python -c "import ${MODULE_NAME}; print(f'Successfully imported ${MODULE_NAME} from TestPyPI!')"

echo "========================================================"
echo "  SUCCESS: TestPyPI package verified successfully!"
echo "========================================================"