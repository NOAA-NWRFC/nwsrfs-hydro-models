# Package Staging & Release Scripts

This directory contains shell scripts used to stage, verify, and publish the `nwsrfspy` Python package to **TestPyPI** and **PyPI**.

---

## Script Overview

| Script | Purpose |
| :--- | :--- |
| `release_stage.sh` | Materializes repo symlinks (`src`, `LICENSE`, `NOTICE`), cleans out stale binary artifacts, and generates the Python source distribution (`.tar.gz`). |
| `publish_pypi.sh` | Verifies that both the source distribution and binary wheels (`.whl`) exist in staging, prompts for interactive confirmation, and uploads them via Twine. |
| `verify_testpypi.sh` | Creates an isolated temporary virtual environment, installs the package from TestPyPI, verifies basic package import, and automatically cleans up
---

## End-to-End PyPI Release Workflow

The recommended way to run these scripts is via `pixi` tasks defined in the workspace `pixi.toml`.

### Step 1: Stage the Source Distribution (`.tar.gz`)

Run the staging script to create a clean build environment in `~/tmp/nwsrfs_py_release/nwsrfs_py` and generate the source tarball:

```bash
pixi run stage-py-release
# Or run directly:
./nwsrfs_py/scripts/release_stage.sh [PIXI_ENV] [STAGE_DIR]
``` 

### Step 2: Download Binary Wheels (`.whl`) from GitHub Actions

1. Go to the **Actions** tab on GitHub and open the completed **Build Wheels (`cibuildwheel`)** workflow run for your tag/release commit.
2. Download the compiled wheel artifacts:
   * `wheels-ubuntu-latest`
   * `wheels-macos-latest`
   * `wheels-windows-latest`
3. Unzip each wheel-[system]-latest.zip
3. Copy all downloaded `.whl` files into the staging `dist/` directory alongside the `.tar.gz` file:

```bash
cp ~/Downloads/wheels-*/*.whl ~/tmp/nwsrfs_py_release/nwsrfs_py/dist/
```

### Step 3: Test Upload to TestPyPI

Run the upload task targeting TestPyPI. The script verifies that both a `.tar.gz` file and `.whl` files exist in the staging directory before asking for confirmation (`y/N`):

```bash
pixi run publish-py-testpypi
# Or run directly:
./nwsrfs_py/scripts/publish_pypi.sh testpypi
```

### Step 4: Verify TestPyPI Installation

In a clean virtual environment, test installing the newly uploaded package:

```bash
pixi run verify-py-testpypi
# Or run directly:
./nwsrfs_py/scripts/verify_testpypi.sh 
```

### Step 5: Upload to Production PyPI

Once verified, publish the staged artifacts to production PyPI. As a safety guard against accidental releases, this task requires you to explicitly type `PUBLISH` to proceed:

```bash
pixi run publish-py-pypi
# Or run directly:
./nwsrfs_py/scripts/publish_pypi.sh pypi
```

---

## Script Reference

### `release_stage.sh`

* **Purpose**: Prepares a clean staging folder, materializes symlinks, cleans binary artifacts, and builds the source distribution (`.tar.gz`).
* **Arguments**:
  * `PIXI_ENV` *(Optional)*: Pixi environment name (`default`, `py310`, `py311`, `py312`). Default: `default`.
  * `STAGE_DIR` *(Optional)*: Destination path. Default: `$HOME/tmp/nwsrfs_py_release`.

### `publish_pypi.sh`

* **Purpose**: Checks for required `.tar.gz` and `.whl` files in the staging directory and uploads them using Twine.
* **Arguments**:
  * `TARGET` *(Optional)*: Upload destination (`testpypi` or `pypi`). Default: `testpypi`.
  * `STAGE_DIR` *(Optional)*: Directory containing staged build artifacts. Default: `$HOME/tmp/nwsrfs_py_release/nwsrfs_py/dist`.
* **Safety Guards**:
  * Checks that at least one `.tar.gz` source package is present.
  * Checks that at least one `.whl` binary wheel is present.
  * Prompts for confirmation (`y/N`) when targeting `testpypi`.
  * Requires typing `PUBLISH` in all-caps when targeting production `pypi`.

### `verify_testpypi.sh`

* **Purpose**: Creates an isolated temporary virtual environment, installs the package from TestPyPI, verifies basic package import, and automatically cleans up upon completion.
* **Arguments**:
  * `PKG_NAME` *(Optional)*: PyPI package distribution name to install. Default: `nwsrfspy`.
  * `MODULE_NAME` *(Optional)*: Python module name to import and verify. Default: `nwsrfs_py`.
* **Features & Isolation**:
  * Creates a unique temporary directory and virtual environment (`mktemp -d` and `python -m venv`) to avoid polluting active workspace environments.
  * Uses `--no-cache-dir` to force `pip` to download artifacts directly from TestPyPI.
  * Tests basic Python module import functionality.
  * Uses a shell `trap` command to guarantee automatic cleanup of temporary files upon exit.