# AGENTS.md

Guidance for coding agents working in `nwsrfs-hydro-models`.

## Project Overview

`nwsrfs-hydro-models` wraps operational NWSRFS (National Weather Service River Forecast System) hydrologic models for standalone use in R and Python:

- **Models**: `SAC-SMA`, `SNOW-17`, `UNIT-HG`, `LAG-K`, `CHANLOSS`, `CONS_USE`, plus `AdjustQ` observed-flow preprocessing and `FA_NWRFC` forcing adjustments.
- **Timestep**: Validated at a 6-hour simulation timestep.
- **Reference**: Walters et al. (2026), *JAWRA*, <https://doi.org/10.1111/1752-1688.70112>.

## Repository Layout and Ownership

- `model_source/`: Shared Fortran 77/90 source (`.f`, `.f90`), R's `Makevars.in` and `Makevars.win`, and `init.c`. Both packages symlink `src -> ../model_source`, so any Fortran change affects both R and Python.
- `nwsrfs_r/`: R package (`nwsrfsr`, on CRAN), maintained by Cameron Bracken. Calls Fortran via `.Fortran()`.
- `nwsrfs_py/`: Python package (`nwsrfspy`, on PyPI), maintained by Geoffrey Walters. Built with `meson-python` and `f2py`. Ask before modifying `nwsrfs_py/` when working on R or repo-level tasks.
- `dev/`: Maintainer scripts and developer documentation (`dev/README.md`, `dev/test-cross-package.R`, `dev/check-local.sh`, `dev/rhub-submit.sh`).
- `pages/`: Landing page assets for the combined GitHub Pages documentation site.

## Environment Setup

- **Pixi (`pixi.toml`)**: Orchestrates tasks across the monorepo and manages the Python, Fortran (conda-forge), Sphinx, and Quarto toolchains.
- **R (`rig` + `rv`)**: R itself is not managed by `pixi`. Use system R (installed via `rig`) together with [`rv`](https://github.com/A2-ai/rv). `nwsrfs_r/rproject.toml` and `nwsrfs_r/rv.lock` manage R development dependencies (`devtools`, `ggplot2`, `knitr`, `pkgdown`, `quarto`, `roxygen2`, `testthat`) from Posit Package Manager (`https://packagemanager.posit.co/cran/latest`).
  ```bash
  cd nwsrfs_r && rv sync
  ```
  `nwsrfs_r/.Rprofile` activates the project library under `nwsrfs_r/rv/library/` whenever R runs with `nwsrfs_r/` as the working directory (including `pixi` R tasks). On macOS, CRAN R expects `/opt/gfortran` (see `dev/README.md`).

## Common Commands

Run from the repository root via `pixi`:

```bash
# R package
pixi run install-r             # R CMD INSTALL into the rv library
pixi run test-r                # testthat suite
pixi run check-r               # R CMD build + R CMD check --no-manual
pixi run check-r-cran          # R CMD build + R CMD check --as-cran
pixi run build-docs-r          # pkgdown site (nwsrfs_r/docs/)
pixi run build-docs-r-readme   # render nwsrfs_r/README.qmd to README.md and README.html

# Python package
pixi run install-py            # editable pip install (--no-build-isolation)
pixi run test-py               # pytest suite (use -e py311..py314 for matrix versions)
pixi run build-docs-py         # Sphinx HTML docs

# Cross-package and pre-CRAN checks
pixi run test-cross            # R vs Python simulation, AdjustQ, and argument-matrix parity
dev/check-local.sh             # Docker LTO and -fcheck=all Fortran bounds checks
```

When regenerating R documentation (`man/` and `NAMESPACE`) from roxygen comments:

```bash
cd nwsrfs_r && Rscript -e 'roxygen2::roxygenize()'
```

## Architecture Notes

### R Package (`nwsrfs_r/`)
- `R/sac-snow-uh.R`: Low-level `.Fortran()` wrappers (`sac_snow()`, `uh()`, `lagk()`, `chanloss()`, `consuse()`, `fa_nwrfc()`, `rsnwelev()`, `pet_ind()`).
- `R/nwsrfs-run.R`: High-level `nwsrfs_run()`, `load_example()`, and `update_pars()` workflow.
- `R/adjustq.R`: `adjustq()` and `adjustq_load_example()` for observed daily/instantaneous flow adjustment.
- `R/data.R` and `data/*.rda`: Bundled `NRKW1` and `SFLN2` example datasets (prepared from `data-raw/prepare_example_data.R`).
- `R/plots.R`: Daily gauge alignment (`sim_to_daily()`), validation metrics (`flow_metrics()`), `plot.nwsrfs_run()`, and `ggplot2` hydrological diagnostics (`plot_hydrograph()`, `plot_water_year()`, `plot_fdc()`, `plot_monthly_bias()`, `plot_cumulative()`, `plot_scatter()`, `plot_water_supply()`, `plot_states()`, `plot_runoff_regime()`, `plot_water_balance()`, `plot_forcing_adjust()`, `plot_uh()`, `plot_adc()`, `plot_lagk()`, `theme_nwsrfs()`).
- `vignettes/nwsrfsr.qmd`: Introductory package vignette (Quarto).

### Python Package (`nwsrfs_py/`)
- `nwsrfs_py/wrapper/nwsrfs_f2py_wrapper.py`: Low-level classes wrapping compiled `nwsrfs_src` `f2py` routines.
- `nwsrfs_py/simulation.py`: `NwsrfsRun` orchestration (`NwsrfsRun.load_example("NRKW1")`).
- `nwsrfs_py/adjustq.py`: `AdjustQ` preprocessing class.

### Shared Fortran (`model_source/`)
- **Module order**: `types.f90` -> `sorting.f90` -> `stats.f90` -> `utilities.f90` must compile in order; explicit dependencies live in `Makevars.in` and `Makevars.win`.
- **Legacy Fortran 77**: `fka7.f`, `flag7.f`, `pin7.f`, `ex57.f`, and `exsnow19.f` use legacy constructs and compile via explicit rules without `-std=f95`. Keep `$(PKG_FFLAGS)` on those rules.
- **Floating-point contraction**: Cross-platform reproducibility requires `-ffp-contract=off`. On Unix, `nwsrfs_r/configure` probes the Fortran compiler and substitutes `@FPCONTRACT@` into `src/Makevars` from `model_source/Makevars.in` (`src/Makevars` is `.Rbuildignore`d and removed by `cleanup`). Never put literal `-f*` flags in a shipped `Makevars`, as CRAN rejects them as non-portable.
- **No nested callbacks**: Do not pass Fortran internal (nested) procedures as callbacks; `gfortran` emits executable-stack trampolines (`GNU_STACK` `RWE`) that fail to load on Debian/CRAN. Use module-level procedures instead.

## Code Style

### R
- Use `=` for assignment (not `<-` or `->`), except inside function arguments where standard R semantics apply.
- Use the base pipe `|>` (not `%>%`). Place variable assignment on the first line and start piped steps on the next line.
- Use `\(x)` shorthand for inline anonymous functions; `function(...)` is fine for multi-line named function definitions.
- Use `# %%` section markers in standalone scripts (such as `dev/` and `data-raw/`), keeping file paths and configuration at the top.
- Do not add `air.toml` or enforce an automatic R code formatter.

### Python
- Follow existing formatting (`ruff` / `black` compatible) and NumPy-style docstrings.
