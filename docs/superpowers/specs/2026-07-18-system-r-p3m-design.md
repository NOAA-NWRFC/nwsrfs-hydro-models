# Switch from pixi-managed R to system R with P3M binaries

Date: 2026-07-18
Status: approved

## Goal

Stop managing R through pixi. Use the system R installed by rig, pull precompiled
binary packages from Posit Package Manager (P3M), and make renv the working local
environment instead of something every task disables. Keep renv rather than
switching to rv; leave a note to revisit rv once it reaches 1.0.

## Background

- pixi currently provides `r-base` plus `r-testthat`, `r-devtools`, `r-roxygen2`,
  and `r-pkgdown` from conda-forge. This conflicts with renv, and renv installs
  against the CRAN source repo, so development packages compile from source.
- Both CI and the pixi tasks set `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`. The renv
  library is not even materialized in a fresh checkout.
- CI already uses system R: `test-r.yml` runs r-lib/actions with
  `use-public-rspm: true`. CI does not change in this design.
- rig is installed with R 4.6.1 as the default. `renv.lock` still pins R 4.5.3
  with plain CRAN as the repository.
- rv (A2-ai) is at v0.19.0, active but pre-1.0 and single vendor. Not mature
  enough to hold the reproducible dev toolchain this project wants.

## Design

### 1. pixi.toml

Remove `r-base`, `r-testthat`, `r-devtools`, `r-roxygen2`, and `r-pkgdown` from
`[dependencies]`. Keep Python, the conda compilers, meson, quarto, sphinx, and
tidy-html5. Regenerate `pixi.lock`. Task names do not change; `R` now resolves
to the rig install at `/usr/local/bin/R` through the inherited PATH.

### 2. renv

- Update `renv.lock`: repository becomes
  `https://packagemanager.posit.co/cran/latest`, pinned R becomes 4.6.1.
- Fresh `renv::restore()` pulls arm64 binaries from P3M, then re-snapshot.
  On macOS and Windows, P3M serves binaries from the same URL as source. On
  Linux, renv's `ppm.enabled` default rewrites the URL to the distro binary
  repo, so collaborators get binaries too.
- The R tasks in pixi.toml stop setting `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`
  and instead run with the renv project active. Tasks launch R from `nwsrfs_r/`
  so `.Rprofile` activates renv; a task that must run from another directory
  sets `R_LIBS` to the renv library path. `install-r` installs nwsrfsr into the
  renv library, and `test-r`, `check-r`, and `build-docs-r` find testthat,
  devtools, and pkgdown there.
- CI keeps disabling the autoloader. r-lib/actions manages dependencies there.

### 3. Fortran toolchain

System R expects CRAN's gfortran at `/opt/gfortran`, which is not installed on
this machine. Install CRAN's gfortran 14.2 universal package from
mac.r-project.org/tools. This matches CRAN's mac builder exactly, which fits
the project's CRAN parity work. It is a documented prerequisite, not automated
by the repo. Homebrew gfortran via `~/.R/Makevars` was considered and rejected
because it diverges from the CRAN toolchain.

### 4. Documentation

`dev/README.md` gains a setup section covering:

- rig for installing and switching R versions (`rig add`, `rig default`)
- the CRAN gfortran prerequisite
- bootstrapping with `renv::restore()` in `nwsrfs_r/`
- P3M as the package source, so installs are binary and fast
- the rv breadcrumb: rv is the likely successor to renv here; revisit when it
  reaches 1.0

Update the top-level README if it describes pixi-managed R.

## Risks and verification

1. Conda compiler activation exports `FC` and `FFLAGS` inside `pixi run`. R's
   Makeconf assignments should override the environment, but the install log
   must show `/opt/gfortran/bin/gfortran` compiling the R package.
2. `R CMD check` runs in subprocesses. Verify the check finds testthat and the
   other Suggests packages from the renv library.
3. The R build switches compiler from conda gfortran to CRAN gfortran 14.2.
   `-ffp-contract=off` already guards the main source of numerical divergence,
   and the Python build keeps conda gfortran unchanged, but the gate is a full
   `pixi run test-all` plus `pixi run check-r` with no baseline movement.

## Out of scope

- CI workflow changes (already on system R and P3M)
- Any change to the Python build or its toolchain
- Adopting rv
