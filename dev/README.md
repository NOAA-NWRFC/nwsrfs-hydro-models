# Developer tooling

Helpers for maintaining the R package. Not part of the package or the CRAN
build.

## R development environment

R is not managed by pixi. Install and switch R versions with
[rig](https://github.com/r-lib/rig):

    rig list             # installed versions
    rig add release      # install the current release
    rig default 4.6      # pick the default R

Two one-time prerequisites:

1. CRAN's Fortran toolchain, which R's Makeconf expects at `/opt/gfortran`:

       curl -fLO https://mac.r-project.org/tools/gfortran-14.2-universal.pkg
       sudo installer -pkg gfortran-14.2-universal.pkg -target /

2. The R development packages (testthat, pkgdown, devtools, roxygen2),
   restored by renv into the project's renv library (kept in the user cache
   directory, since the project has a DESCRIPTION file):

       cd nwsrfs_r && Rscript -e 'renv::restore()'

renv installs from Posit Package Manager
(`https://packagemanager.posit.co/cran/latest`), which serves precompiled
binaries for macOS, Windows and major Linux distros, so restores do not
compile from source. The pixi R tasks (`pixi run test-r` and friends) use
the system R and this renv library; only the Python toolchain still comes
from conda. Maintainer-only packages are declared in
`nwsrfs_r/dev-dependencies.R` so `renv::snapshot()` keeps them in the
lockfile.

[rv](https://github.com/A2-ai/rv) is a likely successor to renv here (a
declarative manager built around a lockfile), but it was pre-1.0 (v0.19) as
of July 2026. Revisit once it reaches 1.0.

## Cross-package test suite

- `dev/test-cross-package.R` holds the R vs Python comparisons that used to
  live in the R package test suite (two strict simulation baselines, two
  adjustq baselines). Run with `pixi run test-cross` (installs both
  packages first). These only make sense in the monorepo, so they are not
  part of the CRAN package tests.
- `dev/regenerate-adjustq-baselines.py` regenerates
  `nwsrfs_py/nwsrfs_py/data/Adjustq_check/*.csv` from the current Python
  build (`pixi run 'python dev/regenerate-adjustq-baselines.py'`).

## `check-local.sh` — CRAN-style instrumented checks in Docker

Runs the two fast instrumented checks from
`.github/workflows/cran-instrumented.yml` locally, so the problem classes that
only appear on CRAN's Linux builds are caught before pushing:

```bash
dev/check-local.sh          # lto, then bounds
dev/check-local.sh lto      # LTO only  (~2-4 min)
dev/check-local.sh bounds   # -fcheck=all only  (~4-6 min)
```

- **lto** — `R CMD INSTALL --use-LTO`; fails on `-Wlto-type-mismatch`
  (inconsistent Fortran `COMMON` layouts / call signatures).
- **bounds** — `R CMD check` with `-fcheck=all`; a runtime out-of-bounds array
  access aborts with a precise `file:line`.

Requires Docker (Desktop or OrbStack) running. Uses `rocker/r-ver` pinned to
`--platform linux/amd64` so the toolchain matches CRAN; on Apple Silicon this
runs under emulation (first run pulls the image and installs `testthat`).

The slower **gcc-ASAN** and **valgrind** checks are intentionally left out — under
emulation they take 30-60 min and many hours. Run those in CI
(`cran-instrumented.yml`, `valgrind.yml`) or via r-hub.

### Why the bounds check does not compare against Python baselines

`check-local.sh` checks the tarball in isolation, so the R package's own
tests must be self-contained: none of them depend on the sibling
`nwsrfs_py/` directory being present. The R vs Python comparisons live
outside the package, in `dev/test-cross-package.R` (see "Cross-package test
suite" above), and only run when invoked directly with `pixi run
test-cross`.

## Cross-platform floating-point reproducibility

Before 1.0.3 the simulation differed between macOS and Linux by up to a few
hundred cfs at individual flow peaks. A bit-level trace of every
transcendental call in the SAC/SNOW17 path found two causes:

1. Floating-point contraction, the dominant effect. Compilers may fuse
   `a*b+c` into a fused multiply-add with a single rounding. Whether they do
   varies by gfortran version (13 vs 14/16 decide differently) and by CPU
   (aarch64 fuses, baseline x86-64 cannot). The one-ulp differences are
   amplified by snow model thresholds and the spin-up loop into visibly
   different simulations.
2. libm rounding, the small residual. Apple libm and glibc differ by one ulp
   on some arguments of `powf` (SAC percolation, `sac1.f`), `expf`
   (`rout19.f`, `PACK19.f`) and double `pow` (ADC table setup). Most of these
   are absorbed by `int()` quantization; the survivors grow to only ~1e-4 mm
   of total channel inflow over a 43 year run.

The fix is `-ffp-contract=off` for all Fortran compiles. On Unix the package
`configure` script probes the compiler and substitutes the flag into a
generated `src/Makevars` (from `Makevars.in`); `Makevars.win` sets it directly
because Rtools is always gfortran. The Python package applies the same flag
through meson. With contraction off, results agree across macOS, Linux,
x86-64 and arm64 to within the libm residual, and the baseline CSVs
(regenerated once from the non-contracting build) match on every platform.

The R-vs-Python agreement carries a fixed residual worth knowing about. The
strict baseline tests in `test-nwsrfs-run.R` compare the R simulation against
CSVs produced by the Python package, with total absolute tolerances of 2 cfs
(NRKW1) and 0.25 cfs (SFLN2) over the full 43 year runs. They currently sit
at about 1.9 and 0.235 cfs, identically on every platform. That 94-96% margin
is the genuine difference between the two wrapper layers around the shared
Fortran (R glue vs pandas time handling), not platform noise or flakiness, so
treat any movement in these totals as a real regression in one of the
wrappers.

Two traps to remember:

- The explicit rules in `Makevars.in` for the legacy `.f` files bypass R's
  implicit rule, so they must carry `$(PKG_FFLAGS)` themselves. Before the
  fix they dropped user FFLAGS entirely, which is why earlier `~/.R/Makevars`
  experiments with `-ffp-contract` appeared to have no effect (`exsnow19.f`
  is in the hot path).
- The flag must never appear literally in a shipped `PKG_FFLAGS`: R CMD check
  warns about any `-f*` flag there. The `@FPCONTRACT@` placeholder in
  `Makevars.in` is what the check sees, and the generated `src/Makevars` is
  excluded from the tarball by `.Rbuildignore` and removed by `cleanup`.

The configure indirection is the only mechanism that works. The seemingly
simpler alternatives all fail: a literal flag in `PKG_FFLAGS` draws the
"Non-portable flags" WARNING (verified; it blocks a CRAN submission);
overriding R's implicit `.f.o`/`.f90.o` rules from `Makevars` is silently
ignored because R prepends `Makevars` to the make invocation and `Makeconf`,
read afterwards, wins (verified: no implicit-rule compile received the flag);
and target-specific `foo.o: PKG_FFLAGS = ...` assignments are both
GNU-make-specific and explicitly linted by the same check. The last resort,
explicit rules for every one of the ~38 Fortran files, would work but is
longer than the configure script and silently loses the flag for any new
source file.

## Full CRAN platform matrix on demand — r-hub v2

rhub v2 runs on the R-Consortium runners from a locally built tarball, so the
monorepo stays intact (no need to move the R package to a repo root, which would
fork the shared `model_source` symlink). `dev/rhub-submit.sh` wraps the whole
flow: it assembles a clean package tree (symlinks materialized, renv and stale
artifacts stripped), runs `R CMD build`, and submits the tarball:

```bash
dev/rhub-submit.sh                # build + submit to the default set:
                                  #   gcc-asan clang-asan valgrind nold ubuntu-next
dev/rhub-submit.sh valgrind       # specific platforms
dev/rhub-submit.sh --list         # show all available platforms
dev/rhub-submit.sh --build-only   # just build the tarball, no submission
```

One-time setup:

```r
install.packages("rhub")
rhub::rc_new_token()   # email token; the submit email defaults to the
                       # DESCRIPTION maintainer, set RHUB_EMAIL to override
```

Results and the tarball become public on the `r-hub2` GitHub org; allow at least
five minutes between submissions. The script prompts before submitting for this
reason (pass `--yes` to skip).
