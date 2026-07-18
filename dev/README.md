# Developer tooling

Helpers for maintaining the R package. Not part of the package or the CRAN
build.

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

Four tests compare the simulation against Python baseline CSVs in `nwsrfs_py/`.
Those baselines were generated on macOS/arm64; the single-precision SNOW17/SAC
math evaluates slightly differently on Linux/x86 (a `libm` floating-point
difference, present from the first timestep and independent of
`-finit`/`-ffp-contract`/`-O` level), so the comparisons cannot match on Linux.
That is why the package marks them `skip_on_cran()` / `skip_if_not(file.exists)`.
`check-local.sh` checks the tarball in isolation with `NOT_CRAN=false`, so they
skip exactly as they do on CRAN.

## Full CRAN platform matrix on demand — r-hub v2

rhub v2 runs on the R-Consortium runners from a locally built tarball, so the
monorepo stays intact (no need to move the R package to a repo root, which would
fork the shared `model_source` symlink):

```r
# install.packages("rhub")
rhub::rc_new_token()          # one-time email token
rhub::rc_submit()             # no args -> lists platforms to pick by number
```

```bash
R CMD build nwsrfs_r          # -> nwsrfsr_<version>.tar.gz
```

```r
rhub::rc_submit(
  "nwsrfsr_<version>.tar.gz",
  platforms = c("gcc-asan", "clang-asan", "valgrind", "nold", "ubuntu-next")
)
```

Results and the tarball become public on the `r-hub2` GitHub org; allow at least
five minutes between submissions.
