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
`check-local.sh` checks the tarball in isolation with `NOT_CRAN=false`, so the
tests skip exactly as they do on CRAN: the sibling `nwsrfs_py/` directory is
not present and `skip_on_cran()` applies. Since 1.0.3 the comparisons
themselves hold on every platform (see the next section); run them from the
monorepo with `pixi run test-r`.

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
