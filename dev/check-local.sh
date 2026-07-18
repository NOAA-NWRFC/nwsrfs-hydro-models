#!/usr/bin/env bash
#
# check-local.sh -- run CRAN-style instrumented checks locally in Docker.
#
# These mirror the fast jobs in .github/workflows/cran-instrumented.yml so you
# can catch the two problem classes that only show up on CRAN's Linux builds
# BEFORE pushing, instead of waiting on CI:
#
#   lto     Link-Time Optimization. Fails on -Wlto-type-mismatch, i.e.
#           inconsistent COMMON-block layouts or mismatched call signatures
#           across Fortran translation units (the /SNUP19/, /SNCO19/ class).
#   bounds  Fortran runtime bounds checking (-fcheck=all). An out-of-bounds
#           array access (the rsnwelev-segfault class) aborts with a precise
#           file:line instead of only crashing on a specific platform.
#
# Both run in a plain rocker/r-ver container. On Apple Silicon they run under
# x86_64 emulation (--platform linux/amd64) so the toolchain matches CRAN; the
# first run pulls the image and installs testthat, so expect a few minutes.
#
# The slower gcc-ASAN and valgrind checks are deliberately NOT here: under
# emulation they take 30-60 min and many hours respectively. Leave those to CI
# (cran-instrumented.yml / valgrind.yml) or run r-hub on demand.
#
# Usage:
#   dev/check-local.sh              # run both (lto, then bounds)
#   dev/check-local.sh lto          # LTO only
#   dev/check-local.sh bounds       # bounds only
#   IMAGE=rocker/r-ver:4.4.0 dev/check-local.sh   # pin a different R version
#
# Requires: Docker (Desktop or OrbStack) running.

set -euo pipefail

# --- settings -------------------------------------------------------------
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="${IMAGE:-rocker/r-ver:latest}"
PLATFORM="${PLATFORM:-linux/amd64}"   # match CRAN (x86_64); emulated on arm64
FFLAGS_BOUNDS='-g -O2 -fcheck=all -finit-real=snan -finit-integer=-99999 -fbacktrace'
# --------------------------------------------------------------------------

checks=("$@")
if [ "${#checks[@]}" -eq 0 ]; then
  checks=(lto bounds)
fi

if ! docker info >/dev/null 2>&1; then
  echo "error: Docker is not running. Start Docker Desktop or OrbStack first." >&2
  exit 1
fi

# Assemble a clean, self-contained package tree in a temp dir: turn the
# committed symlinks (src -> ../model_source, NOTICE -> ../NOTICE) into real
# copies and drop stale build artifacts and renv files. Deliberately does NOT
# copy the sibling nwsrfs_py/, so the tarball checked below has no baseline CSVs
# -- exactly the isolation CRAN checks under (see the bounds notes below).
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
cp -R "$REPO_ROOT/nwsrfs_r" "$WORK/nwsrfs_r"
rm -rf "$WORK/nwsrfs_r/src" && cp -R "$REPO_ROOT/model_source" "$WORK/nwsrfs_r/src"
rm -f "$WORK/nwsrfs_r/NOTICE" && cp "$REPO_ROOT/NOTICE" "$WORK/nwsrfs_r/NOTICE"
rm -rf "$WORK/nwsrfs_r/renv" "$WORK/nwsrfs_r/.Rprofile"
find "$WORK/nwsrfs_r" -type f \
  \( -name '*.o' -o -name '*.so' -o -name '*.dylib' -o -name '*.a' \
  -o -name '*.mod' -o -name '*.smod' -o -name '*.obj' -o -name '*.dll' \
  -o -name '*.lib' \) -delete

# The in-container driver. Receives the check name as $1.
cat > "$WORK/run.sh" <<'CONTAINER'
set -e
check="$1"
cp -R /work/nwsrfs_r /tmp/pkg
mkdir -p ~/.R

case "$check" in
  lto)
    echo ">>> LTO check (R CMD INSTALL --use-LTO, fail on -Wlto-type-mismatch)"
    printf 'LTO_OPT = -flto\nLTO_FC_OPT = -flto\nFFLAGS = -g -O2 -flto\nFCFLAGS = -g -O2 -flto\n' > ~/.R/Makevars
    find /tmp/pkg -type f \( -name '*.o' -o -name '*.so' -o -name '*.mod' \) -delete 2>/dev/null || true
    mkdir -p /tmp/lib
    if R CMD INSTALL --use-LTO --no-docs --library=/tmp/lib /tmp/pkg > /tmp/lto.log 2>&1; then :; else
      echo "INSTALL FAILED"; tail -25 /tmp/lto.log; exit 1
    fi
    if grep -qE '\-Wlto-type-mismatch|does not match original declaration' /tmp/lto.log; then
      echo "FAIL: LTO type-mismatch warnings"
      grep -nE 'lto-type-mismatch|does not match original declaration|previously declared here|type mismatch in parameter' /tmp/lto.log
      exit 1
    fi
    echo "PASS: no LTO type-mismatch warnings"
    ;;

  bounds)
    echo ">>> Fortran bounds check (R CMD check on tarball, -fcheck=all)"
    # testthat as a binary from Posit PM for this container's distro codename.
    CODENAME="$(. /etc/os-release; echo "${VERSION_CODENAME:-}")"
    Rscript -e "options(repos=c(CRAN=paste0('https://packagemanager.posit.co/cran/__linux__/','${CODENAME}','/latest'))); if(!requireNamespace('testthat',quietly=TRUE)) install.packages('testthat')" > /tmp/dep.log 2>&1 || true
    if ! Rscript -e 'quit(status = !requireNamespace("testthat", quietly=TRUE))'; then
      echo "could not install testthat in container"; tail -15 /tmp/dep.log; exit 1
    fi
    printf 'FFLAGS = %s\nFCFLAGS = %s\n' "$FFLAGS_BOUNDS" "$FFLAGS_BOUNDS" > ~/.R/Makevars
    find /tmp/pkg -type f \( -name '*.o' -o -name '*.so' -o -name '*.mod' \) -delete 2>/dev/null || true
    cd /tmp
    if R CMD build /tmp/pkg > /tmp/build.log 2>&1; then :; else
      echo "BUILD FAILED"; tail -20 /tmp/build.log; exit 1
    fi
    pkg="$(ls nwsrfsr_*.tar.gz)"
    # Isolated dir (no nwsrfs_py sibling) + NOT_CRAN=false so the four Python
    # baseline-comparison tests skip exactly as they do on CRAN. They compare
    # against CSVs generated on macOS/arm64 and cannot match on Linux/x86 (a
    # libm floating-point difference, not a memory fault). The examples --
    # including the rsnwelev one that segfaulted on CRAN -- still run under
    # -fcheck=all, and a runtime bounds error there aborts the check.
    mkdir -p /tmp/check && cp "$pkg" /tmp/check/ && cd /tmp/check
    export NOT_CRAN=false
    if R CMD check --no-manual "$pkg" > /tmp/check.log 2>&1; then :; else
      echo "CHECK returned non-zero"; :
    fi
    grep -E 'Status:' /tmp/check.log | tail -1 || true
    grep -hE 'FAIL [0-9]+ \| WARN|SKIP [0-9]+' /tmp/check/nwsrfsr.Rcheck/tests/*.Rout* 2>/dev/null | tail -2 || true
    # Scan only run transcripts, not the whole .Rcheck tree: 00_pkg_src/ holds
    # the package sources, and a source comment can legitimately contain "above
    # upper bound". A real gfortran abort always prints "Fortran runtime error:".
    hit=0
    for f in $(find /tmp/check/nwsrfsr.Rcheck \( -name '*.Rout' -o -name '*.Rout.fail' \) 2>/dev/null); do
      if grep -qE 'Fortran runtime error:' "$f"; then
        echo "FAIL: Fortran runtime error in $(basename "$f")"
        grep -nE 'Fortran runtime error:|Index .* of dimension .* (above upper|below lower) bound' "$f" | head
        hit=1
      fi
    done
    if [ "$hit" = "1" ]; then exit 1; fi
    if grep -qE '^Status: OK' /tmp/check.log; then
      echo "PASS: Status OK, no bounds errors"
    else
      echo "FAIL: R CMD check did not report Status: OK"
      tail -30 /tmp/check.log
      exit 1
    fi
    ;;

  *)
    echo "unknown check: $check (expected 'lto' or 'bounds')"; exit 2 ;;
esac
CONTAINER

# FFLAGS_BOUNDS must be visible inside the container script.
export_flags=(-e "FFLAGS_BOUNDS=${FFLAGS_BOUNDS}")

rc=0
for check in "${checks[@]}"; do
  echo "=============================================================="
  echo "  $check  (image=$IMAGE platform=$PLATFORM)"
  echo "=============================================================="
  if docker run --rm --platform "$PLATFORM" "${export_flags[@]}" \
       -v "$WORK":/work:ro "$IMAGE" bash /work/run.sh "$check"; then
    echo "  --> $check PASSED"
  else
    echo "  --> $check FAILED"
    rc=1
  fi
done

exit "$rc"
