#!/usr/bin/env bash
#
# rhub-submit.sh -- build the R package tarball and submit it to r-hub v2.
#
# r-hub v2 runs R CMD check on the R Consortium runners from a locally built
# tarball, so the monorepo stays intact (no need to move the R package to a
# repo root, which would fork the shared model_source symlink). This wraps the
# recipe in dev/README.md: assemble a clean package tree, R CMD build it, and
# submit the tarball with rhub::rc_submit().
#
# Usage:
#   dev/rhub-submit.sh                       # submit to the default platforms
#   dev/rhub-submit.sh valgrind gcc-asan     # submit to specific platforms
#   dev/rhub-submit.sh --list                # list available platforms
#   dev/rhub-submit.sh --build-only          # build the tarball, no submission
#   dev/rhub-submit.sh --yes [platform ...]  # skip the confirmation prompt
#   RHUB_EMAIL=you@example.org dev/rhub-submit.sh   # override the token email
#
# One-time setup (see dev/README.md):
#   install.packages("rhub"); rhub::rc_new_token()
#
# Notes:
#   * Submissions and their tarballs become PUBLIC on the r-hub2 GitHub org.
#   * Allow at least five minutes between submissions.
#   * The token email defaults to the package maintainer in DESCRIPTION;
#     set RHUB_EMAIL if your token is registered under a different address.

set -euo pipefail

# --- settings -------------------------------------------------------------
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEFAULT_PLATFORMS="gcc-asan,clang-asan,valgrind,nold,ubuntu-next"
# --------------------------------------------------------------------------

list_only=0
build_only=0
assume_yes=0
platforms=()
for arg in "$@"; do
  case "$arg" in
    -l|--list)       list_only=1 ;;
    -b|--build-only) build_only=1 ;;
    -y|--yes)        assume_yes=1 ;;
    -h|--help)       sed -n '2,26p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*)              echo "error: unknown option: $arg (try --help)" >&2; exit 2 ;;
    *)               platforms+=("$arg") ;;
  esac
done

if ! command -v Rscript >/dev/null 2>&1; then
  echo "error: Rscript not found on PATH." >&2
  exit 1
fi

if [ "$list_only" = "0" ] && ! Rscript -e 'quit(status = !requireNamespace("rhub", quietly = TRUE))'; then
  echo "error: the rhub package is not installed." >&2
  echo "       Run: Rscript -e 'install.packages(\"rhub\")'" >&2
  exit 1
fi

if [ "$list_only" = "1" ]; then
  Rscript -e 'print(rhub::rhub_platforms(), n = Inf)'
  exit 0
fi

if [ "${#platforms[@]}" -eq 0 ]; then
  PLATFORMS="$DEFAULT_PLATFORMS"
else
  PLATFORMS="$(IFS=,; echo "${platforms[*]}")"
fi

# Assemble a clean, self-contained package tree in a temp dir: turn the
# committed symlinks (src -> ../model_source, NOTICE -> ../NOTICE) into real
# copies and drop renv files, stale build artifacts, and any src/Makevars
# left behind by a local install (configure regenerates it; .Rbuildignore
# would exclude it from the tarball anyway).
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
cp -R "$REPO_ROOT/nwsrfs_r" "$WORK/nwsrfs_r"
rm -rf "$WORK/nwsrfs_r/src" && cp -R "$REPO_ROOT/model_source" "$WORK/nwsrfs_r/src"
rm -f "$WORK/nwsrfs_r/NOTICE" && cp "$REPO_ROOT/NOTICE" "$WORK/nwsrfs_r/NOTICE"
rm -rf "$WORK/nwsrfs_r/renv" "$WORK/nwsrfs_r/.Rprofile"
rm -f "$WORK/nwsrfs_r/src/Makevars"
find "$WORK/nwsrfs_r" -type f \
  \( -name '*.o' -o -name '*.so' -o -name '*.dylib' -o -name '*.a' \
  -o -name '*.mod' -o -name '*.smod' -o -name '*.obj' -o -name '*.dll' \
  -o -name '*.lib' \) -delete

echo ">>> R CMD build"
(cd "$WORK" && R CMD build nwsrfs_r > build.log 2>&1) || {
  echo "BUILD FAILED"; tail -20 "$WORK/build.log"; exit 1; }
tarball="$(ls "$WORK"/nwsrfsr_*.tar.gz)"
cp "$tarball" "$REPO_ROOT/"
TARBALL="$REPO_ROOT/$(basename "$tarball")"
echo "built $TARBALL"

if [ "$build_only" = "1" ]; then
  exit 0
fi

# Preflight: an r-hub token must exist locally.
if ! Rscript -e '
tok <- tryCatch(rhub::rc_list_local_tokens(), error = function(e) NULL)
quit(status = as.integer(is.null(tok) || nrow(tok) == 0))
'; then
  echo "error: no r-hub token found on this machine." >&2
  echo "       Run once: Rscript -e 'rhub::rc_new_token()'  (email token; see dev/README.md)" >&2
  exit 1
fi

echo ">>> Submitting $(basename "$TARBALL") to: $PLATFORMS"
if [ "$assume_yes" = "0" ]; then
  echo "The package and its check results will be PUBLIC at https://github.com/r-hub2."
  printf "Type 'yes' to continue: "
  read -r ans
  if [ "$ans" != "yes" ]; then
    echo "Aborted."
    exit 1
  fi
fi

TARBALL="$TARBALL" PLATFORMS="$PLATFORMS" RHUB_EMAIL="${RHUB_EMAIL:-}" Rscript -e '
platforms <- strsplit(Sys.getenv("PLATFORMS"), ",")[[1]]
email <- Sys.getenv("RHUB_EMAIL")
email <- if (nzchar(email)) email else NULL
rhub::rc_submit(
  path = Sys.getenv("TARBALL"),
  platforms = platforms,
  email = email,
  confirmation = TRUE
)
'
