# Maintainer dependencies used only from the pixi tasks (win-builder
# submissions) and interactive development (roxygen2 man page generation).
# renv scans this file, so renv::snapshot() keeps these in renv.lock.
# Excluded from the CRAN tarball via .Rbuildignore.
if (FALSE) {
  library(devtools)
  library(roxygen2)
}
