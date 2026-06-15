source("renv/activate.R")

# INLA ships from its own repository, not CRAN. Append it to the repos so
# renv (snapshot/restore) and install.packages() can discover and resolve it.
# Without this, `renv::snapshot()` fails with "package 'INLA' is not available".
options(repos = c(
  getOption("repos"),
  INLA = "https://inla.r-inla-download.org/R/stable"
))
