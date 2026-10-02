# ============================================================================
# config.R - project-wide paths and logging (sourced by every script in scripts/)
# ============================================================================

# All scripts assume the working directory is the repository root (goos-polar-argo-figures/),
# i.e. run via `Rscript scripts/0X_*.R` from the root.
if (!dir.exists("data") || !dir.exists("scripts")) {
  stop(
    "config.R: working directory does not look like the project root ",
    "(expected ./data and ./scripts to exist). Run scripts from goos-polar-argo-figures/."
  )
}

DATA_DIR   <- "data"
OUT_DIR    <- "output"
RDS_DIR    <- file.path(OUT_DIR, "rds")
TABLE_DIR  <- file.path(OUT_DIR, "tables")
FIG_DIR    <- file.path(OUT_DIR, "figures")
GIS_DIR    <- file.path(OUT_DIR, "gis")
for (d in c(RDS_DIR, TABLE_DIR, FIG_DIR, GIS_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

log_msg <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))
}
