# ============================================================================
# 01_fetch_argo_index.R
#
# Fetches the Argo GDAC's global profile index (one row per profile, every Argo
# float worldwide, updated daily) and saves it, cleaned and with no geographic
# filter, as output/rds/argo_index_clean.rds - the input to every figure script.
#
# Source file is only ever READ (downloaded into data/, not modified).
# ============================================================================

source("R/config.R")
suppressPackageStartupMessages(library(data.table))

## ---- CONFIG ------------------------------------------------------------

INDEX_URL   <- "https://data-argo.ifremer.fr/ar_index_global_prof.txt.gz"
INDEX_GZ    <- file.path(DATA_DIR, "ar_index_global_prof.txt.gz")
INDEX_TXT   <- file.path(DATA_DIR, "ar_index_global_prof.txt")


## ---- 1. fetch (cache - re-download only if missing) -----------------------

if (!file.exists(INDEX_GZ)) {
  log_msg("Downloading Argo GDAC global profile index from %s ...", INDEX_URL)
  download.file(INDEX_URL, INDEX_GZ, mode = "wb", quiet = FALSE)
} else {
  log_msg("Using already-downloaded %s (delete it to re-fetch a fresh copy - updated daily upstream).", INDEX_GZ)
}
if (!file.exists(INDEX_TXT)) {
  R.utils::gunzip(INDEX_GZ, INDEX_TXT, remove = FALSE)
}

## ---- 2. read + clean --------------------------------------------------

# 8 leading '#'-commented header lines, then a CSV header row
idx <- fread(INDEX_TXT, skip = 8, na.strings = c("", "NA"))
log_msg("Loaded %d profile rows from the global index.", nrow(idx))

idx[, wmo := tstrsplit(file, "/", fixed = TRUE, keep = 2L)]
idx[, dac := tstrsplit(file, "/", fixed = TRUE, keep = 1L)]
idx[, date := as.POSIXct(as.character(date), format = "%Y%m%d%H%M%S", tz = "UTC")]

n0 <- nrow(idx)
idx <- idx[!is.na(latitude) & !is.na(longitude) & !is.na(date)]
log_msg("Dropped %d/%d rows with no valid position/date (%.2f%%).", n0 - nrow(idx), n0, 100 * (n0 - nrow(idx)) / n0)

# a few rows carry sentinel values instead of NA for a bad fix (e.g. lat -99.999,
# lon -999.999); drop them, or they break the LAEA projection downstream
n1 <- nrow(idx)
idx <- idx[latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 360]
log_msg("Dropped %d/%d rows with out-of-range lat/lon sentinel values (%.4f%%).", n1 - nrow(idx), n1, 100 * (n1 - nrow(idx)) / n1)

# the index mixes -180/180 and 0-360 longitude conventions - normalise to -180/180
idx[longitude > 180, longitude := longitude - 360]

saveRDS(idx, file.path(RDS_DIR, "argo_index_clean.rds"))
log_msg("Wrote %s (%d profiles, %d floats, all institutions, no geographic filter).",
        file.path(RDS_DIR, "argo_index_clean.rds"), nrow(idx), uniqueN(idx$wmo))

log_msg("Done.")
