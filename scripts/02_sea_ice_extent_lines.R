# ============================================================================
# 02_sea_ice_extent_lines.R
#
# Median sea ice extent lines for the GOOS polar maps (scripts/03-04), to
# show where floats were likely under ice. Both lines are built; only the winter-max
# line is drawn on the maps. Years 2021-2025 in both hemispheres (complete seasons):
#   - "max": median of each year's WINTER-MAXIMUM extent (Sept south / March north)
#   - "min": median of each year's SUMMER-MINIMUM extent (Feb south / Sept north)
# Extent = the standard 15% concentration edge. For each year the day of max/min
# extent is found within a seasonal window; that day's ice mask (conc >= 15%) is
# kept; the median line is the 0.5 contour of the fraction of years with ice
# (i.e. ice in at least half the years), contoured on a 2x bilinear-refined grid so
# the edge isn't a 25 km staircase at map scale. A year is used only if the data
# fully covers its window - so years still in progress (e.g. Sept 2026) are left out,
# and the years actually used are stored with each line.
# Land/missing cells are NA in the source, so contours stop at coastlines rather
# than tracing them - only true ice/open-water edges are drawn.
#
# Source: NOAA/NSIDC CDR of Passive Microwave Sea Ice Concentration v6 (G02202_V6),
# daily 25 km, one file per year per hemisphere, downloaded once (only if missing)
# into output/gis/seaice_daily_raw_{south,north}/.
#
# Outputs: output/gis/seaice_extent_lines_{south,north}.rds (sf lines in each map's
# own CRS, columns kind = max/min, years = years used)
# ============================================================================

source("R/config.R")
source("R/utils.R")
suppressPackageStartupMessages({
  library(terra)
  library(sf)
})

YEARS <- 2021:2025  # complete seasons in both hemispheres (Sept 2026 not yet available)
EXTENT_THRESH <- 0.15
HEMIS <- list(
  south = list(url = "https://noaadata.apps.nsidc.org/NOAA/G02202_V6/south/aggregate", prefix = "sic_pss25",
               raw_dir = file.path(GIS_DIR, "seaice_daily_raw_south"),
               crs_out = "+proj=laea +lat_0=-90 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
               windows = list(max = c("08-01", "10-31"), min = c("01-15", "03-31"))),
  north = list(url = "https://noaadata.apps.nsidc.org/NOAA/G02202_V6/north/aggregate", prefix = "sic_psn25",
               raw_dir = file.path(GIS_DIR, "seaice_daily_raw_north"),
               crs_out = "+proj=laea +lat_0=90 +lon_0=-45 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
               windows = list(max = c("02-01", "04-15"), min = c("08-15", "10-15")))
)

fetch_year <- function(h, yr) {
  dir.create(h$raw_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(h$raw_dir, sprintf("%d.nc", yr))
  if (file.exists(dest)) return(dest)
  if (yr < as.integer(format(Sys.Date(), "%Y"))) {
    fname <- sprintf("%s_%d0101-%d1231_v06r00.nc", h$prefix, yr, yr)
  } else {
    # in-progress year: filename end-date changes as days are added - look it up
    listing <- paste(readLines(paste0(h$url, "/"), warn = FALSE), collapse = "\n")
    m <- regmatches(listing, regexpr(sprintf("%s_%d[0-9]*-[0-9]*_v06r00\\.nc", h$prefix, yr), listing))
    if (!length(m)) { log_msg("  no NSIDC file for %d at %s - skipping.", yr, h$url); return(NA_character_) }
    fname <- m[1]
  }
  log_msg("  downloading %s ...", fname)
  options(timeout = max(900, getOption("timeout")))
  download.file(sprintf("%s/%s", h$url, fname), dest, quiet = TRUE, mode = "wb")
  dest
}

for (hn in names(HEMIS)) {
  h <- HEMIS[[hn]]
  log_msg("== %s hemisphere ==", hn)
  files <- vapply(YEARS, function(y) fetch_year(h, y), character(1))
  out <- list()
  for (kind in c("max", "min")) {
    masks <- list(); used <- integer(0)
    for (i in seq_along(YEARS)) {
      if (is.na(files[i])) next
      yr <- YEARS[i]
      r <- rast(files[i], subds = "cdr_seaice_conc")
      d <- as.Date(time(r))
      w <- as.Date(sprintf("%d-%s", yr, h$windows[[kind]]))
      if (min(d) > w[1] || max(d) < w[2]) {
        log_msg("  %s %d: data (%s to %s) doesn't cover the %s window - year not used.", kind, yr, min(d), max(d), kind)
        next
      }
      rw <- r[[which(d >= w[1] & d <= w[2])]]
      ext_cells <- global(rw >= EXTENT_THRESH, "sum", na.rm = TRUE)[, 1]
      k <- if (kind == "max") which.max(ext_cells) else which.min(ext_cells)
      masks[[length(masks) + 1]] <- rw[[k]] >= EXTENT_THRESH
      used <- c(used, yr)
      log_msg("  %s %d: %s (%d ice cells).", kind, yr, as.Date(time(rw))[k], ext_cells[k])
    }
    frac <- mean(rast(masks))                        # fraction of years with ice (NA on land)
    frac <- disagg(frac, fact = 2, method = "bilinear")
    cl <- st_as_sf(as.contour(frac, levels = 0.5))
    cl <- st_transform(st_cast(cl, "MULTILINESTRING"), h$crs_out)
    cl$kind <- kind
    cl$years <- sprintf("%d-%d", min(used), max(used))
    cl$n_years <- length(used)
    out[[kind]] <- cl[, c("kind", "years", "n_years")]
    log_msg("  median %s line from %d years (%s).", kind, length(used), paste(used, collapse = ", "))
  }
  res <- do.call(rbind, out)
  out_path <- file.path(GIS_DIR, sprintf("seaice_extent_lines_%s.rds", hn))
  saveRDS(res, out_path)
  log_msg("Wrote %s", out_path)
}
log_msg("Done.")
