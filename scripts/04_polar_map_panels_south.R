# ============================================================================
# 04_polar_map_panels_south.R
#
# GOOS polar Argo map panels - ANTARCTIC region. Companion script:
# 03_polar_map_panels_north.R (identical design; only geography/labels differ).
# Derived from the IMOS Argo region map (imos-argo-animations: basemap, palette, tracks)
# but pole-centred LAEA, 45 deg parallel inscribed in the frame (same scale N and S),
# GOOS logo, Roboto fonts (bundled in fonts/), no titles.
#
# Two classification options, ALL countries' floats, deployed 2001+:
#   A) Recent deployments: orange = floats deployed LIVE_START_YEAR (2021) onwards;
#      blue = deployed earlier.
#   B) Active fleet (final option): orange = the last RECENT_YEARS (5) of ACTIVE floats'
#      tracks; blue = inactive floats + active floats' older tracks.
#      Active = a profile within ACTIVE_WINDOW_DAYS (90 d) of the index snapshot, or
#      within UNDER_ICE_WINDOW_DAYS (13 months) if last seen poleward of 60 deg (floats
#      under ice can't surface for months; 13 months keeps the Aug 2025 Beaufort Gyre
#      deployments active). Deployment date = first profile in the GLOBAL index.
# Overlays: dashed 60 deg parallel (Argo's "polar"), median winter-max sea ice extent
# 2021-2025 (from scripts/02).
#
# Writes, per option (A, B):
#   output/figures/panels/GOOS_argo_antarctic_option_<opt>_panel.png  standalone
#       panel (logo + own legend) for editorial use
#   output/figures/panels/composite/GOOS_argo_antarctic_option_<opt>_base.png (+ _legend.csv)
#       inputs for scripts/05_polar_composites.R (Antarctic panel keeps the logo,
#       Arctic panel carries the one shared legend there)
# Run order: 01 -> 02 (only if sea ice data changes) -> 03 -> 04 -> 05; 06 is independent.
# ============================================================================

# Roboto (bundled in fonts/, downloaded from Google Fonts via Fontsource, OFL
# licence). magick/ImageMagick only finds fonts through fontconfig, and ignores
# direct .ttf paths here, so point fontconfig at fonts/ (plus the system font
# dirs) BEFORE magick is loaded. ggplot text goes through ragg + systemfonts instead.
FONT_DIR <- normalizePath("fonts")
local({
  cf <- file.path(tempdir(), "goos_fonts.conf")
  writeLines(c('<?xml version="1.0"?>', '<!DOCTYPE fontconfig SYSTEM "fonts.dtd">', "<fontconfig>",
               sprintf("<dir>%s</dir>", FONT_DIR), "<dir>/System/Library/Fonts</dir>", "<dir>/Library/Fonts</dir>",
               sprintf("<cachedir>%s</cachedir>", file.path(tempdir(), "fccache")), "</fontconfig>"), cf)
  Sys.setenv(FONTCONFIG_FILE = cf)
})
FONT_LABEL   <- "Roboto Condensed"  # regular

source("R/config.R")
source("R/utils.R")
source("R/theme.R")
suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(terra)
  library(ggplot2)
  library(magick)
  library(maptiles)
})
systemfonts::register_font("Roboto Condensed GOOS",
                           plain = file.path(FONT_DIR, "RobotoCondensed-Regular.ttf"),
                           bold  = file.path(FONT_DIR, "RobotoCondensed-Bold.ttf"),
                           italic = file.path(FONT_DIR, "RobotoCondensed-Italic.ttf"))
stopifnot(any(magick_fonts()$family == FONT_LABEL))

## ---- CONFIG ------------------------------------------------------------

BATHY_PATH <- file.path(DATA_DIR, "bathymetry", "ETOPO1_Ice_bathymetry_geotiff.tif")
LOGO_PATH  <- "GOOS_Main_logo_-_white.png"

LIVE_START_YEAR <- 2021L  # Option A: 'recent' = deployed 2021 onwards
HISTORICAL_DEPLOY_START <- 2001L  # floats deployed before this are dropped entirely
# Option B "active" rule (same in both hemispheres): reported within ACTIVE_WINDOW_DAYS
# of the snapshot, or within UNDER_ICE_WINDOW_DAYS if last seen poleward of
# UNDER_ICE_LAT. 13 months is the shortest round window that keeps all recent Beaufort
# Gyre floats active (silent up to 372 d at the 2026-07-30 snapshot).
ACTIVE_WINDOW_DAYS     <- 90
UNDER_ICE_LAT          <- 60
UNDER_ICE_WINDOW_DAYS  <- 396     # 13 months x 30.44 d
# Option B: only the last RECENT_YEARS of active floats' tracks are orange; older
# segments are drawn blue with the inactive floats
RECENT_YEARS <- 5
LAND_LABEL_COL  <- "#8C8C8C"  # landmass labels: solid light grey
LAND_LABEL_SIZE <- 4.3
TRACK_COLOURS <- c("Historical" = "#5E89BC", "Live" = "#F0A500")
DRAW_ORDER <- c("Historical", "Live")  # Historical (far more numerous) drawn underneath

TILE_PROVIDER  <- "CartoDB.DarkMatterNoLabels"
SAT_ZOOM       <- 5
TILE_CACHE_DIR <- file.path(GIS_DIR, "cartodb_darkmatter_tile_cache")
MERCATOR_LAT_LIMIT <- 85.0511

POLE_GAP_FILL <- rgb(9, 9, 9, maxColorValue = 255)

SEGMENT_LINEWIDTH <- 0.25
# low alpha for the (dense) historical tracks so the live array stands out on top
SEGMENT_ALPHA_VALUES <- c("Historical" = 0.15, "Live" = 0.55)
BASEMAP_GRID_N <- 1400
MAP_BUFFER_DEG <- 4

ANIM_LON_0 <- 150
ANIM_POLAR_CRS <- sprintf("+proj=laea +lat_0=-90 +lon_0=%d +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", ANIM_LON_0)

## ---- panel size --------------------------------------------------------------
# 1008 x 1018 px = the map area of the IMOS region map's 1080x1350 canvas (minus margins, title
# and footer), so the panels match the IMOS deliverables' map size.
MAP_PX_W <- 1008L
MAP_PX_H <- 1018L

measure_text_img <- function(text, size, color, bg = "black", style = "normal") {
  img <- image_blank(width = 6000, height = size * 4, color = bg)
  img <- image_annotate(img, text, font = FONT_LABEL, size = size, color = color, style = style, gravity = "northwest", location = "+0+0")
  image_trim(img, fuzz = 1)
}

## ---- 1. determine the map's own rendered viewport FIRST ---------------------
# (needed before loading position data - see below)

xy_to_lonlat <- function(x, y, crs) {
  m <- sf::sf_project(from = crs, to = "OGC:CRS84", pts = cbind(x, y))
  colnames(m) <- c("lon", "lat")
  as.data.frame(m)
}

PORTRAIT_ASPECT <- MAP_PX_W / MAP_PX_H
# pole-centred view, frame just wide enough to inscribe the VIEW_EDGE_LAT parallel
# (+2% margin); identical scale in the North/South scripts since the same parallel is
# used. Corners still reach lower latitudes.
VIEW_EDGE_LAT <- 45
edge_xy <- project_xy(seq(-180, 180, by = 1), rep(-1 * VIEW_EDGE_LAT, 361), crs = ANIM_POLAR_CRS)
half_w <- max(sqrt(edge_xy$x^2 + edge_xy$y^2)) * 1.02
half_h <- half_w / PORTRAIT_ASPECT
xlim <- c(-half_w, half_w)
ylim <- c(-half_h, half_h)

# Argo's polar boundary, drawn as a faint dashed circle with a small label
POLAR_LAT <- 60
polar_circle <- project_xy(seq(-180, 180, by = 0.5), rep(-1 * POLAR_LAT, 721), crs = ANIM_POLAR_CRS)

FRAME_PX_W <- MAP_PX_W
FRAME_PX_H <- MAP_PX_H
log_msg("Panel size: %d x %d px.", FRAME_PX_W, FRAME_PX_H)

## ---- 2. load ALL Argo profiles, project, keep only what falls inside the ---
## map's rendered viewport (xlim/ylim above) ---------------------------------

idx <- readRDS(file.path(RDS_DIR, "argo_index_clean.rds"))
setDT(idx)
log_msg("Loaded %d profiles across %d floats globally (no geographic filter yet).", nrow(idx), uniqueN(idx$wmo))

# deployment date (first profile) and last transmission (date + latitude) from the
# GLOBAL index, BEFORE the year cap and viewport filter, so floats deployed or last seen
# outside the map are still classified correctly
snapshot_date <- max(idx$date)
float_dt <- idx[order(date), .(deploy_date = date[1], last_date = date[.N], last_lat = latitude[.N]), by = wmo]
float_dt[, days_silent := as.numeric(difftime(snapshot_date, last_date, units = "days"))]
float_dt[, active := days_silent <= ACTIVE_WINDOW_DAYS |
                     (abs(last_lat) > UNDER_ICE_LAT & days_silent <= UNDER_ICE_WINDOW_DAYS)]
RECENT_CUTOFF <- as.POSIXct(seq(as.Date(snapshot_date), by = sprintf("-%d years", RECENT_YEARS), length.out = 2)[2], tz = "UTC")
log_msg("Index snapshot %s: %d floats active globally (%d via the under-ice allowance).",
        format(snapshot_date, "%Y-%m-%d"), sum(float_dt$active), sum(float_dt$active & float_dt$days_silent > ACTIVE_WINDOW_DAYS))

# profiles from 2001-2026 only
YEAR_MIN <- as.POSIXct("2001-01-01", tz = "UTC")
YEAR_MAX <- as.POSIXct("2027-01-01", tz = "UTC")
n_before_year_cap <- nrow(idx)
idx <- idx[date >= YEAR_MIN & date < YEAR_MAX]
log_msg("Capped to 2001-2026: dropped %d/%d profiles outside that range.", n_before_year_cap - nrow(idx), n_before_year_cap)

xy_all <- project_xy(idx$longitude, idx$latitude, crs = ANIM_POLAR_CRS)
idx[, `:=`(x = xy_all$x, y = xy_all$y)]
in_view <- idx$x >= xlim[1] & idx$x <= xlim[2] & idx$y >= ylim[1] & idx$y <= ylim[2]
reg <- idx[in_view]
setDT(reg)
reg <- merge(reg, float_dt, by = "wmo", all.x = TRUE)
n_pre <- uniqueN(reg[as.integer(format(deploy_date, "%Y")) < HISTORICAL_DEPLOY_START]$wmo)
reg <- reg[as.integer(format(deploy_date, "%Y")) >= HISTORICAL_DEPLOY_START]
log_msg("Dropped %d floats deployed before %d.", n_pre, HISTORICAL_DEPLOY_START)
setorder(reg, wmo, date)
reg[, track_id := wmo]
log_msg("%d/%d profiles (%d floats) fall inside the map's rendered viewport.",
        nrow(reg), nrow(idx), uniqueN(reg$wmo))
rm(idx, xy_all, in_view)

reg[, `:=`(xend = data.table::shift(x, -1L), yend = data.table::shift(y, -1L),
           lon_end = data.table::shift(longitude, -1L), lat_end = data.table::shift(latitude, -1L),
           dt_days = as.numeric(difftime(data.table::shift(date, -1L), date, units = "days"))), by = track_id]
segs <- reg[!is.na(xend)]
# Argo cycle is ~10 days (median); a gap > 30 days is a float going quiet or -
# mostly, since this dataset is viewport-filtered - drifting out of frame and back,
# which would otherwise draw a spurious straight "teleport" line between the visits.
n_gap <- sum(segs$dt_days > 30)
segs <- segs[dt_days <= 30]
# drop physically implausible jumps between consecutive profiles (bad positions in the
# index): 99.99% of <=30-day steps are < ~560 km, but a few hundred jump 500-5,300 km
# and would draw straight lines across continents. Great-circle distance from lon/lat.
MAX_STEP_KM <- 600
segs[, step_km := 6371 * 2 * asin(sqrt(sin((lat_end - latitude) * pi / 360)^2 +
                                   cos(latitude * pi / 180) * cos(lat_end * pi / 180) * sin((lon_end - longitude) * pi / 360)^2))]
n_jump <- sum(segs$step_km > MAX_STEP_KM, na.rm = TRUE)
segs <- segs[is.na(step_km) | step_km <= MAX_STEP_KM]
log_msg("Dropped %d segments with implausible >%d km jumps between consecutive profiles.", n_jump, MAX_STEP_KM)
log_msg("Built %d track segments for %d floats (dropped %d spurious segments spanning >30-day gaps).",
        nrow(segs), uniqueN(segs$track_id), n_gap)

# polar-region counts for Option B (latest position poleward of 60 deg; same active rule
# as the map). Logged only - legends carry no counts.
POLAR_SIGN <- -1
polar_fl <- float_dt[POLAR_SIGN * last_lat > UNDER_ICE_LAT & as.integer(format(deploy_date, "%Y")) >= HISTORICAL_DEPLOY_START]
POLAR_COUNTS <- c(Live = polar_fl[active == TRUE, .N], Historical = polar_fl[active == FALSE, .N])
log_msg("Polar (beyond 60 deg) counts for the Option B legend: active %d, inactive %d.", POLAR_COUNTS[["Live"]], POLAR_COUNTS[["Historical"]])

VERSIONS <- list(
  A = list(class_live = quote(as.integer(format(deploy_date, "%Y")) >= LIVE_START_YEAR),
           # "{counts}" placeholder: removed when the legend is drawn (no counts shown)
           labels = c("Live" = sprintf("Floats deployed %d-2026 {counts}", LIVE_START_YEAR),
                      "Historical" = sprintf("Floats deployed %d-%d {counts}", HISTORICAL_DEPLOY_START, LIVE_START_YEAR - 1L))),
  # B: orange = the last RECENT_YEARS of ACTIVE floats' tracks only; blue = inactive
  # floats + active floats' older segments.
  B = list(class_live = quote(active & date >= RECENT_CUTOFF),
           hist_floats = quote(active == FALSE),  # not quote(!active): data.table reads DT[!col] as a not-join
           counts = POLAR_COUNTS,                 # polar-region counts (logged)
           labels = c("Live" = sprintf("Active floats: %s-%s {counts}", format(RECENT_CUTOFF, "%Y"), format(snapshot_date, "%Y")),
                      "Historical" = sprintf("Historical: inactive {counts} & active floats pre-%s", format(RECENT_CUTOFF, "%Y"))))
)

view_boundary <- rbind(
  cbind(seq(xlim[1], xlim[2], length.out = 200), ylim[1]),
  cbind(seq(xlim[1], xlim[2], length.out = 200), ylim[2]),
  cbind(xlim[1], seq(ylim[1], ylim[2], length.out = 200)),
  cbind(xlim[2], seq(ylim[1], ylim[2], length.out = 200))
)
view_ll <- xy_to_lonlat(view_boundary[, 1], view_boundary[, 2], ANIM_POLAR_CRS)
bathy_lat_range <- range(view_ll$lat)
if (xlim[1] <= 0 && 0 <= xlim[2] && ylim[1] <= 0 && 0 <= ylim[2]) bathy_lat_range[1] <- -90
log_msg("Viewport reaches lat %.1f to %.1f - using this for the bathymetry crop.",
        bathy_lat_range[1], bathy_lat_range[2])

## ---- 3. bake basemap (ETOPO1 land/ocean mask + cached CartoDB Dark Matter tiles) --

log_msg("Building the basemap (ETOPO1 ocean/land mask + CartoDB Dark Matter imagery)...")
bathy <- rast(BATHY_PATH)
bb <- ext(-180, 180, bathy_lat_range[1] - MAP_BUFFER_DEG, bathy_lat_range[2] + MAP_BUFFER_DEG)
bathy_crop <- crop(bathy, bb)
bathy_polar <- project(bathy_crop, ANIM_POLAR_CRS, method = "bilinear")
fact <- max(1, round(max(nrow(bathy_polar), ncol(bathy_polar)) / BASEMAP_GRID_N))
bathy_agg <- aggregate(bathy_polar, fact = fact, fun = "mean", na.rm = TRUE)

tile_ymin <- max(bathy_lat_range[1] - MAP_BUFFER_DEG, -MERCATOR_LAT_LIMIT)
tile_ymax <- min(bathy_lat_range[2] + MAP_BUFFER_DEG, MERCATOR_LAT_LIMIT)
tile_bbox_sf <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = -180, xmax = 180, ymin = tile_ymin, ymax = tile_ymax), crs = 4326
))
log_msg("Fetching %s tiles (zoom %d) for lat %.1f to %.1f (cached)...", TILE_PROVIDER, SAT_ZOOM, tile_ymin, tile_ymax)
dir.create(TILE_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
sat <- get_tiles(tile_bbox_sf, provider = TILE_PROVIDER, zoom = SAT_ZOOM, crop = TRUE,
                  project = FALSE, cachedir = TILE_CACHE_DIR, verbose = TRUE)
sat_agg <- project(sat, bathy_agg, method = "bilinear")

clamp255 <- function(v) pmin(pmax(v, 0), 255)
r_v <- clamp255(values(sat_agg[[1]])[, 1]); g_v <- clamp255(values(sat_agg[[2]])[, 1]); b_v <- clamp255(values(sat_agg[[3]])[, 1])
gap <- is.na(r_v) | is.na(g_v) | is.na(b_v)
base_col <- rep(POLE_GAP_FILL, length(r_v))
base_col[!gap] <- rgb(r_v[!gap], g_v[!gap], b_v[!gap], maxColorValue = 255)
log_msg("%.1f%% of the basemap grid has no imagery (polar gap beyond %.2f deg S) - filled with a solid dark colour.",
        100 * mean(gap), MERCATOR_LAT_LIMIT)

coast <- sf::st_make_valid(get_southern_coastline(crs = ANIM_POLAR_CRS, cache = file.path(GIS_DIR, "coastline_polar.rds")))
coast <- coast[coast$name != "Fiji", ]

strip_holes <- function(geom) {
  sf::st_sfc(lapply(geom, function(g) {
    if (inherits(g, "MULTIPOLYGON")) sf::st_multipolygon(lapply(g, function(poly) list(poly[[1]])))
    else if (inherits(g, "POLYGON")) sf::st_polygon(list(g[[1]]))
    else g
  }), crs = sf::st_crs(geom))
}
sf::st_geometry(coast) <- strip_holes(sf::st_geometry(coast))

# Ocean cells the tiles render dark are repainted with the median ocean colour. Ross
# Sea special case: parts of the Ross Ice Shelf sit at only +3 to +7 m in ETOPO1-Ice
# (elev >= 0, so read as land) while the Natural Earth coastline puts them in open
# water. Cells below LOW_LYING_THRESH_M that fall outside the coastline polygon are
# therefore also treated as ocean; real Antarctic terrain is far higher, so only this
# near-sea-level ice-shelf margin is affected.
elev_v <- values(bathy_agg)[, 1]
coast_mask <- terra::rasterize(terra::vect(coast), bathy_agg, field = 1, background = 0)
inside_coast <- values(coast_mask)[, 1] == 1
LOW_LYING_THRESH_M <- 20
ambiguous_low_lying <- !is.na(elev_v) & elev_v >= 0 & elev_v < LOW_LYING_THRESH_M & !inside_coast
is_true_ocean <- (!is.na(elev_v) & elev_v < 0) | ambiguous_low_lying
brightness <- (r_v + g_v + b_v) / 3
ocean_ref <- is_true_ocean & !gap & brightness >= 37
ocean_ref_col <- rgb(median(r_v[ocean_ref]), median(g_v[ocean_ref]), median(b_v[ocean_ref]), maxColorValue = 255)
tile_artifact <- is_true_ocean & !gap & brightness < 37
base_col[tile_artifact] <- ocean_ref_col
log_msg("Corrected %d/%d cells (%.3f%%) where ETOPO1/coastline data says ocean but the tile rendered it dark (%d via the low-lying/coastline-mismatch path; reference ocean colour: %s).",
        sum(tile_artifact), length(base_col), 100 * sum(tile_artifact) / length(base_col),
        sum(ambiguous_low_lying & tile_artifact), ocean_ref_col)

basemap_dt <- as.data.table(as.data.frame(bathy_agg, xy = TRUE, na.rm = FALSE))[, 1:2]
setnames(basemap_dt, c("x", "y"))
basemap_dt[, col := base_col]
log_msg("Basemap ready (%d cells).", nrow(basemap_dt))

# Antarctica: drawn with the basemap, under the tracks
continent_labels <- data.frame(label = "Antarctica", vjust = 0.5, hjust = 0.5, angle = 0, size = LAND_LABEL_SIZE, x = -350000, y = 0)
px_to_xy <- function(px, py) c(x = xlim[1] + px / MAP_PX_W * diff(xlim), y = ylim[2] - py / MAP_PX_H * diff(ylim))

# Africa, S. America, New Zealand: drawn ON TOP of the tracks, placed by panel pixel
# position (only their polar-facing edges are in view). Africa's thin visible strip:
# label starts at the tip (left-aligned) and runs over the tracks. S. America: the
# standalone panels' legend covers its land, so there it sits just above the legend;
# in the composite (no legend on this panel) it is centred on the land.
top_land_labels <- function(sa_px) {
  d <- data.frame(label = c("Africa", "South\nAmerica", "New\nZealand"), hjust = c(0, 0.5, 0.5))
  cbind(d, rbind(px_to_xy(6, 905), px_to_xy(sa_px[1], sa_px[2]), px_to_xy(702, 28)))
}
top_land_layers <- function(sa_px) {
  geom_text(data = top_land_labels(sa_px), aes(x = x, y = y, label = label, hjust = hjust), colour = LAND_LABEL_COL,
            size = LAND_LABEL_SIZE, lineheight = 0.85, family = "Roboto Condensed GOOS", inherit.aes = FALSE)
}
SA_PX_STANDALONE <- c(790, 818)  # just above the standalone legend
SA_PX_COMPOSITE  <- c(850, 925)  # centred on the visible landmass

# ocean sector names (italic, cartographic convention for water) at 50S, clear of the
# sea ice line; near-opaque with a thin dark halo since they sit over dense tracks
ocean_labels <- data.frame(
  label = c("Pacific\nsector", "Atlantic\nsector", "Indian\nsector"),
  lon   = c(-140, -20, 80),
  lat   = c(-50, -50, -50)
)
ocean_labels <- cbind(ocean_labels, project_xy(ocean_labels$lon, ocean_labels$lat, crs = ANIM_POLAR_CRS))
polar_circle_label <- cbind(data.frame(label = "60\u00b0S"), project_xy(115, -57.8, crs = ANIM_POLAR_CRS))  # just outside the circle
OCEAN_LABEL_SIZE <- 5
OCEAN_HALO_OFFSET <- diff(xlim) * 0.0012
ocean_halo <- do.call(rbind, lapply(seq(0, 2 * pi, length.out = 9)[-9], function(a)
  transform(ocean_labels, x = x + OCEAN_HALO_OFFSET * cos(a), y = y + OCEAN_HALO_OFFSET * sin(a))))

LOGO_STD_W <- 216L

logo_grob <- NULL
if (!is.null(LOGO_PATH) && file.exists(LOGO_PATH)) {
  # the GOOS PNG has wide transparent padding (842x596 canvas, logo in the middle
  # band) - trim it so the logo fills LOGO_STD_W and sits flush with the corner pad
  logo_img <- magick::image_trim(magick::image_read(LOGO_PATH))
  logo_img_info <- magick::image_info(logo_img)
  logo_asp <- logo_img_info$height / logo_img_info$width
  logo_w <- diff(xlim) * (LOGO_STD_W / MAP_PX_W)
  logo_h <- logo_w * logo_asp
  logo_pad <- diff(xlim) * 0.02
  logo_xmin <- xlim[1] + logo_pad; logo_xmax <- logo_xmin + logo_w
  logo_ymax <- ylim[2] - logo_pad; logo_ymin <- logo_ymax - logo_h
  logo_resized <- magick::image_resize(logo_img, sprintf("%dx", round(LOGO_STD_W * 2)))
  logo_grob <- grid::rasterGrob(as.raster(logo_resized), interpolate = TRUE)
  log_msg("Logo loaded from %s.", LOGO_PATH)
} else if (!is.null(LOGO_PATH)) {
  log_msg("LOGO_PATH is set (%s) but the file doesn't exist - skipping the logo overlay.", LOGO_PATH)
}

## ---- legend (adapted from seal_connectivity/scripts/13's build_species_legend) --
# median sea ice extent line (built by scripts/02_sea_ice_extent_lines.R), in GOOS
# brand green #189669 lightened in OKLCh (same hue, L 0.60 -> 0.82) so it reads on the
# dark basemap and stays lighter than both track colours
SEAICE_COL <- "#70DDAC"
SEAICE_LWD <- 0.6
seaice_lines <- readRDS(file.path(GIS_DIR, "seaice_extent_lines_south.rds"))
seaice_lines <- seaice_lines[seaice_lines$kind == "max", ]  # winter-max line only (summer-min not drawn)
seaice_years <- setNames(seaice_lines$years, seaice_lines$kind)
SEAICE_LEGEND <- c(max = sprintf("Median winter max sea ice extent (%s)", seaice_years[["max"]]))

LEGEND_SWATCH_PX <- 22L
LEGEND_ROW_GAP_PX <- 10L
LEGEND_TEXT_SIZE  <- 20L
LEGEND_PAD_PX     <- 16L
LEGEND_BG_COLOR   <- "black"

# line swatch for legend rows: a horizontal solid or dotted stroke across the swatch box
line_swatch <- function(col, style, w = LEGEND_SWATCH_PX, stroke = 3L) {
  sw <- image_blank(w, w, LEGEND_BG_COLOR)
  y0 <- (w - stroke) %/% 2
  if (style == "solid") {
    sw <- image_composite(sw, image_blank(w, stroke, col), offset = sprintf("+0+%d", y0))
  } else {  # dotted
    for (x0 in seq(0, w - stroke, by = 2L * stroke)) sw <- image_composite(sw, image_blank(stroke, stroke, col), offset = sprintf("+%d+%d", x0, y0))
  }
  sw
}
build_two_way_legend <- function(labels, colours, display_labels = labels, swatch_styles = NULL) {
  text_rows <- lapply(labels, function(lb) {
    img <- measure_text_img(display_labels[[lb]], LEGEND_TEXT_SIZE, "white", bg = LEGEND_BG_COLOR)
    list(img = img, i = image_info(img))
  })
  row_h <- vapply(text_rows, function(r) max(r$i$height, LEGEND_SWATCH_PX), numeric(1))
  row_w <- vapply(text_rows, function(r) LEGEND_SWATCH_PX + 10L + r$i$width, numeric(1))
  content_w <- max(row_w)
  content_h <- sum(row_h) + LEGEND_ROW_GAP_PX * (length(text_rows) - 1)
  panel_w <- content_w + 2L * LEGEND_PAD_PX
  panel_h <- content_h + 2L * LEGEND_PAD_PX
  panel <- image_blank(width = panel_w, height = panel_h, color = LEGEND_BG_COLOR)

  y <- LEGEND_PAD_PX
  for (i in seq_along(labels)) {
    r <- text_rows[[i]]; rh <- row_h[i]
    st <- if (is.null(swatch_styles)) "fill" else swatch_styles[[labels[i]]]
    swatch <- if (st == "fill") image_blank(width = LEGEND_SWATCH_PX, height = LEGEND_SWATCH_PX, color = colours[[labels[i]]]) else
      line_swatch(colours[[labels[i]]], st)
    panel <- image_composite(panel, swatch, offset = sprintf("+%d+%d", LEGEND_PAD_PX, y + round((rh - LEGEND_SWATCH_PX) / 2)))
    panel <- image_composite(panel, r$img, offset = sprintf("+%d+%d", LEGEND_PAD_PX + LEGEND_SWATCH_PX + 10L, y + round((rh - r$i$height) / 2)))
    y <- y + rh + LEGEND_ROW_GAP_PX
  }
  panel
}

# legend text scale: same px-per-legend-px factor as the IMOS region map's legend (longest label
# "Australia-deployed floats" scaled to 260px), so text size is independent of label length
ref_legend_w <- image_info(build_two_way_legend(c("A", "B"), c(A = "white", B = "white"),
                                                c(A = "Australia-deployed floats", B = "Other floats")))$width
LEGEND_SCALE <- 260 / ref_legend_w

## ---- 4. render (both options, shared basemap) --------------------------------

for (v in names(VERSIONS)) {
  cfg <- VERSIONS[[v]]
  segs[, period := factor(fifelse(eval(cfg$class_live), "Live", "Historical"), levels = DRAW_ORDER)]
  setorder(segs, period)
  segs[, alpha_val := SEGMENT_ALPHA_VALUES[as.character(period)]]
  n_floats <- c(Live = uniqueN(segs[period == "Live"]$track_id),
                Historical = if (is.null(cfg$hist_floats)) uniqueN(segs[period == "Historical"]$track_id)
                             else uniqueN(segs[eval(cfg$hist_floats)]$track_id))
  if (!is.null(cfg$counts)) n_floats <- cfg$counts
  log_msg("Version %s: %s = %d floats; %s = %d floats.", v,
          cfg$labels[["Live"]], n_floats[["Live"]], cfg$labels[["Historical"]], n_floats[["Historical"]])

  # legends carry no float counts - drop the placeholder
  display_labels <- setNames(vapply(DRAW_ORDER, function(k) sub(" {counts}", "", cfg$labels[[k]], fixed = TRUE), character(1)), DRAW_ORDER)
  # float rows (Live first) + the sea ice line row
  legend_keys <- c(rev(DRAW_ORDER), "ice_max")
  legend_img <- build_two_way_legend(legend_keys,
                                     c(TRACK_COLOURS, ice_max = SEAICE_COL),
                                     c(display_labels, ice_max = SEAICE_LEGEND[["max"]]),
                                     swatch_styles = c(Live = "fill", Historical = "fill", ice_max = "solid"))
  legend_info <- image_info(legend_img)
  legend_target_w_px <- round(legend_info$width * LEGEND_SCALE)
  legend_resized <- magick::image_resize(legend_img, sprintf("%dx", legend_target_w_px * 2))
  legend_grob <- grid::rasterGrob(as.raster(legend_resized), interpolate = TRUE)
  legend_w <- diff(xlim) * (legend_target_w_px / MAP_PX_W)
  legend_h <- legend_w * (legend_info$height / legend_info$width)
  legend_pad <- diff(xlim) * 0.02
  legend_xmax <- xlim[2] - legend_pad; legend_xmin <- legend_xmax - legend_w
  legend_ymin <- ylim[1] + legend_pad; legend_ymax <- legend_ymin + legend_h

  p <- ggplot() +
    geom_raster(data = basemap_dt, aes(x = x, y = y, fill = col)) +
    scale_fill_identity() +
    geom_sf(data = coast, fill = NA, color = "black", linewidth = 0.15, inherit.aes = FALSE) +
    geom_text(data = continent_labels, aes(x = x, y = y, label = label, vjust = vjust, hjust = hjust, angle = angle, size = size),
              color = LAND_LABEL_COL, lineheight = 0.85, family = "Roboto Condensed GOOS") +
    scale_size_identity() +
    geom_segment(data = segs, aes(x = x, y = y, xend = xend, yend = yend, color = period, alpha = alpha_val),
                 linewidth = SEGMENT_LINEWIDTH, lineend = "round") +
    scale_color_manual(values = TRACK_COLOURS, guide = "none") +
    scale_alpha_identity() +
    # median winter-max sea ice extent line 2021-2025 (scripts/02), under the 60 deg circle
    geom_sf(data = seaice_lines, colour = SEAICE_COL, linewidth = SEAICE_LWD, inherit.aes = FALSE) +
    # 60 deg circle, dashes "52" = 5 on / 2 off (in linewidth units)
    geom_path(data = polar_circle, aes(x = x, y = y), colour = "white", alpha = 0.75, linewidth = 0.55,
              linetype = "52", inherit.aes = FALSE) +
    geom_text(data = polar_circle_label, aes(x = x, y = y, label = label), colour = "white", alpha = 0.8,
              size = 3.6, family = "Roboto Condensed GOOS", fontface = "italic", inherit.aes = FALSE) +
    geom_text(data = ocean_halo, aes(x = x, y = y, label = label), color = "black", alpha = 0.6,
              size = OCEAN_LABEL_SIZE, lineheight = 0.85, family = "Roboto Condensed GOOS", fontface = "italic") +
    geom_text(data = ocean_labels, aes(x = x, y = y, label = label), color = "white", alpha = 0.9,
              size = OCEAN_LABEL_SIZE, lineheight = 0.85, family = "Roboto Condensed GOOS", fontface = "italic") +
    coord_sf(crs = ANIM_POLAR_CRS, xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_argo_map() +
    theme(
      plot.background  = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      plot.title    = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption  = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )

  logo_layer <- if (!is.null(logo_grob))
    annotation_custom(logo_grob, xmin = logo_xmin, xmax = logo_xmax, ymin = logo_ymin, ymax = logo_ymax)
  legend_layer <- annotation_custom(legend_grob, xmin = legend_xmin, xmax = legend_xmax, ymin = legend_ymin, ymax = legend_ymax)

  panel_dir <- file.path(FIG_DIR, "panels")
  dir.create(file.path(panel_dir, "composite"), recursive = TRUE, showWarnings = FALSE)
  # ragg device so ggplot text can use the registered (bundled) Roboto Condensed
  save_panel <- function(plot, path) {
    ggsave(path, plot, width = FRAME_PX_W / 150, height = FRAME_PX_H / 150, dpi = 150, units = "in",
           bg = "black", device = ragg::agg_png)
    log_msg("Wrote %s", path)
  }
  # standalone panel (logo + this region's legend), for editorial use
  save_panel(p + top_land_layers(SA_PX_STANDALONE) + logo_layer + legend_layer, file.path(panel_dir, sprintf("GOOS_argo_antarctic_option_%s_panel.png", v)))
  # composite variant: Antarctic (left) keeps the GOOS logo, no legend; the Arctic
  # panel (right) carries the one shared legend added by scripts/05
  save_panel(p + top_land_layers(SA_PX_COMPOSITE) + logo_layer, file.path(panel_dir, "composite", sprintf("GOOS_argo_antarctic_option_%s_base.png", v)))
  # this region's legend labels (+ logged counts), for scripts/05's shared legend
  fwrite(data.table(region = "antarctic", option = v,
                    live_label = cfg$labels[["Live"]], live_n = n_floats[["Live"]],
                    hist_label = cfg$labels[["Historical"]], hist_n = n_floats[["Historical"]],
                    ice_years = seaice_years[["max"]]),
         file.path(panel_dir, "composite", sprintf("GOOS_argo_antarctic_option_%s_legend.csv", v)))
}
log_msg("Done.")
