# ============================================================================
# utils.R - shared helper functions (requires config.R to be sourced first)
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

#' Fast vectorised lon/lat (WGS84) -> projected x/y (metres), for plotting
#' non-sf geoms (geom_point/geom_segment/geom_text) on the same coord_sf(crs=)
#' panel as sf layers. coord_sf only auto-transforms geom_sf() layers; plain
#' x/y geoms are passed straight to the panel's target CRS units, so lon/lat
#' degrees must be pre-projected or they render collapsed near the origin.
project_xy <- function(lon, lat, crs) {
  m <- sf::sf_project(from = "OGC:CRS84", to = crs, pts = cbind(lon, lat))
  colnames(m) <- c("x", "y")
  as.data.frame(m)
}

#' Fetch (and cache) a Southern Hemisphere Natural Earth coastline for
#' circumpolar mapping, pre-transformed into `crs`. `ymax` (default -5) can be
#' raised for a more northerly extent - pass a distinct `cache` path when doing
#' so, since the crop result isn't reusable between different ymax values.
get_southern_coastline <- function(crs, cache = file.path(GIS_DIR, "coastline_polar.rds"), ymax = -5) {
  if (file.exists(cache)) {
    return(readRDS(cache))
  }
  log_msg("Downloading Natural Earth coastline (first run only, cached afterwards)...")
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  # a bbox crop spanning the full -180/180 longitude range confuses s2
  # spherical predicates (it silently drops most polygons); switch to planar
  # GEOS for this simple lat-band crop, then restore s2 for everything else
  s2_was_on <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  world <- suppressWarnings(sf::st_crop(world, xmin = -180, xmax = 180, ymin = -90, ymax = ymax))
  sf::sf_use_s2(s2_was_on)
  # densify before transforming: in a polar-projected CRS a long, sparsely-vertexed
  # edge (e.g. near the +/-180 seam) otherwise becomes a straight chord across the
  # disk instead of following the true curved path
  world <- sf::st_segmentize(world, units::set_units(0.5, "degree"))
  world_polar <- st_transform(world, crs)
  saveRDS(world_polar, cache)
  world_polar
}
