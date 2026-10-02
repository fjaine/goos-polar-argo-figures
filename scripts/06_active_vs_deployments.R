# ============================================================================
# 06_active_vs_deployments.R
#
# Companion chart to the GOOS map figures (scripts/03-04): floats ACTIVE each year vs
# floats DEPLOYED each year, 2001-2026, for the two polar regions and the global array
# (ALL countries).
#
# Definitions (kept consistent with the maps' Option B):
#   - Deployed in year Y, region R: the float's FIRST profile is in Y and in R.
#   - Active on 1 July of year Y, region R: deployed on/before that date and not
#     yet past its final transmission, and its latest profile on/before that date
#     is in R. Floats that Option B classes as still active at the index snapshot
#     (profile <= 90 d, or <= 13 months poleward of 60 deg for sea ice) count as
#     alive up to the snapshot, so silent under-ice floats don't fake a 2026 dip.
#   - Regions: south of 60S, north of 60N, and global (all latitudes).
#
# Design: three stacked small multiples, each with its OWN y-scale (a ~4,000-float
# global array would flatten the polar series) but ONE axis per panel - both
# measures are counts of floats, so no dual axis. Deployments = blue bars, active =
# amber line (same colours as the map figures); 2026 deployments bar drawn lighter
# (Jan-Jul only). Same GOOS canvas/fonts/footer as the maps.
#
# FIGURE 2 (same script, global array): (a) the age structure of the active fleet on 1 July each year (stacked by age class, sequential
# amber ramp, older = lighter/more salient; checked in OKLab: >= 0.10 L between
# adjacent classes, darkest L 0.51 on black), and (b) floats added (deployed) vs
# lost (final transmission, floats not active at the snapshot) each year, as
# bars above/below one zero line. Recent losses are a lower bound: floats silent
# < 90 d (or < 13 months poleward of 60 deg) aren't counted as lost yet.
#
# Outputs: output/figures/GOOS_argo_active_vs_deployments.png
#          output/figures/GOOS_argo_fleet_age_and_replacement.png
#          output/tables/GOOS_argo_active_vs_deployments.csv (the plotted numbers)
#          output/tables/GOOS_argo_fleet_age_and_replacement.csv
# ============================================================================

# Roboto via fontconfig for magick, systemfonts + ragg for ggplot (see scripts/03).
FONT_DIR <- normalizePath("fonts")
local({
  cf <- file.path(tempdir(), "goos_fonts.conf")
  writeLines(c('<?xml version="1.0"?>', '<!DOCTYPE fontconfig SYSTEM "fonts.dtd">', "<fontconfig>",
               sprintf("<dir>%s</dir>", FONT_DIR), "<dir>/System/Library/Fonts</dir>", "<dir>/Library/Fonts</dir>",
               sprintf("<cachedir>%s</cachedir>", file.path(tempdir(), "fccache")), "</fontconfig>"), cf)
  Sys.setenv(FONTCONFIG_FILE = cf)
})
FONT_HEADING <- "Roboto"
FONT_LABEL   <- "Roboto Condensed"

source("R/config.R")
source("R/utils.R")
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(magick)
})
systemfonts::register_font("Roboto Condensed GOOS",
                           plain = file.path(FONT_DIR, "RobotoCondensed-Regular.ttf"),
                           bold  = file.path(FONT_DIR, "RobotoCondensed-Bold.ttf"),
                           italic = file.path(FONT_DIR, "RobotoCondensed-Italic.ttf"))
systemfonts::register_font("Roboto GOOS", plain = file.path(FONT_DIR, "Roboto-Bold.ttf"),
                           bold = file.path(FONT_DIR, "Roboto-Bold.ttf"))
stopifnot(any(magick_fonts()$family == FONT_LABEL), any(magick_fonts()$family == FONT_HEADING))

## ---- CONFIG ------------------------------------------------------------

LOGO_PATH <- "GOOS_Main_logo_-_white.png"
YEAR_FIRST <- 2001L
ACTIVE_WINDOW_DAYS    <- 90
UNDER_ICE_LAT         <- 60
UNDER_ICE_WINDOW_DAYS <- 396  # 13 months, matches the GOOS map scripts (both hemispheres)
POLAR_LAT             <- 60   # region boundaries: south of -60, north of +60
COL_DEPLOYED <- "#5E89BC"     # map figures' historical blue
COL_ACTIVE   <- "#F0A500"     # map figures' live amber
REGION_LABELS <- c(S = "Southern polar region (south of 60°S)",
                   N = "Arctic region (north of 60°N)",
                   G = "Global array")

CANVAS_W <- 1080L; CANVAS_H <- 1350L
MARGIN_SIDE <- 36L; MARGIN_TOP <- 80L; MARGIN_BOTTOM <- 45L
CONTENT_W <- CANVAS_W - 2L * MARGIN_SIDE
TITLE_H <- 56L; GAP_A <- 30L; GAP_BEFORE_FOOTER <- 40L
FOOTER_H <- 81L; FOOTER_TEXT_SIZE <- 12L; FOOTER_TEXT_RAISE <- 25; FOOTER_LINE_GAP <- 0.8
CHART_H <- CANVAS_H - MARGIN_TOP - TITLE_H - GAP_A - GAP_BEFORE_FOOTER - FOOTER_H - MARGIN_BOTTOM
TITLE_TEXT <- sprintf("Argo floats active vs deployed each year (%d-2026)", YEAR_FIRST)

## ---- 1. data --------------------------------------------------------------

idx <- readRDS(file.path(RDS_DIR, "argo_index_clean.rds"))
setDT(idx)
idx <- idx[!is.na(latitude) & !is.na(date)]
setkey(idx, wmo, date)
snapshot_date <- max(idx$date)
log_msg("Loaded %d profiles, %d floats; index snapshot %s.", nrow(idx), uniqueN(idx$wmo), format(snapshot_date, "%Y-%m-%d"))

region_of <- function(lat) fifelse(lat <= -POLAR_LAT, "S", fifelse(lat >= POLAR_LAT, "N", NA_character_))

fl <- idx[, .(dep = date[1], dep_lat = latitude[1], last = date[.N], last_lat = latitude[.N]), by = wmo]
fl[, silent := as.numeric(difftime(snapshot_date, last, units = "days"))]
fl[, active_now := silent <= ACTIVE_WINDOW_DAYS | (abs(last_lat) > UNDER_ICE_LAT & silent <= UNDER_ICE_WINDOW_DAYS)]
fl[, last_ext := fifelse(active_now, snapshot_date, last)]
fl[, dep_year := as.integer(format(dep, "%Y"))]

years <- YEAR_FIRST:as.integer(format(snapshot_date, "%Y"))

# deployments per year, by region of first profile
dep <- rbind(
  fl[dep_year %in% years & !is.na(region_of(dep_lat)), .(n = .N), by = .(year = dep_year, region = region_of(dep_lat))],
  fl[dep_year %in% years, .(n = .N, region = "G"), by = .(year = dep_year)]
)

# active on 1 July, by region of latest position on/before that date (rolling join)
act <- rbindlist(lapply(years, function(y) {
  t <- as.POSIXct(sprintf("%d-07-01", y), tz = "UTC")
  alive <- fl[dep <= t & last_ext >= t, wmo]
  pos <- idx[.(alive, t), on = .(wmo, date), roll = TRUE, .(wmo, latitude)]
  pos[, region := region_of(latitude)]
  rbind(pos[!is.na(region), .(n = .N), by = region], data.table(region = "G", n = nrow(pos)))[, year := y]
}))

tab <- merge(dcast(dep, year + region ~ ., value.var = "n", fun.aggregate = sum)[, .(year, region, deployed = .)],
             act[, .(year, region, active_1_july = n)], by = c("year", "region"), all = TRUE)
tab[is.na(deployed), deployed := 0L]
tab[is.na(active_1_july), active_1_july := 0L]  # e.g. no floats south of 60S on 1 July 2001
tab[, partial_year := year == max(years)]
setorder(tab, region, year)
fwrite(tab[, .(region = REGION_LABELS[region], year, deployed, active_1_july, deployed_partial_year = partial_year)],
       file.path(TABLE_DIR, "GOOS_argo_active_vs_deployments.csv"))
for (r in names(REGION_LABELS)) {
  x <- tab[region == r]
  log_msg("%s: active %d (%d) -> %d (%d); deployed %d (%d) -> %d (%d, partial).", REGION_LABELS[[r]],
          x$active_1_july[1], x$year[1], x$active_1_july[nrow(x)], x$year[nrow(x)],
          x$deployed[1], x$year[1], x$deployed[nrow(x)], x$year[nrow(x)])
}

## ---- 2. chart ---------------------------------------------------------------

# shared chart theme (black canvas, Roboto Condensed, recessive grid) - reused by figure 2
goos_theme <- theme_minimal(base_family = "Roboto Condensed GOOS", base_size = 13) +
theme(
  plot.background   = element_rect(fill = "black", colour = NA),
  panel.background  = element_rect(fill = "black", colour = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor  = element_blank(),
  panel.grid.major.y = element_line(colour = "#2a2a2a", linewidth = 0.3),
  axis.text         = element_text(colour = "grey70", size = 11),
  axis.title.y      = element_text(colour = "grey70", size = 12, margin = margin(r = 8)),
  axis.ticks.x      = element_line(colour = "grey40", linewidth = 0.3),
  strip.text        = element_text(family = "Roboto GOOS", face = "bold", colour = "white", size = 14,
                                   hjust = 0, margin = margin(t = 10, b = 6)),
  legend.position   = "top",
  legend.justification = "left",
  legend.text       = element_text(colour = "white", size = 13),
  legend.key.width  = unit(22, "pt"),
  legend.margin     = margin(0, 0, 4, 0),
  panel.spacing.y   = unit(14, "pt"),
  plot.caption      = element_text(colour = "grey55", size = 9.5, hjust = 0, lineheight = 1.1, margin = margin(t = 12)),
  plot.caption.position = "plot",
  plot.margin       = margin(4, 10, 0, 4)
)

tab[, region_f := factor(REGION_LABELS[region], levels = REGION_LABELS)]
last_pts <- tab[year == max(years)]

p <- ggplot(tab, aes(x = year)) +
  geom_col(aes(y = deployed, fill = "Floats deployed that year", alpha = partial_year), width = 0.72) +
  geom_line(aes(y = active_1_july, colour = "Floats active on 1 July"), linewidth = 0.9, lineend = "round") +
  geom_point(data = last_pts, aes(y = active_1_july, colour = "Floats active on 1 July"), size = 2.2) +
  geom_text(data = last_pts, aes(y = active_1_july, label = format(active_1_july, big.mark = ",")),
            hjust = -0.25, vjust = 0.5, colour = "white", size = 3.6, family = "Roboto Condensed GOOS") +
  # partial-year note to the right of the last bar (clear of the neighbouring bar)
  geom_text(data = last_pts, aes(x = year + 0.45, y = deployed / 2, label = "Jan-Jul"), hjust = 0,
            colour = "grey65", size = 2.9, family = "Roboto Condensed GOOS") +
  facet_wrap(~region_f, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Floats deployed that year" = COL_DEPLOYED), name = NULL) +
  scale_colour_manual(values = c("Floats active on 1 July" = COL_ACTIVE), name = NULL) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.45), guide = "none") +
  scale_x_continuous(breaks = seq(2005, 2025, 5), limits = c(YEAR_FIRST - 0.6, max(years) + 2.6), expand = c(0, 0)) +  # room for end labels
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.12)),
                     limits = c(0, NA)) +
  guides(colour = guide_legend(order = 1, override.aes = list(linewidth = 1.2, size = 0)),
         fill = guide_legend(order = 2)) +
  labs(x = NULL, y = "Number of floats") +
  goos_theme

chart_path <- file.path(tempdir(), "goos_active_vs_deployments.png")
ggsave(chart_path, p, width = CONTENT_W / 150, height = CHART_H / 150, dpi = 150, units = "in",
       bg = "black", device = ragg::agg_png)

## ---- 3. compose (same canvas/header/footer as the GOOS maps) ---------------

build_header <- function(canvas_w, h, title, size = 36) {
  repeat {
    probe <- image_trim(image_annotate(image_blank(canvas_w * 3, h * 3, "black"), title, font = FONT_HEADING,
                                       size = size, weight = 700, color = "white"), fuzz = 1)
    if (image_info(probe)$width <= canvas_w * 0.97 || size <= 20) break
    size <- size - 1
  }
  image_annotate(image_blank(canvas_w, h, "black"), title, font = FONT_HEADING, size = size, weight = 700,
                 color = "white", gravity = "north", location = "+0+2")
}

FOOTER_STATEMENT <- "Polar Argo floats are deployed as part of the international OneArgo program (https://argo.ucsd.edu/oneargo/)"
FOOTER_RUNS <- list(
  list("Data: ", TRUE),
  list("Argo Global Profile Index (Coriolis) & IMOS Australian Ocean Data Network  |  ", FALSE),
  list("Data visualisation: ", TRUE),
  list("Fabrice Jaine (Integrated Marine Observing System, IMOS)", FALSE)
)
footer_gp <- function(bold, size) grid::gpar(fontfamily = "Roboto Condensed GOOS", fontface = if (bold) "bold" else "plain",
                                             fontsize = size, col = "grey65")
build_footer_block <- function(strip_w, size = FOOTER_TEXT_SIZE) {
  png_path <- file.path(tempdir(), "goos_trend_footer.png")
  ragg::agg_png(png_path, width = strip_w, height = FOOTER_H, res = 72, background = "black")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = c(0, strip_w), yscale = c(0, FOOTER_H)))
  widths <- vapply(FOOTER_RUNS, function(r) grid::convertWidth(grid::grobWidth(grid::textGrob(r[[1]], gp = footer_gp(r[[2]], size))),
                                                              "native", valueOnly = TRUE), numeric(1))
  top_baseline <- FOOTER_H / 2 - size * 0.35 + FOOTER_TEXT_RAISE
  grid::grid.text(FOOTER_STATEMENT, x = grid::unit(strip_w / 2, "native"), y = grid::unit(top_baseline, "native"),
                  hjust = 0.5, vjust = 0, gp = footer_gp(FALSE, size))
  x0 <- (strip_w - sum(widths)) / 2
  for (k in seq_along(FOOTER_RUNS)) {
    grid::grid.text(FOOTER_RUNS[[k]][[1]], x = grid::unit(x0 + sum(widths[seq_len(k - 1)]), "native"),
                    y = grid::unit(top_baseline - size * (1 + FOOTER_LINE_GAP), "native"), hjust = 0, vjust = 0,
                    gp = footer_gp(FOOTER_RUNS[[k]][[2]], size))
  }
  invisible(grDevices::dev.off())
  image_read(png_path)
}

TITLE_Y <- MARGIN_TOP
CHART_Y <- TITLE_Y + TITLE_H + GAP_A
FOOTER_Y <- CANVAS_H - MARGIN_BOTTOM - FOOTER_H

LOGO_W <- 150L
logo <- image_resize(image_trim(image_read(LOGO_PATH)), sprintf("%dx", LOGO_W))
compose_figure <- function(title, chart_png, out_name) {
  canvas <- image_blank(CANVAS_W, CANVAS_H, "black")
  canvas <- image_composite(canvas, build_header(CONTENT_W, TITLE_H, title), offset = sprintf("+%d+%d", MARGIN_SIDE, TITLE_Y))
  canvas <- image_composite(canvas, image_read(chart_png), offset = sprintf("+%d+%d", MARGIN_SIDE, CHART_Y))
  canvas <- image_composite(canvas, build_footer_block(CONTENT_W), offset = sprintf("+%d+%d", MARGIN_SIDE, FOOTER_Y))
  # GOOS logo top-right of the chart area, level with the (left-justified) first legend row
  canvas <- image_composite(canvas, logo, offset = sprintf("+%d+%d", CANVAS_W - MARGIN_SIDE - LOGO_W, CHART_Y - 4L))
  out_path <- file.path(FIG_DIR, out_name)
  image_write(canvas, out_path)
  log_msg("Wrote %s", out_path)
}
compose_figure(TITLE_TEXT, chart_path, "GOOS_argo_active_vs_deployments.png")

## ---- 4. FIGURE 2: fleet age + floats added vs lost (global) -----------------

AGE_BREAKS <- c(0, 2, 4, 6, Inf)
AGE_LABELS <- c("Under 2 years", "2-4 years", "4-6 years", "6+ years")
AGE_COLS <- setNames(c("#8A5A00", "#C98200", "#F7B733", "#FFE0A3"), AGE_LABELS)  # young dark -> old light
COL_LOST <- "grey55"

age <- rbindlist(lapply(years, function(y) {
  t <- as.POSIXct(sprintf("%d-07-01", y), tz = "UTC")
  a <- fl[dep <= t & last_ext >= t]
  a[, age_cls := cut(as.numeric(difftime(t, dep, units = "days")) / 365.25, AGE_BREAKS, AGE_LABELS, right = FALSE)]
  a[, .(n = .N), by = age_cls][, year := y]
}))
age[, age_cls := factor(age_cls, levels = AGE_LABELS)]
age_tot <- age[, .(total = sum(n), old = sum(n[age_cls %in% AGE_LABELS[3:4]])), by = year][, pct_old := round(100 * old / total)]
setorder(age_tot, year)
# selective direct labels: share aged 4+ years at the start of each decade-ish and now
pct_lab <- age_tot[year %in% c(2010L, 2015L, 2020L, max(years))]

repl <- merge(fl[dep_year %in% years, .(added = .N), by = .(year = dep_year)],
              fl[!active_now & as.integer(format(last, "%Y")) %in% years, .(lost = .N), by = .(year = as.integer(format(last, "%Y")))],
              by = "year", all = TRUE)
repl[is.na(added), added := 0L][is.na(lost), lost := 0L]
repl[, partial_year := year == max(years)]
setorder(repl, year)

fwrite(merge(dcast(age, year ~ age_cls, value.var = "n", fill = 0L), age_tot[, .(year, active_total = total, pct_aged_4plus = pct_old)], by = "year")[
  repl[, .(year, deployed = added, stopped_transmitting = lost, partial_year)], on = "year"],
  file.path(TABLE_DIR, "GOOS_argo_fleet_age_and_replacement.csv"))
log_msg("Share of active fleet aged 4+ years: %s.", paste(sprintf("%d %d%%", pct_lab$year, pct_lab$pct_old), collapse = ", "))

x_scale <- scale_x_continuous(breaks = seq(2005, 2025, 5), limits = c(YEAR_FIRST - 0.6, max(years) + 2.6), expand = c(0, 0))

p_age <- ggplot(age, aes(x = year, y = n, fill = age_cls)) +
  geom_col(width = 0.72) +
  geom_text(data = pct_lab, aes(x = year, y = total, label = sprintf("%d%%\naged 4+", pct_old)), inherit.aes = FALSE,
            vjust = -0.3, lineheight = 0.9, colour = "white", size = 3.2, family = "Roboto Condensed GOOS") +
  scale_fill_manual(values = AGE_COLS, breaks = AGE_LABELS, name = NULL) +
  x_scale +
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.2)), limits = c(0, NA)) +
  labs(title = "Age of the active fleet on 1 July", x = NULL, y = "Number of floats") +
  goos_theme +
  theme(plot.title = element_text(family = "Roboto GOOS", face = "bold", colour = "white", size = 14, margin = margin(t = 6, b = 6)),
        plot.title.position = "plot", legend.key.width = unit(14, "pt"))

p_rep <- ggplot(repl, aes(x = year)) +
  geom_col(aes(y = added, fill = "Floats deployed", alpha = partial_year), width = 0.72) +
  geom_col(aes(y = -lost, fill = "Floats that stopped transmitting", alpha = partial_year), width = 0.72) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.4) +
  geom_text(data = repl[partial_year == TRUE], aes(x = year + 0.45, y = added / 2, label = "Jan-Jul"), hjust = 0,
            colour = "grey65", size = 2.9, family = "Roboto Condensed GOOS") +
  scale_fill_manual(values = c("Floats deployed" = COL_DEPLOYED, "Floats that stopped transmitting" = COL_LOST), name = NULL) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.45), guide = "none") +
  x_scale +
  scale_y_continuous(labels = function(v) scales::comma(abs(v)), expand = expansion(mult = 0.08)) +
  labs(title = "Floats added vs lost each year", x = NULL, y = "Number of floats") +
  goos_theme +
  theme(plot.title = element_text(family = "Roboto GOOS", face = "bold", colour = "white", size = 14, margin = margin(t = 6, b = 6)),
        plot.title.position = "plot")

# stack the two panels (separate ggsaves: they have different legends/encodings)
panel_h <- c(age = round(CHART_H * 0.53), rep = CHART_H - round(CHART_H * 0.53))
pa <- file.path(tempdir(), "goos_fleet_age.png"); pr <- file.path(tempdir(), "goos_fleet_repl.png")
ggsave(pa, p_age, width = CONTENT_W / 150, height = panel_h[["age"]] / 150, dpi = 150, units = "in", bg = "black", device = ragg::agg_png)
ggsave(pr, p_rep, width = CONTENT_W / 150, height = panel_h[["rep"]] / 150, dpi = 150, units = "in", bg = "black", device = ragg::agg_png)
chart2_path <- file.path(tempdir(), "goos_fleet_age_and_replacement.png")
image_write(image_append(c(image_read(pa), image_read(pr)), stack = TRUE), chart2_path)
compose_figure("Argo fleet age and replacement, global array (2001-2026)", chart2_path, "GOOS_argo_fleet_age_and_replacement.png")
log_msg("Done.")
