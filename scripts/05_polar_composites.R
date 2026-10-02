# ============================================================================
# 05_polar_composites.R
#
# Assembles the GOOS polar composites, one per classification option:
#   Antarctic (LEFT, keeps the GOOS logo, no legend) | Arctic (RIGHT, no logo, with
#   ONE shared legend top-right) + the shared two-line footer. No titles - the option
#   is identified by the file name and the legend.
# The shared legend covers both maps; it carries no float counts and the sea ice row
# has no years (both regions use 2021-2025). Same style as the per-panel legends in
# scripts/03-04, 12% smaller.
# Inputs come from those scripts (output/figures/panels/composite/) - run them first.
#
# Outputs: output/figures/GOOS_argo_polar_option_A.png   (deployed 2021+ vs earlier)
#          output/figures/GOOS_argo_polar_option_B.png   (active vs historical; final option)
# ============================================================================

# Roboto via fontconfig for magick, systemfonts + ragg for grid text (see scripts/03).
FONT_DIR <- normalizePath("fonts")
local({
  cf <- file.path(tempdir(), "goos_fonts.conf")
  writeLines(c('<?xml version="1.0"?>', '<!DOCTYPE fontconfig SYSTEM "fonts.dtd">', "<fontconfig>",
               sprintf("<dir>%s</dir>", FONT_DIR), "<dir>/System/Library/Fonts</dir>", "<dir>/Library/Fonts</dir>",
               sprintf("<cachedir>%s</cachedir>", file.path(tempdir(), "fccache")), "</fontconfig>"), cf)
  Sys.setenv(FONTCONFIG_FILE = cf)
})
FONT_LABEL <- "Roboto Condensed"

source("R/config.R")
source("R/utils.R")
suppressPackageStartupMessages({
  library(magick)
  library(data.table)
})
systemfonts::register_font("Roboto Condensed GOOS",
                           plain = file.path(FONT_DIR, "RobotoCondensed-Regular.ttf"),
                           bold  = file.path(FONT_DIR, "RobotoCondensed-Bold.ttf"))
stopifnot(any(magick_fonts()$family == FONT_LABEL))

COMP_DIR <- file.path(FIG_DIR, "panels", "composite")
MARGIN <- 36L           # outer margin and gap between the two panels
FOOTER_TEXT_SIZE <- 18L
FOOTER_GAP <- 45L       # equal space above and below the footer text
FOOTER_LINE_GAP <- 0.8

# same colours as the map panels
TRACK_COLOURS <- c(Live = "#F0A500", Historical = "#5E89BC")
SEAICE_COL <- "#70DDAC"

## ---- shared legend (same builder/metrics as scripts/03-04) --------------------
LEGEND_SWATCH_PX <- 22L; LEGEND_ROW_GAP_PX <- 10L; LEGEND_TEXT_SIZE <- 20L
LEGEND_PAD_PX <- 16L; LEGEND_BG_COLOR <- "black"

measure_text_img <- function(text, size, color, bg = "black") {
  img <- image_blank(width = 6000, height = size * 4, color = bg)
  img <- image_annotate(img, text, font = FONT_LABEL, size = size, color = color, gravity = "northwest", location = "+0+0")
  image_trim(img, fuzz = 1)
}
line_swatch <- function(col, w = LEGEND_SWATCH_PX, stroke = 3L) {
  image_composite(image_blank(w, w, LEGEND_BG_COLOR), image_blank(w, stroke, col), offset = sprintf("+0+%d", (w - stroke) %/% 2))
}
build_legend <- function(rows) {  # rows: list of list(label, colour, style = "fill"/"solid")
  txt <- lapply(rows, function(r) { img <- measure_text_img(r$label, LEGEND_TEXT_SIZE, "white", LEGEND_BG_COLOR); list(img = img, i = image_info(img)) })
  row_h <- vapply(txt, function(t) max(t$i$height, LEGEND_SWATCH_PX), numeric(1))
  content_w <- max(vapply(txt, function(t) LEGEND_SWATCH_PX + 10L + t$i$width, numeric(1)))
  panel <- image_blank(content_w + 2L * LEGEND_PAD_PX, sum(row_h) + LEGEND_ROW_GAP_PX * (length(rows) - 1) + 2L * LEGEND_PAD_PX, LEGEND_BG_COLOR)
  y <- LEGEND_PAD_PX
  for (k in seq_along(rows)) {
    sw <- if (rows[[k]]$style == "fill") image_blank(LEGEND_SWATCH_PX, LEGEND_SWATCH_PX, rows[[k]]$colour) else line_swatch(rows[[k]]$colour)
    panel <- image_composite(panel, sw, offset = sprintf("+%d+%d", LEGEND_PAD_PX, y + round((row_h[k] - LEGEND_SWATCH_PX) / 2)))
    panel <- image_composite(panel, txt[[k]]$img, offset = sprintf("+%d+%d", LEGEND_PAD_PX + LEGEND_SWATCH_PX + 10L, y + round((row_h[k] - txt[[k]]$i$height) / 2)))
    y <- y + row_h[k] + LEGEND_ROW_GAP_PX
  }
  panel
}
# same px-per-legend-px scale factor as the panels (IMOS region map's 260px reference legend)
# ...reduced 12% for the composite's shared legend (less overlap with the data)
COMPOSITE_LEGEND_SHRINK <- 0.88
LEGEND_SCALE <- COMPOSITE_LEGEND_SHRINK * 260 / image_info(build_legend(list(list(label = "Australia-deployed floats", colour = "white", style = "fill"),
                                                                            list(label = "Other floats", colour = "white", style = "fill"))))$width

## ---- footer ------------------------------------------------------------------
FOOTER_STATEMENT <- "Polar Argo floats are deployed as part of the international OneArgo program (https://argo.ucsd.edu/oneargo/)"
FOOTER_RUNS <- list(
  list("Data: ", TRUE),
  list("Argo Global Profile Index (Coriolis), IMOS Australian Ocean Data Network & NOAA/NSIDC Sea Ice Concentration CDR v6  |  ", FALSE),
  list("Data visualisation: ", TRUE),
  list("Fabrice Jaine (Integrated Marine Observing System, IMOS)", FALSE)
)
footer_gp <- function(bold, size) grid::gpar(fontfamily = "Roboto Condensed GOOS", fontface = if (bold) "bold" else "plain",
                                             fontsize = size, col = "grey65")
# two centred lines (statement, then mixed-weight credits), drawn with grid on ragg and
# trimmed to the text's ink bounds so the gaps above/below can be set exactly
build_footer <- function(strip_w, size) {
  h <- round(size * 5)
  png_path <- file.path(tempdir(), "goos_polar_footer.png")
  ragg::agg_png(png_path, width = strip_w, height = h, res = 72, background = "black")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(xscale = c(0, strip_w), yscale = c(0, h)))
  widths <- vapply(FOOTER_RUNS, function(r) grid::convertWidth(grid::grobWidth(grid::textGrob(r[[1]], gp = footer_gp(r[[2]], size))),
                                                              "native", valueOnly = TRUE), numeric(1))
  top_baseline <- h * 0.6
  grid::grid.text(FOOTER_STATEMENT, x = grid::unit(strip_w / 2, "native"), y = grid::unit(top_baseline, "native"),
                  hjust = 0.5, vjust = 0, gp = footer_gp(FALSE, size))
  x0 <- (strip_w - sum(widths)) / 2
  for (k in seq_along(FOOTER_RUNS)) {
    grid::grid.text(FOOTER_RUNS[[k]][[1]], x = grid::unit(x0 + sum(widths[seq_len(k - 1)]), "native"),
                    y = grid::unit(top_baseline - size * (1 + FOOTER_LINE_GAP), "native"), hjust = 0, vjust = 0,
                    gp = footer_gp(FOOTER_RUNS[[k]][[2]], size))
  }
  invisible(grDevices::dev.off())
  image_trim(image_read(png_path), fuzz = 1)
}

## ---- compose -----------------------------------------------------------------
for (opt in c("A", "B")) {
  f <- function(region, what) file.path(COMP_DIR, sprintf("GOOS_argo_%s_option_%s_%s", region, opt, what))
  needed <- c(f("antarctic", "base.png"), f("arctic", "base.png"), f("antarctic", "legend.csv"), f("arctic", "legend.csv"))
  missing <- needed[!file.exists(needed)]
  if (length(missing)) stop("Missing input(s) - run scripts/03_polar_map_panels_north.R and 04_polar_map_panels_south.R first: ",
                            paste(basename(missing), collapse = ", "))
  antarctic <- image_read(needed[1]); arctic <- image_read(needed[2])
  ls <- fread(needed[3]); ln <- fread(needed[4])
  stopifnot(ls$live_label == ln$live_label, ls$hist_label == ln$hist_label)
  pw <- image_info(antarctic)$width; ph <- image_info(antarctic)$height

  # shared legend (same labels in both regions; checked below)
  legend <- build_legend(list(
    # no float counts in the legend - drop scripts/03-04's {counts} placeholder
    list(label = sub(" {counts}", "", ls$live_label, fixed = TRUE),
         colour = TRACK_COLOURS[["Live"]], style = "fill"),
    list(label = sub(" {counts}", "", ls$hist_label, fixed = TRUE),
         colour = TRACK_COLOURS[["Historical"]], style = "fill"),
    # both regions use the same 2021-2025 years (scripts/02), so no years shown
    list(label = "Median winter max sea ice extent", colour = SEAICE_COL, style = "solid")
  ))
  legend <- image_resize(legend, sprintf("%dx", round(image_info(legend)$width * LEGEND_SCALE)))
  li <- image_info(legend)
  pad <- round(pw * 0.02)  # same 2% corner padding as the panels
  arctic <- image_composite(arctic, legend, offset = sprintf("+%d+%d", pw - pad - li$width, pad))

  W <- 3L * MARGIN + 2L * pw
  footer <- build_footer(W - 2L * MARGIN, FOOTER_TEXT_SIZE)
  fi <- image_info(footer)
  H <- MARGIN + ph + FOOTER_GAP + fi$height + FOOTER_GAP

  canvas <- image_blank(W, H, "black")
  canvas <- image_composite(canvas, antarctic, offset = sprintf("+%d+%d", MARGIN, MARGIN))
  canvas <- image_composite(canvas, arctic, offset = sprintf("+%d+%d", 2L * MARGIN + pw, MARGIN))
  canvas <- image_composite(canvas, footer, offset = sprintf("+%d+%d", round((W - fi$width) / 2), MARGIN + ph + FOOTER_GAP))

  out_path <- file.path(FIG_DIR, sprintf("GOOS_argo_polar_option_%s.png", opt))
  image_write(canvas, out_path)
  log_msg("Wrote %s (%d x %d)", out_path, W, H)
}
log_msg("Done.")
