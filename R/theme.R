# ============================================================================
# theme.R - base ggplot2 theme for the map figures (scripts add their own
# black backgrounds and margins on top of it)
# ============================================================================

INK_PRIMARY   <- "#0b0b0b"
INK_SECONDARY <- "#52514e"
INK_MUTED     <- "#898781"
SURFACE       <- "#fcfcfb"

theme_argo_map <- function(base_size = 11) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = INK_PRIMARY, size = base_size * 1.25),
      plot.subtitle = ggplot2::element_text(color = INK_SECONDARY, size = base_size * 0.95,
                                             margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(color = INK_MUTED, size = base_size * 0.75, hjust = 0),
      legend.title = ggplot2::element_text(color = INK_PRIMARY, size = base_size * 0.9, face = "bold"),
      legend.text = ggplot2::element_text(color = INK_SECONDARY, size = base_size * 0.8),
      legend.position = "right",
      panel.background = ggplot2::element_rect(fill = SURFACE, color = NA),
      plot.background = ggplot2::element_rect(fill = SURFACE, color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
