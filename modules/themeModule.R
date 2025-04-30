# modules/themeModule.R

# Unified professional theme (inspired by Gantt style, Tableau aesthetics)
theme_dashboard <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.background = element_rect(fill = "#f8f8f8", color = NA),
      plot.background = element_rect(fill = "#f8f8f8", color = NA),
      panel.grid.major.x = element_line(color = "#d0d0d0"),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
      axis.text.y = element_text(face = "bold", color = "#333333"),
      axis.title = element_text(face = "bold", color = "#444444"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5, color = "#222222"),
      strip.text = element_text(face = "bold", color = "white"),
      strip.background = element_rect(fill = "#003366", color = NA),
      legend.title = element_text(face = "bold", color = "#333333"),
      legend.text = element_text(color = "#333333"),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.key = element_blank()
    )
}
