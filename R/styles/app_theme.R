app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = NULL,
    bg = "#FFFFFF",
    fg = "#111111",
    primary = "#C1121F",
    secondary = "#4B5563",
    success = "#2F6F4E",
    danger = "#8F1D2C",
    base_font = bslib::font_collection("Inter", "system-ui", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "sans-serif"),
    heading_font = bslib::font_collection("Inter", "system-ui", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "sans-serif")
  )
}
