# create the poodle logo of the styledTables package
# install.packages("hexSticker")
hexSticker::sticker(
  "poodle.png",
  package = c("styled   ", "    table"),
  p_size = 22,
  p_y = 0.7,
  p_x = c(0.68, 1.26),
  s_y = 1.3,
  s_x = 1,
  h_fill = "#ff81ff",
  p_color = c("#e91e63", "#fffafa"),
  h_color = "#fa61fa",
  s_width = 0.4,
  s_height = 0.4,
  filename = "../man/figures/logo.png"
)
