# create the poodle logo of the styledtable package
# install.packages("hexSticker")
hexSticker::sticker(
    "../man/figures/poodle.png",
    package="styledtable", 
    p_size=16,
    p_y = 0.8,
    s_x=1,
    s_y=1.3,
    h_fill = "#ff81ff",
    p_color = "#fffafa",
    h_color = "#fa61fa",
    s_width=0.3,
    s_height=0.3,
    filename="../man/figures/logo.png"
  )
