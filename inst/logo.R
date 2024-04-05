
library(ggplot2)
library(emojifont)
library(scales)
library(hexSticker)

load.fontawesome()

d <- data.frame(x = 1, y = 1, label = fontawesome::fa("fas fa-tools"))
ap <- ggplot() +
  geom_text(data = d, aes(x = x, y = y, label = label),
            family = "fontawesome-webfont",
            size = 60) +
  ylim(0, 2) +
  theme_void() + theme_transparent()
# ap
s <- sticker(ap, package="",
             s_x=1, s_y=1.15, s_width=1.2, s_height=5,
             filename="man/figures/logo.png",
             h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "redcaptools",
             u_size = 12,
             u_x = 1,
             u_y = 0.15
)
s
