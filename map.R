
rm(list = ls())

library(pacman)
pacman::p_load(
  raster, rgdal, ggspatial, rgeos, tidyverse, broom, ggrepel,
  wesanderson, sf, ggsflabel, cowplot
)

dpt <- readRDS(file = "./data/dpt.rds")
mun <- readRDS(file = "./data/mun.rds")

cities <- c("Sahagún", "Momil", "Repelón", "Santo Tomás", "Polonuevo")

dpt_interested <- dpt %>%
  filter(NOMBRE_DPT %in% c("CÓRDOBA", "SUCRE", "BOLÍVAR", "ATLANTICO"))

col_bb <- st_as_sfc(st_bbox(dpt_interested))


g2 <- ggplot() +
  geom_sf(
    data = dpt %>%
      filter(!NOMBRE_DPT == "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
    fill = "white", col = "black", size = .1
  ) +
  geom_sf(
    data = mun %>% filter(NOM_MUNICI %in% cities),
    aes(fill = NOM_MUNICI), show.legend = F
  ) +
  geom_sf(data = col_bb, fill = NA, color = "red", size = 1.3) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 7, face = "bold")
  )



map <- ggplot() +
  geom_sf(
    data = dpt %>%
      filter(!NOMBRE_DPT == "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
    fill = "white", col = "black", size = .1
  ) +
  geom_sf(
    data = mun %>% filter(NOM_MUNICI %in% cities),
    aes(fill = NOM_MUNICI), show.legend = F
  ) +
  geom_sf_label_repel(
    data = mun %>% filter(NOM_MUNICI %in% cities),
    aes(label = NOM_MUNICI),
    force = 100, nudge_x = -2, seed = 10
  ) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5)) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "tr", which_north = "true",
  #                        pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  coord_sf() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0.005, 0.005),
    legend.key.size = unit(0.9, "cm"),
    legend.background = element_rect(fill = alpha("white", 1), colour = alpha("white", 0.4))
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  annotate(
    geom = "text", x = -68, y = -3, label = "Autor: Luis Fdo. \nDelgado",
    color = "grey22", size = 2
  )

macro <- map +
  coord_sf(
    ylim = c(5, 12.2),
    xlim = c(-78, -67), expand = T
  )


gg_inset <- ggdraw() +
  draw_plot(macro) +
  draw_plot(g2, x = 0.69, y = 0.61, width = 0.40, height = 0.30)

save_plot("output/map.png", gg_inset,
  base_aspect_ratio = 1.5 # make room for figure legend
)
