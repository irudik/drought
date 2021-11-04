# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, janitor, magrittr, fixest, raster,
  broom, tidyverse, tidylog, stars, viridis, scales
)
options("tidylog.display" = NULL)

# load mask and data
water_mask = read_stars("data/raw/LAND_MASK.CRI.nc")
water = read_stars("data/raw/GRCTellus.JPL.200204_202108.GLO.RL06M.MSCNv02CRI.nc") %>%
  slice(time, 1)

# mask the data
water[water_mask == 0] = NA

# plot the single layer above
ggplot() +
  geom_stars(data = water) +
  scale_fill_gradient2(
    name = "Whatever the\ndata are",
    low = "#b2182b", mid = "#ffffef", high = "#2166ac",
    midpoint = 0, space = "Lab", na.value = "#ffffff",
    guide = guide_colourbar(
      ticks.linewidth = 2,
      ticks.colour = "#333333",
      frame.colour = "#333333",
      frame.linewidth = 2
    ),
    aesthetics = "fill",
    trans = "pseudo_log",
    breaks = c(-400, 0, 400),
    limits = c(-400, 400)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    text = element_text(family = "Lato"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(1.5, "cm")
  )

ggsave("output/water_test.png", width = 20, height = 10)
