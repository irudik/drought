# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, janitor, magrittr, fixest, raster,
  broom, tidyverse, tidylog, stars, viridis, scales
)
options("tidylog.display" = NULL)

# load mask and data
emma = fread("data/raw/emma-wrds/wrds-muni-data.csv")