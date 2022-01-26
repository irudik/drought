# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, janitor, magrittr, fixest, 
  broom, tidyverse, tidylog, sf
)
options("tidylog.display" = NULL)

# if we have all huc 8 can loop over and pull them all
# api call: https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=13040302&startdate=1/1/2000&enddate=1/1/2021&statisticsType=2

drought = st_read("data/raw/us-drought-monitor/USDM_20211102.shp")
