setwd("C:\\Users\\anton\\Dropbox\\drought")

library(tidyverse)
library(terra)

income_data <- read.csv("./data/raw/CAINC1__ALL_AREAS_1969_2020.csv")
#Remove empty and non-helpful variables

#Keep only per-capita personal income (for now), line 3
income_data<-subset(income_data, LineCode == 3, select = -c(Region, TableName, IndustryClassification, Description, Unit, LineCode))

filename<-"./data/raw/cb_2018_us_county_5m"
e <- ext(-150,0,20,50)
county_v <- vect(filename, extent = e)


merge(county_v,income_data,all.x=TRUE, by.x="GEOID",by.y="GeoFIPS")

#Get SPEI data
spei_r <- rast('./data/raw/spei01.nc')
spei_r <- crop(spei_r,e)

#Project the county data to be the same as the drought data
project(county_v, spei_r)

#Plot income in 2018

#To extract layer 1:
2018_layer = spei_data[[1404]]

#FIRST GOAL: just find the centroid and attach the centroid to each latitude 
#longitude square to do the first simple correlations between income and drought.



