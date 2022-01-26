setwd("C:\\Users\\anton\\Dropbox\\drought")

#GitHub file to save dataframes to:
int_folder = "C:\\Users\\anton\\Documents\\GitHub\\drought\\data\\intermediate\\"

library(tidyverse)
library(tidyr)
library(terra)
library(dplyr)
library(lubridate)

#Get SPEI data
spei_r <- rast('./data/raw/spei01.nc')

#Project the county data to be the same as the drought data
project(county_v, spei_r)

#Match the spatial extents
spei_r <- crop(spei_r,e)

#Calculate the SPEI by county-month
#Calculating County aggregate SPEI by SIMPLE AVERAGE

spei_by_month<-data.frame()
spei_by_onemonth<-data.frame(county_v$GEOID)
colnames(spei_by_onemonth)<-"fips"

#This block takes spei and averages it over county boundaries for every month,

#t = 817: Jan 1969 t = 1416: final observation
for (t in 817:1416){
  layer = spei_r[[t]]
  spei_by_county = extract(layer,county_v, mean, method = "simple", weights = TRUE)
  spei_by_county<-data.frame(spei_by_county)
  colnames(spei_by_county)<-c("ID","spei")
  spei_by_onemonth$spei<-spei_by_county$spei
  spei_by_onemonth$year<-year(time(spei_r)[t])
  spei_by_onemonth$month<-month(time(spei_r)[t])
  print(t)
  spei_by_month<-rbind(spei_by_month, spei_by_onemonth)
}

##CHECK TO MAKE SURE THAT THE IDS CORRESPOND TO THE CORRECT FIPS CODES!

save(spei_by_month, file = paste(int_folder, "spei_by_month.RData", sep = "", collapse = NULL))
writeRaster(spei_r,  filename = paste(int_folder, "spei_r.tif", sep = "", collapse = NULL))
time_mat<-time(spei_r)
save(time_mat, file = paste(int_folder, "time_mat.RData", sep = "", collapse = NULL))

