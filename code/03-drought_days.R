##This code calculates a by-county measure of SPEI and also determines the 
##measure of drought used.

setwd("C:\\Users\\anton\\Dropbox\\drought")

#GitHub file to save dataframes to:
int_folder = "C:\\Users\\anton\\Documents\\GitHub\\drought\\data\\intermediate\\"


library(tidyverse)
library(lubridate)
library(tidyr)
library(terra)
library(dplyr)

#Load Data
load("./data/intermediate/raw_df.RData")
load("./data/intermediate/time_mat.RData")
county_v<-vect("./data/intermediate/ county_v.GTiff")
load("./data/intermediate/spei_by_month.RData")

#Initialize the drought dataframe:
drought_df<-main_df[,c("GeoFIPS", "year")]

#Different ways to calculate drought:

#1. Binary variable for whether there was drought in a year.
#What is the cutoff level for a drought?
drought_cutoff = -1

# drought_df$drought<-0
# for (y in 1969:2018){
#   for (f in county_v$GEOID){
#   county_year<-filter(spei_by_month, fips == f & year == y)
#   drought_df[drought_df$year == y & drought_df$GeoFIPS == f, "drought"]<-
#     ifelse(any(county_year<drought_cutoff, na.rm=TRUE),1,0)
#   }
#   print(y)
# }


#2. "Drought-days" measure: add the amount of drought in a month less than -1.

drought_df$drought_day<-0
for (y in 1969:2018){
  for (f in county_v$GEOID){
    county_year<-filter(spei_by_month, fips == f & year == y)
    drought_day<-sum(county_year[county_year$spei<drought_cutoff,]$spei, na.rm=TRUE)
    drought_df[drought_df$year == y & drought_df$GeoFIPS == f, "drought_day"]<-drought_day
  }
  print(y)
}

##Filter drought_df only for those values in county_v$GEOID
drought_df<- filter(drought_df, GeoFIPS %in% county_v$GEOID)

#Save files
save(drought_df, file = paste(int_folder, "drought_df.RData", sep = "", collapse = NULL))

