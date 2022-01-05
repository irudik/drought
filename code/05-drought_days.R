##This code calculates a by-county measure of SPEI and also determines the 
##measure of drought used.

setwd("C:\\Users\\anton\\Dropbox\\drought")

library(tidyverse)
library(lubridate)
library(tidyr)
library(terra)
library(dplyr)

#Load Data
load("./data/intermediate/main_df.RData")
spei_r <- rast("./data/intermediate/ spei_r.grd")
load("./data/intermediate/time_mat.RData")
time(spei_r)<-time_mat
county_v<-vect("./data/intermediate/ county_v.GTiff")
load("./data/intermediate/spei_by_month.RData")


#Different ways to calculate drought:

#1. Binary variable for whether there was drought in a year.
#What is the cutoff level for a drought?
drought_cutoff = -1

main_df$drought<-0
for (y in 1969:2018){
  for (f in county_v$GEOID){
  county_year<-filter(spei_by_month, fips == f & year == y)
  main_df[main_df$year == y & main_df$GeoFIPS == f, "drought"]<-
    ifelse(any(county_year<drought_cutoff, na.rm=TRUE),1,0)
  }
  print(y)
}


#2. "Drought-days" measure: add the amount of drought in a month less than -1.

main_df$drought_day<-0
for (y in 1969:2018){
  for (f in county_v$GEOID){
    county_year<-filter(spei_by_month, fips == f & year == y)
    drought_day<-sum(county_year[county_year$spei<drought_cutoff,]$spei, na.rm=TRUE)
    main_df[main_df$year == y & main_df$GeoFIPS == f, "drought_day"]<-drought_day
  }
  print(y)
}


##I SHOULD FILTER MAIN DF ONLY FOR THOSE VALUES IN COUNTY_V$GEOID

