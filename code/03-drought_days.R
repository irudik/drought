##This code calculates a by-county measure of SPEI and also determines the 
##measure of drought used.

##The calculations will be done on as many of the time intervals of the SPEI
##as exist in the data

#################### User Defined Inputs ########################################

setwd("C:\\Users\\anton\\Dropbox\\drought")

#GitHub file to save dataframes to:
int_folder = "C:\\Users\\anton\\Documents\\GitHub\\drought\\data\\intermediate\\"

#Which time-scales of SPEI are we calculating?
time_scale = c("01","12")

#What is the cutoff level for a drought?
drought_cutoff = -1

################################################################################

library(tidyverse)
library(lubridate)
library(tidyr)
library(terra)
library(dplyr)

#Load Data
load("./data/intermediate/main_df.RData")
###load("./data/intermediate/time_mat.RData")
county_v<-vect("./data/intermediate/ county_v.GTiff")
load("./data/intermediate/spei_by_month.RData")

#Initialize the full drought dataframe
drought_df<-main_df[,c("GeoFIPS", "year")]

for (ts in time_scale){
  filename = paste(int_folder, "spei", ts, "_by_month.RData", sep = "")
  load(filename)
  #Initialize the drought dataframe for the timescale:
  mini_drought_df<-as.data.frame(matrix(ncol = 3, nrow = 0))
  spei_varname = paste("drought_month", ts, sep = "")
  colnames(mini_drought_df)<-c("GeoFIPS", "year", spei_varname)
  
  #The following loop adds together the spei when the spei is less than -1
  for (y in 1969:2018){
    for (f in county_v$GEOID){
      county_year<-filter(spei_by_month, fips == f & year == y)
      drought_month<-sum(county_year[county_year$spei<drought_cutoff,]$spei, na.rm=TRUE)
      mini_drought_df[nrow(drought_df) + 1,] <- c(f,y,drought_month)
      }
    print(y)
  }
  main_df<-merge(drought_df, mini_drought_df, by=c("GeoFIPS", "year"), all = TRUE)
}

#Different ways to calculate drought:

#Indicator for whether there was a drought in the year

# drought_df$drought<-0
# for (y in 1969:2018){
#   for (f in county_v$GEOID){
#   county_year<-filter(spei_by_month, fips == f & year == y)
#   drought_df[drought_df$year == y & drought_df$GeoFIPS == f, "drought"]<-
#     ifelse(any(county_year<drought_cutoff, na.rm=TRUE),1,0)
#   }
#   print(y)
# }


#Save files
save(drought_df, file = paste(int_folder, "drought_df.RData", sep = "", collapse = NULL))

