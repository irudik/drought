#This code creates drought variables with my SPEI data. SPEI data exists for
#timescales from 1 month to 48 months.

#################### User Defined Inputs #######################################
#Input the timescales of SPEI to process, as strings below:
spei_timescales<-c("01","12")
#Input directory where drought folder exists
setwd("C:\\Users\\anton\\Dropbox\\drought")
#Input directory to save files to.
int_folder = ".\\data\\intermediate\\"
################################################################################

#Libraries

library(tidyverse)
library(tidyr)
library(terra)
library(dplyr)
library(lubridate)

for (ts in spei_timescales){
  filename<-paste("./data/raw/spei/spei",ts,".nc",sep="")
  #Get SPEI data
  spei_r <- rast(filename)
  
  #Project the county data to have the same coordinates as the drought data
  county_v<-vect("./data/intermediate/ county_v.GTiff")
  project(county_v, spei_r)
  #Match the spatial extents
  e <- ext(-150,-40,20,60) #This spatial extent shrinks the filesize to just US
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
    spei_by_county = extract(layer,county_v, mean, method = "simple", weights = TRUE) #This line does the calculation
    spei_by_county<-data.frame(spei_by_county)
    colnames(spei_by_county)<-c("ID","spei")
    spei_by_onemonth$spei<-spei_by_county$spei
    spei_by_onemonth$year<-year(time(spei_r)[t])
    spei_by_onemonth$month<-month(time(spei_r)[t])
    print(t)
    spei_by_month<-rbind(spei_by_month, spei_by_onemonth)
  }
  
  save(spei_by_month, file = paste(int_folder, "spei",ts,"_by_month.RData",sep="", collapse = NULL))
  writeRaster(spei_r,  filename = paste(int_folder, "spei",ts,"_r.tif", sep = "", collapse = NULL))
}  
time_mat<-time(spei_r) #The spei file saves without the time frame. It is the same for each iteration of the loop.
save(time_mat, file = paste(int_folder, "time_mat.RData", sep = "", collapse = NULL))




