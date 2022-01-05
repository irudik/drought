setwd("C:\\Users\\anton\\Dropbox\\drought")

#GitHub file to save dataframes to:
int_folder = "C:\\Users\\anton\\Documents\\GitHub\\drought\\data\\intermediate\\"

library(tidyverse)
library(tidyr)
library(terra)
library(dplyr)
library(lubridate)

income_data <- read.csv("./data/raw/CAINC1__ALL_AREAS_1969_2020.csv")

#Keep only per-capita personal income (for now), line 3
income_data<-subset(income_data, LineCode == 3, select = -c(Region, TableName, IndustryClassification, Description, Unit, LineCode))

cols <- names(income_data)[c(1,3:53)]
income_data[cols] <- lapply(income_data[cols], as.numeric)

#make a long dataframe
main_df<-gather(income_data, year, income, X1969:X2020, factor_key=FALSE)
main_df$year <- substring(main_df$year, 2)
main_df$year<-as.numeric(main_df$year)

########## Check to see if NAs line up later

#Add farm earnings, nonfarm earnings,  from two datasets. Note the industry definitions
#change between the two datasets.

#Income is by county, in thousands.
SIC_data<-read.csv("./data/raw/CAINC5S__ALL_AREAS_1969_2000.csv")
farm_income_old<-subset(SIC_data, LineCode == 81, 
                        select = -c(GeoName,	Region,	TableName,	LineCode,	IndustryClassification,	Description,	Unit))
nonfarm_income_old<-subset(SIC_data, LineCode == 82, 
                           select = -c(GeoName,	Region,	TableName,	LineCode,	IndustryClassification,	Description,	Unit))
farm_income_old<-gather(farm_income_old, year, farm_income, X1969:X2000, factor_key=FALSE)
nonfarm_income_old<-gather(nonfarm_income_old, year, nonfarm_income, X1969:X2000, factor_key=FALSE)


NAICS_data<-read.csv("./data/raw/CAINC5N__ALL_AREAS_2001_2020.csv")

farm_income_new<-subset(NAICS_data, LineCode == 81, 
                        select = -c(GeoName,	Region,	TableName,	LineCode,	IndustryClassification,	Description,	Unit))
nonfarm_income_new<-subset(NAICS_data, LineCode == 82, 
                           select = -c(GeoName,	Region,	TableName,	LineCode,	IndustryClassification,	Description,	Unit))
farm_income_new<-gather(farm_income_new, year, farm_income, X2001:X2020, factor_key=FALSE)
nonfarm_income_new<-gather(nonfarm_income_new, year, nonfarm_income, X2001:X2020, factor_key=FALSE)

#Combine the dataframes, make all entries numeric. Now, income is given in millions of dollars per county.
farm_income<-rbind(farm_income_old, farm_income_new)
farm_income$year <- as.numeric(substring(farm_income$year, 2))
farm_income$GeoFIPS<-as.numeric(farm_income$GeoFIPS)
farm_income$farm_income<-as.numeric(farm_income$farm_income)/1000
nonfarm_income<-rbind(nonfarm_income_old, nonfarm_income_new)
nonfarm_income$year <- as.numeric(substring(nonfarm_income$year, 2))
nonfarm_income$GeoFIPS<-as.numeric(nonfarm_income$GeoFIPS)
nonfarm_income$nonfarm_income<-as.numeric(nonfarm_income$nonfarm_income)/1000

main_df<-merge(main_df, farm_income, by=c("GeoFIPS","year"))
main_df<-merge(main_df, nonfarm_income, by=c("GeoFIPS","year"))

#Get county border data

filename<-"./data/raw/cb_2018_us_county_5m"
e <- ext(-150,-40,20,60)
county_v <- vect(filename, extent = e)
county_v$GEOID<-as.numeric(county_v$GEOID)

#Get SPEI data
spei_r <- rast('./data/raw/spei01.nc')

#Project the county data to be the same as the drought data
project(county_v, spei_r)

#Match the spatial extents
spei_r <- crop(spei_r,e)

#Reorder by FIPS
county_v <- county_v[order(county_v$GEOID)]

#Calculate the SPEI by county-month
#Calculating County aggregate SPEI by SIMPLE AVERAGE

spei_by_month<-data.frame()
spei_by_onemonth<-data.frame(county_v$GEOID)
colnames(spei_by_onemonth)<-"fips"

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

#Save files
save(main_df, file = paste(int_folder, "raw_df.RData", sep = "", collapse = NULL))
save(spei_by_month, file = paste(int_folder, "spei_by_month.RData", sep = "", collapse = NULL))
writeRaster(spei_r,  filename = paste(int_folder, "spei_r.tif"))
writeVector(county_v, filename = paste(int_folder, "county_v.GTiff"))
time_mat<-time(spei_r)
save(time_mat, file = paste(int_folder, "time_mat.RData", sep = "", collapse = NULL))
