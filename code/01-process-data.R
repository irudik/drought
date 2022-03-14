#After a brief scan, it looks like the only NAs are in Alaska. No other variables
#have missing results.

setwd("C:\\Users\\anton\\Dropbox\\drought")

#GitHub file to save dataframes to:
int_folder = ".\\data\\intermediate\\"

library(tidyverse)
library(tidyr)
library(terra)
library(dplyr)
library(lubridate)
library(haven)

################### MERGING IN BEA DATA #########################################

income_data <- read.csv("./data/raw/CAINC1__ALL_AREAS_1969_2020.csv")

#Keep only total income (thousands) and per-capita personal income (for now)
income_data<-subset(income_data, select = -c(Region, TableName, IndustryClassification, Description, Unit))


# cols <- names(income_data)[c(1,3:53)]
# income_data[cols] <- lapply(income_data[cols], as.numeric)

#make a long dataframe
income_data<-gather(income_data, year, value, X1969:X2020, factor_key=FALSE)
income_data<-spread(income_data,key=LineCode,value=value)
colnames(income_data)<-c("GeoFIPS", "GeoName", "year","total_income","population","pc_income","drop")
income_data<-subset(income_data,select=-c(drop))
income_data$year <- as.numeric(substring(income_data$year, 2))
income_data[,c("GeoFIPS","total_income","population","pc_income")] <-lapply(income_data[,c("GeoFIPS","total_income","population","pc_income")],as.numeric)
income_data$total_income<-income_data$total_income/1000 #Income data in millions
income_data$population<-income_data$population/1000000 #Population in millions


#Define a dataframe to merge other data onto
main_df<-income_data

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

#Combine the dataframes, make all entries numeric. Income is given in thousands of dollars per county.
farm_income<-rbind(farm_income_old, farm_income_new)
farm_income$GeoFIPS<-as.numeric(gsub("\"","",farm_income$GeoFIPS)) 
farm_income$year <- as.numeric(substring(farm_income$year, 2))
farm_income$farm_income<-as.numeric(farm_income$farm_income)/1000 #Now, income in millions
nonfarm_income<-rbind(nonfarm_income_old, nonfarm_income_new)
nonfarm_income$GeoFIPS<-as.numeric(gsub("\"","",nonfarm_income$GeoFIPS)) 
nonfarm_income$year <- as.numeric(substring(nonfarm_income$year, 2))
nonfarm_income$nonfarm_income<-as.numeric(nonfarm_income$nonfarm_income)/1000 #Now, income in millions

main_df<-merge(main_df, farm_income, by=c("GeoFIPS","year"), all=TRUE)
main_df<-merge(main_df, nonfarm_income, by=c("GeoFIPS","year"), all=TRUE)

################### ADD COUNTY BORDER, CHARACTERISTICS DATA ####################

#Get county border data

filename<-"./data/raw/cb_2018_us_county_5m"
e <- ext(-150,-40,20,60)
county_v <- vect(filename, extent = e)
county_v$GEOID<-as.numeric(county_v$GEOID)
#Reorder by FIPS
county_v <- county_v[order(county_v$GEOID)]

#Create a dataframe of centroids of the counties. This is necessary to use 
#Conley standard errors
centroid_df<-as.data.frame(county_v$GEOID) 
colnames(centroid_df)<-"GeoFIPS"
centroid_vect<-centroids(county_v)
lon<-rep(0,nrow(centroid_df))
lat<-rep(0,nrow(centroid_df))
for (i in 1:nrow(centroid_df)){
  lon[i]<-xmin(centroid_vect[i])
  lat[i]<-ymin(centroid_vect[i]) #xmin, xmax are the same for centroids
}
centroid_df<-cbind(centroid_df, lon, lat)

#Create a variable for west of the hundredth Meridian
centroid_df$west<-ifelse(centroid_df$lon< (-100),1,0)

main_df<-merge(main_df,centroid_df, by="GeoFIPS", all=TRUE)

#Make FIPS codes for average rainfall data
rain_data <- subset(read.csv("./data/raw/110-pcp-202112-1.csv", skip = 3), select=c("Location.ID", "X1901.2000.Mean"))
colnames(rain_data)[2]<-"baseline_rain"
postal_code_data<-read.csv("./data/raw/postal_code_to_FIPS.csv")
colnames(postal_code_data)[1] <- "state_name"
colnames(postal_code_data)[3] <- "state_FIPS"
rain_data$state<-substr(rain_data$Location.ID,start=1,stop=2)
rain_data$county_FIPS<-as.numeric(substr(rain_data$Location.ID,start=4,stop=7))
rain_data<-merge(rain_data, postal_code_data, by.x="state", by.y="Postal.Code", all=TRUE)
rain_data$FIPS<-1000*rain_data$state_FIPS+rain_data$county_FIPS
rain_data<-subset(rain_data, select=c("FIPS","baseline_rain"))
main_df<-merge(main_df, rain_data, by.x="GeoFIPS", by.y="FIPS", all=TRUE)

#Get aggregate employment data.
#Line code 190: wages + salaries
#Line code 240: total employment
#Line code 250: wages and salary employment
employment_data<-subset(read.csv("./data/raw/CAINC30__ALL_AREAS_1969_2020.csv"), 
                        LineCode  %in% c(190,240,250), 
                        select = -c(GeoName, Region,	TableName,	IndustryClassification,	Description,	Unit))
employment_data<-gather(employment_data, year, value, X1969:X2020, factor_key=FALSE)
employment_data$year <- as.numeric(substring(employment_data$year, 2))
employment_data<-spread(employment_data, key=LineCode, value=value)
colnames(employment_data)<-c("GeoFIPS", "year", "sum_wages", "total_employment", "wage_employment")
employment_data<- as.data.frame(lapply(employment_data,as.numeric))
#Make average wage. Note: in thousands
employment_data$mean_salary<-employment_data$sum_wages/employment_data$wage_employment 
main_df<-merge(main_df, employment_data, by=c("GeoFIPS", "year"), all=TRUE)

######################### ADDING USGS DATA #####################################

#add USGS dataframe
load("./data/intermediate/usgs.RData")
main_df<-merge(main_df, usgs, by=c("GeoFIPS", "year"), all=TRUE)


####################### GET WATER INTENSITY DATA ###############################
#water_intensity_df<-read_dta("./data/raw/water.dta")
#I will pull out several variables. di_win1: direct water intensity for blue
#water. It is the cost of water use divided by the value added + cost of water
#use, constructed by Debaere (2014) as an estimate for US 2002 water use.
#di_win1_grbl combines green and blue direct water intensity. hendrickson is a 
#variable that divides direct water use in 1000s of gallons by million dollars
#of output. Multiply by output to get number of gallons.
#water_intensity_df<-water_intensity_df %>%
#  filter(iso == "AFG") %>% #Note: this is a balanced panel and all variables I am pulling have the same value for each country
#  subset(select = c(io1997,industry_description,di_win1, di_win1_grbl, hendrickson))

################################################################################

#Create a variable for state FIPS in main_df so I can more easily split the data
#into states later.
main_df$state_FIPS<-floor(main_df$GeoFIPS /1000)

########################## EXPLORE DATA ########################################

#There are some missing observations in the dataframe. Remove if there is no
#geolocation given (this corresponds to no year as well)
main_df<-filter(main_df, GeoName != "<NA>")

#NOTE: This is a balanced panel except for Alaska, and 5 counties: 
#Cibola NM (NA til 1981), La Paz AZ (til 1982), Menominee WI (til 1988), Shawano WI (til 1988), Broomfield CO (til 2001)
main_df<-filter(main_df, state_FIPS != 2)
missings<-which(is.na(main_df$farm_income))
missings_mat<-main_df[missings,]

###################### CREATE A SECTOR-BY-SECTOR DATASET ############################
load("./data/intermediate/sector_df.RData")
main_sector_df<-left_join(main_df, sector_df, by = c("GeoFIPS", "year"))

################################################################################

#Save files
save(main_df, file = paste(int_folder, "main_df.RData", sep = "", collapse = NULL))
save(main_sector_df, file = paste(int_folder, "main_sector_df.RData", sep = "", collapse = NULL))
save(centroid_df, file=paste(int_folder, "centroid.RData", sep="", collapse = NULL))
writeVector(county_v, filename = paste(int_folder, "county_v.GTiff", sep=""))


