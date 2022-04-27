#This R file compiles industry data in the US.

#NOTE TO CHECK: I'm not sure if my crosswalk goes to NAICS 2012??
#NOTE TO CHECK: Check the county balance for aggregate water data (when I add up
#by county, is this actually a national total? And are the different measures comparable?)

setwd("C:\\Users\\anton\\Dropbox\\drought")

#GitHub file to save dataframes to:
int_folder = ".\\data\\intermediate\\"

library(tidyverse)
library(tidyr)
library(terra)
library(dplyr)
library(plyr)
library(readxl)

#In this code, I will make sectors often. Some sectors are made up of multiple
#two-digit NAICS codes. To streamline the process, I write code here to easily do 
#this computation. Note: it requires a variable called naics2 in the dataset.

#df is a dataframe already at naics2 level. formula is the expression
#of the values to be aggregated (e.g. emp~fips+year)
sector_from_naics2<- function(df, formula) {
  df_3133<-filter(df, naics2 %in% c(31,32,33))
  df_3133<-aggregate(formula, data = df_3133, FUN = sum, na.rm = FALSE)
  df_3133$naics2 = "31-33"
  df_4445<-filter(df, naics2 %in% c(44,45))
  df_4445<-aggregate(formula, data =df_4445, FUN = sum, na.rm = FALSE)
  df_4445$naics2 = "44-45"
  df_4849<-filter(df, naics2 %in% c(48,49))
  df_4849<-aggregate(formula, data = df_4849, FUN = sum, na.rm = FALSE)
  df_4849$naics2 = "48-49"
  df_sec<-filter(df, !naics2 %in% c(31,32,33,44,45,48,49))
  df_sec<-rbind(df_sec, df_3133, df_4445, df_4849)
  return(df_sec)
}

#Make a sector-level dataset

#1. Transform industries into sectors
#2. Aggregate employment by sector
#3. Probs just ditch v1 bc there's no documentation on it

#Import and aggregate employment data by sector.
emp_df<-read.csv("./data/raw/efsy_panel_naics.csv")
emp_df$naics2<-substring(emp_df$naics12,1,2)
emp_df$fips<-emp_df$fipstate*1000+emp_df$fipscty
emp_naics2_df<-aggregate(emp~fips+year+naics2, data=emp_df,FUN=sum, na.rm = FALSE)
#Use function to make sector-level aggregation
emp_sec_df<-sector_from_naics2(emp_naics2_df, formula(emp~fips+year))
#Create national employment by sector (Note: there are 3219 counties in the dataset,
#44 years, and 22 sectors. There are no empty observations. The panel is slightly
#unbalanced. Check later to make sure I am not missing huge numbers of people.)

#CODE TO AGGREGATE TO US:
#us_emp_sec<-aggregate(emp~naics2, data = emp_sec_df, FUN = sum)

#Import industry data; convert SIC codes to NAICS at the sector-level.

inc_69_00<-read.csv("./data/raw/CAINC5S__ALL_AREAS_1969_2000.csv")
inc_69_00<-gather(inc_69_00, year, value, X1969:X2000, factor_key=FALSE)
inc_69_00$value<-as.numeric(inc_69_00$value)
not_A<-filter(inc_69_00, IndustryClassification %in% c("B","C","D","E","F","G","H","I")) #Keep only sectors
not_A$year <- as.numeric(substring(not_A$year, 2))
not_A<-subset(not_A, select = c(GeoFIPS, IndustryClassification, year, value))

A<-filter(inc_69_00, IndustryClassification =="[01-02]" | IndustryClassification =="[07-09]") #SIC A is split into two groups; add these to inc
A<-aggregate(value~GeoFIPS+year, data = A, FUN=sum, na.rm = FALSE)
A$IndustryClassification<-"A"
A$year<-as.numeric(substring(A$year, 2))

inc_69_00<-rbind(not_A, A) #Combine this sector-level dataset
inc_69_00<-inc_69_00[order(inc_69_00$year,inc_69_00$GeoFIPS, inc_69_00$IndustryClassification),]

#Import SIC Crosswalk
crosswalk<-read.csv("./data/intermediate/SIC1_to_NAICS1.csv")
inc_69_00_sec<-merge(x=inc_69_00,y=crosswalk,by.x="IndustryClassification",by.y="SIC",all=TRUE)
#Since we are looking at income, and the crosswalk has establishment, employment
#and pay weights, I will multiply the value by the pay weights.
inc_69_00_sec$inc = inc_69_00_sec$value * inc_69_00_sec$Pay_weight
inc_69_00_sec = aggregate(inc ~ GeoFIPS + year + NAICS, data = inc_69_00_sec, FUN = sum, na.rm = FALSE)
colnames(inc_69_00_sec)<-c("GeoFIPS", "year", "naics2", "inc")
inc_69_00_sec$GeoFIPS=gsub(pattern = '\"| ', replace='', x = inc_69_00_sec$GeoFIPS)

#Import NAICS data for later years
inc_01_18<-read.csv("./data/raw/CAINC5N__ALL_AREAS_2001_2020.csv") 
inc_01_18<-gather(inc_01_18, year, value, X2001:X2020, factor_key=FALSE)
inc_01_18_noag<-inc_01_18 %>%
  filter(IndustryClassification %in% 
        c("21","22","23","31-33","42","44-45","48-49","51","52","53","54","55","56","61","62","71","72","81")) %>% #Keep only sectors
  subset(select = c(GeoFIPS, IndustryClassification, year, value))
inc_01_18_ag<- filter(inc_01_18, IndustryClassification %in% c("111-112","113-115")) #Keep only sectors
inc_01_18_ag$value<-as.numeric(inc_01_18_ag$value)
inc_01_18_ag<-aggregate(value ~ year + GeoFIPS, data = inc_01_18_ag,FUN = sum, na.rm = FALSE)
inc_01_18_ag$IndustryClassification = "11"
inc_01_18_sec<-rbind(inc_01_18_ag, inc_01_18_noag)
inc_01_18_sec$year = as.numeric(substring(inc_01_18_sec$year, 2))
colnames(inc_01_18_sec)<-c("year", "GeoFIPS", "inc", "naics2")
inc_01_18_sec$GeoFIPS<-gsub(pattern = ' ', replace = '', x = inc_01_18_sec$GeoFIPS)

inc_sec<-rbind(inc_69_00_sec, inc_01_18_sec)
inc_sec$GeoFIPS<-as.numeric(inc_sec$GeoFIPS)
sector_df<-merge(inc_sec, emp_sec_df, by.x = c("GeoFIPS", "year", "naics2"), by.y = c("fips", "year", "naics2"), all = TRUE)

#Create an employ


#Save county-level industrial information. 

save(sector_df, file = paste(int_folder, "sector_df.RData", sep = "", collapse = NULL))


