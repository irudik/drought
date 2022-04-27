# Clear working directory
rm(list = ls())

#################### User Defined Inputs #######################################
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
library(magick)

##Files to load (NOTE FOR FUTURE: THIS FILE COMES BEFORE IT IS CREATED IN THE CODE!!!)
e <- ext(-130,-60,20,50)
county_v<-vect("./data/intermediate/ county_v.GTiff", extent = e)

#Initialize precip dataframe, 
precip_df<- data.frame(matrix(ncol = 5, nrow = 0))
colnames(precip_df) <- c('GEOID', 'precip','month','year')

#This loop aggregates precipitation over a county. In the future, I may be able 
#to do a more localized analysis.
for (year in 1981:2021){
  for (month in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    filename<-paste0("PRISM_ppt_stable_4kmM3_",year,month,"_bil.bil")
    #Get precip data
    precip <- rast(paste0("./data/raw/precip/PRISM_ppt_stable_4kmM3_198101_202109_bil/",filename))
    #Note: both datasets use NAD83; no need to project.
    #precip_project<-project(precip, county_v)
    bymonth = extract(precip,county_v, mean, method = "simple", weights = TRUE) 
    #Note: ID variable represents the order of the observations in the points data
    id_frame<-data.frame(ID = 1:length(county_v), GEOID = county_v$GEOID)
    precip_by_month<-merge(id_frame, bymonth, by = "ID")
    colnames(precip_by_month)[3] <- "precip"
    precip_by_month<-precip_by_month %>%
      mutate(month = month, year = year) %>%
      subset(select = -c(ID))
    precip_df<-rbind(precip_df, precip_by_month)
  }
  print(year)
}

precip_by_year<-aggregate(precip~GEOID + year, data = precip_df, FUN = sum)

#save(precip_by_year, file = paste(int_folder, "precip_by_year.RData", sep = "", collapse = NULL))
#save(precip_df, file = paste(int_folder, "precip_df.RData", sep = "", collapse = NULL))

##Add older precipitation data

#This loop aggregates precipitation over a county. In the future, I may be able 
#to do a more localized analysis.
load(paste(int_folder,"precip_df.RData", sep = ""))

for (y in 1969:1980){
  for (m in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    filename<-paste0("PRISM_ppt_stable_4kmM2_",y,"_all_bil/PRISM_ppt_stable_4kmM2_",y,month,"_bil.bil")
    #Get precip data
    precip <- rast(paste0("./data/raw/precip/",filename))
    #Note: both datasets use NAD83; no need to project.
    #precip_project<-project(precip, county_v)
    bymonth = extract(precip,county_v, mean, method = "simple", weights = TRUE) 
    #Note: ID variable represents the order of the observations in the points data
    id_frame<-data.frame(ID = 1:length(county_v), GEOID = county_v$GEOID)
    precip_by_month<-merge(id_frame, bymonth, by = "ID")
    colnames(precip_by_month)[3] <- "precip"
    precip_by_month<-precip_by_month %>%
      mutate(month = m, year = y) %>%
      subset(select = -c(ID))
    precip_df<-rbind(precip_df, precip_by_month)
  }
  print(y)
}

########### MAKES A GIF ########################################################

for (y in 1981:2020){
  to_plot<-merge(county_v, filter(precip_by_year, year == y), by = "GEOID", all = TRUE)
  png(paste0("./output/precip_gif/plot",y,".png"), width = 720, height = 475)
  plot(to_plot, "precip", type="interval", breaks = c(0,250,635,990,1750,10000),
                 main = paste0("Annual Precipitation in ", y), col = c("red4","orange","yellow","lightskyblue1","navy"))
  dev.off()
}

for (y in 1981:2020){
  img<-image_read(paste0("./output/precip_gif/plot",y,".png"))
  assign(paste0("img",y),img)
}

img_group<-c(img1981,img1982,img1983,img1984,img1985,img1986,img1987,img1988,
             img1989,img1990,img1991,img1992,img1993,img1994,img1995,img1996,
             img1997,img1998,img1999,img2000,img2001,img2002,img2003,img2004,
             img2005,img2006,img2007,img2008,img2009,img2010,img2011,img2012,
              img2013,img2014,img2015,img2016,img2017,img2018,img2019,img2020)
my.animation<-image_animate(image_scale(img_group,"720x475"),fps=1,dispose = "previous")

