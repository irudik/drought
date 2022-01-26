#This code runs regressions with Conley standard errors

setwd("C:\\Users\\anton\\Dropbox\\drought")

library(tidyverse)
library(conleyreg)

load("./data/intermediate/main_df.RData")
load("./data/intermediate/drought_df.RData")

#Redefine drought-days to be positive
drought_df$drought_day<- (-1)*drought_df$drought_day

#Merge the main dataframe and drought dataframe
main_df<-merge(main_df, drought_df, by=c("year","GeoFIPS"))

#First: in total, run the regression of drought days on farm income, in total.
main_df$log_farm_income<-log(main_df$farm_income)
reg_df<-filter(main_df, !is.nan(log_farm_income)) #15102 are NaN, 2290 are infinite (of 159588)
reg_df<-filter(reg_df, !is.infinite(log_farm_income))

results<-conleyreg(
  log_farm_income ~ drought_day | year + state_FIPS,
  data = filter(reg_df, state_FIPS == 35),
  #data = reg_df,
  dist_cutoff = 2000,
  model = "ols",
  unit = "GeoFIPS",
  time = "year",
  lat = "lat",
  lon = "lon",
  lag_cutoff=4)
results

#Other variables are not infinite valued. Use entire dataset.
#Regression of drought days on nonfarm income, in total.
main_df$log_nonfarm_income<-log(main_df$nonfarm_income)
main_df$log_total_income<-log(main_df$total_income)

#Note: average drought-months in a California county is 2.22.

main_df$log_population<-log(main_df$population)

#interact baseline rainfall and drought days
main_df$interact<-(main_df$drought_day)*(main_df$baseline_rain)

#Below is the main specification. Change the dependent variable and whether we use
#interaction or drought_day. data_name refers to the column label when saving.

data_name="NM"

results<-conleyreg(
  log_total_employment~ drought_day | year + state_FIPS,
  data = filter(main_df, state_FIPS == 35),
  #data = filter(main_df, west == 0),
  #data = main_df,
  dist_cutoff = 2000,
  model = "ols",
  unit = "GeoFIPS",
  time = "year",
  lat = "lat",
  lon = "lon",
  lag_cutoff=4)
results

results_column<-matrix(c(results[1],results[2],results[4],data_name),ncol=1)
results_mat<-cbind(results_mat,results_column)
