#This code runs regressions with Conley standard errors. It outputs latex-style
#tables for the dependent variables and data subsets that the user specifies in
#the beginning of this code.

setwd("C:\\Users\\anton\\Dropbox\\drought")

#Where to put finished tables
output_folder <- "C:\\Users\\anton\\Dropbox\\drought\\output"

#User defined inputs.
dependent_vars<-c("log_nonfarm_income", "log_total_income") #Any dependent variable in main_df or created in this file
states_of_interest<-c(6,4) #state fips codes
subsets_of_interest<-c("all","west","east",states_of_interest) #options: all, east, west, state fips (entered above)

################################################################################

library(tidyverse)
library(conleyreg)
library(xtable)

load("./data/intermediate/main_df.RData")
load("./data/intermediate/drought_df.RData")


#Redefine drought-days to be positive
drought_df$drought_day<- (-1)*drought_df$drought_day

#Merge the main dataframe and drought dataframe
main_df<-merge(main_df, drought_df, by=c("year","GeoFIPS"))

#First: in total, run the regression of drought days on farm income, in total.
#Run it on log(farm_income + 1)
main_df$farm_income<-main_df$farm_income+1
# main_df$log_farm_income<-log(main_df$farm_income)
# reg_df<-filter(main_df, !is.nan(log_farm_income)) #8656 are Na, 8293 are nan, 10 are inf (of 159588)
# reg_df<-filter(reg_df, !is.infinite(log_farm_income))


# reg_df<-filter(main_df, !is.nan(log_farm_income)) #15465 are NA, 15102 are NAN, 2290 are infinite (of 159588)
# reg_df<-filter(reg_df, !is.infinite(log_farm_income))

#Note: average drought-months in a California county is 2.22.

#Take the log of dependent variables to run in the regression.
main_df$log_population<-log(main_df$population)
main_df$log_nonfarm_income<-log(main_df$nonfarm_income)
main_df$log_total_income<-log(main_df$total_income)
main_df$log_total_employment<-log(main_df$total_employment)
main_df$log_farm_income<-log(main_df$farm_income) 

#Construct the mean wage variable and take the log
main_df$mean_wage<-main_df$mean_salary/main_df$wage_employment
main_df$log_mean_wage<-log(main_df$mean_wage)

#interact baseline rainfall and drought days
main_df$interact<-(main_df$drought_day)*(main_df$baseline_rain)


#Below is the main specification. Change the dependent variable and whether we use
#interaction or drought_day. data_name refers to the column label when saving.

for (var in dependent_vars) {
  counter = 1
  for (subset in subsets_of_interest){
    data_name <- subset
    reg<- paste(var,"~drought_day + interact | year + GeoFIPS")
    #This chunk determines which subset of data to use
    if (subset == "west") {
      data<-filter(main_df, west == 1)
    } else if (subset == "east") {
      data<-filter(main_df, west == 0)
    } else if (!is.na(as.numeric(subset))) {
      data<-filter(main_df, state_FIPS == as.numeric(subset))
    } else {
      data<-main_df
    }
    #This chunk runs the regression
    results<-conleyreg(
      reg,
      data = data,
      dist_cutoff = 2000,
      model = "ols",
      unit = "GeoFIPS",
      time = "year",
      lat = "lat",
      lon = "lon",
      lag_cutoff=4)
    #This chunk saves the results from one regression
    results_column<-matrix(c(results[1,1],results[1,2],results[1,4],results[2,1],results[2,2],results[2,4]),ncol=1)
    #This chunk puts the results from one regression into a dataframe for that dependent variable.
    if (counter == 1){
      results_mat<-results_column
    } else {
      results_mat<-cbind(results_mat,results_column)
    }
    counter=counter+1
  }
  filename<-paste(output_folder,"\\",var,".tex",sep="")
  colnames(results_mat)<-subsets_of_interest
  info_cols<-paste(replicate(length(subsets_of_interest), "c"), collapse = "") #This row just says how many columns of information there are to make the latex table
  align<-paste("l|",info_cols,sep="")
  full_table<-xtable(results_mat, align = align, digits=4)
  print(full_table, type="latex", file=filename)
}
