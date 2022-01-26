setwd("C:\\Users\\anton\\Dropbox\\drought")

library(tidyverse)
library(tidyr)
library(terra)
library(dplyr)
library(ggplot2)
library(fixest)


load("./data/intermediate/main_df.RData")
load("./data/intermediate/centroid.RData")
load("./data/intermediate/drought_df.RData")

#Redefine drought-days to be positive
drought_df$drought_day<- (-1)*drought_df$drought_day

#Merge the main dataframe and drought dataframe
main_df<-merge(main_df, drought_df, by=c("year","GeoFIPS"))

#Question 1: How persistent are drought-days?

main_df<-main_df[with(main_df, order(GeoFIPS,year)),]

main_df<-main_df %>%
  group_by(GeoFIPS) %>%
  dplyr::mutate(lag1 = dplyr::lag(drought_day,n=1)) %>%
  dplyr::mutate(lag2 = dplyr::lag(drought_day,n=2)) %>%
  dplyr::mutate(lag3 = dplyr::lag(drought_day,n=3)) %>%
  dplyr::mutate(lag4 = dplyr::lag(drought_day,n=4)) %>%
  dplyr::mutate(lag5 = dplyr::lag(drought_day,n=5)) %>%
  as.data.frame()


#NOTE: mutate adds new variables and preseves existing ones; transmute adds new 
#variables and drops existing ones.

#What is the autocorrelation of the lags?
res<-lm(drought_day~lag1, data=main_df)
summary(res) #lag1        0.170706   0.002508   68.06   <2e-16 ***

res<-lm(drought_day~lag2, data=main_df)
summary(res) #lag2 0.068192   0.002592   26.31   <2e-16 ***

res<-lm(drought_day~lag3, data=main_df)
summary(res) #lag3        0.039934   0.002635   15.16   <2e-16 ***

res<-lm(drought_day~lag4, data=main_df)
summary(res) #lag4        0.032611   0.002663   12.25   <2e-16 ***


res<-lm(drought_day~lag1+lag2+lag3+lag4, data=data=main_df)
summary(res)



#CONCLUSION: SPEI-months are autocorrelated. Having one SPEI-month in the year 
#before is corelated with 1/5 of an SPEI-month more in the following year. 

#Note that if drought days was random, there would not be any correlation found
#here. I did not force correlation (based on monte-carlo experiment)

# #TEST my variable to see if I forced correlation
# test<-rnorm(100,0,1)
# test<-ifelse(test>0,test,0)
# test<-as.data.frame(test) %>%
#   dplyr::mutate(lag1 = dplyr::lag(test,n=1,default=NA)) %>%
#   dplyr::mutate(lag2 = dplyr::lag(test,n=2,default=NA)) %>%
#   dplyr::mutate(lag3 = dplyr::lag(test,n=3,default=NA)) %>%
#   as.data.frame()

#Does drought autocorrelation change based on location in the US?

centroid_df$west<-ifelse(centroid_df$lon< (-100),1,0)
main_df<-merge(main_df, centroid_df, by.x = "GeoFIPS", by.y = "county_v$GEOID")
#lags<-merge(lags, centroid_df, by.x = "GeoFIPS", by.y = "county_v$GEOID")


#What is the autocorrelation of the lags, by being west of the 100th meridian?
res<-lm(drought_day~lag1, data=subset(main_df, west==1))
summary(res)

res<-lm(drought_day~lag1, data=subset(main_df, west==0))
summary(res)

#What is the autocorrelation of the lags, by being west of the 100th meridian?
res<-lm(drought_day~lag1, data=subset(main_df, west==1))
summary(res)

res<-lm(drought_day~lag1, data=subset(main_df, west==0))
summary(res)

#Interestingly, drought seems to be more autocorrelated, at least in the first
#lag, in the east.

################################################################################

#Check average drought days in the West vs east over the time period.

mean(main_df$drought_day) #2.554559
sd(main_df$drought_day) #2.257016

#Average SPEI-month by west and east
main_df %>%
  group_by(west) %>%
  summarise_at(vars(drought_day), list(name = mean))
#West has slightly more droughts. Note we don't see a huge change probably because
#SPEI is relative:   east average: 2.50 west average: 2.76

#standard deviation SPEI-month by west and east
main_df %>%
  group_by(west) %>%
  summarise_at(vars(drought_day), list(name = sd))
#West has greater variance: east:  2.22 west: 2.40

#Check average drought-days over time for east and west

dd_by_year<-main_df %>%
  group_by(year) %>%
  summarise_at(vars(drought_day), list(name = mean))

dd_by_year_west<-main_df %>%
  filter(west == 1, na.rm = TRUE) %>%
  group_by(year) %>%
  summarise_at(vars(drought_day), list(name = mean))
dd_by_year_west<-dd_by_year_west[1:50,]

dd_by_year_east<-main_df %>%
  filter(west == 0, na.rm = TRUE) %>%
  group_by(year) %>%
  summarise_at(vars(drought_day), list(name = mean))
dd_by_year_east<-dd_by_year_east[1:50,]


plot(dd_by_year_west, main="SPEI-months by year, Eastern and Western Counties",
     xlab="Year ", ylab="SPEI-months ", pch=19)
points(dd_by_year_east, col = "green", pch = 8)

res<-lm(name~year, data=dd_by_year_west)
summary(res)
res<-lm(name~year, data=dd_by_year_east)
summary(res)

#No apparent trends: average SPEI over year is just about zero.
plot(dd_by_year_west, main="SPEI-months by year, Eastern and Western Counties",
     xlab="Year ", ylab="SPEI-months ", pch=19)
points(dd_by_year_east, col = "green", pch = 8)
abline(lm(dd_by_year_west$name~dd_by_year_west$year), col="black")
abline(lm(dd_by_year_east$name~dd_by_year_east$year), col="green")
legend("topright",legend=c("West", "East"), 
       fill = c("black","green")
)

#Question: How does drought affect average farm and non-farm income?

#Partial out fixed effects
main_df$log_farmincome<-log(main_df$farm_income)
main_df$log_nonfarmincome<-log(main_df$nonfarm_income)


main_df[,c("drought_day_demean","farm_income_demean", "nonfarm_income_demean")] = 
  demean(X = main_df[,c("drought_day","log_farmincome","log_nonfarmincome")],
         f = main_df[, c("GeoFIPS", "year")],na.rm = FALSE)

#Make binscatters by grouping data by deciles.
total_ddd_decile<-main_df %>%
  mutate(decile = ntile(drought_day_demean, 10)) %>%
  group_by(decile) %>% 
  summarise(across(drought_day_demean:nonfarm_income_demean, mean))

plot(total_ddd_decile$drought_day_demean, total_ddd_decile$farm_income_demean,
  main="Farm Income by drought days: deciles, all counties",
  ylab="Demeaned farm income, log millions",
  xlab="Demeaned drought months",
  pch=19)
reg_model<-lm(farm_income_demean~drought_day_demean, data=total_ddd_decile)
reg_model
abline(reg_model, col="black")

plot(total_ddd_decile$drought_day_demean, total_ddd_decile$nonfarm_income_demean,
     main="Non-farm Income by drought days: deciles, all counties",
     ylab="Demeaned nonfarm income, log millions",
     xlab="Demeaned drought months",
     pch=19)
reg_model<-lm(nonfarm_income_demean~drought_day_demean, data=total_ddd_decile)
reg_model
abline(reg_model, col="black")

west_ddd_decile<-main_df %>%
  filter(west == 1) %>%
  mutate(decile = ntile(drought_day_demean, 10)) %>%
  group_by(decile) %>% 
  summarise(across(drought_day_demean:nonfarm_income_demean, mean))

east_ddd_decile<-main_df %>%
  filter(west == 0) %>%
  mutate(decile = ntile(drought_day_demean, 10)) %>%
  group_by(decile) %>% 
  summarise(across(drought_day_demean:nonfarm_income_demean, mean))

plot(west_ddd_decile$drought_day_demean, west_ddd_decile$farm_income_demean,
     main="Farm Income by drought days: deciles, Western counties",
     ylab="Demeaned farm income, log millions",
     xlab="Demeaned drought months",
     pch=19)
reg_model<-lm(farm_income_demean~drought_day_demean, data=west_ddd_decile)
reg_model
abline(reg_model, col="black")

plot(west_ddd_decile$drought_day_demean, west_ddd_decile$nonfarm_income_demean,
     main="Non-farm Income by drought days: deciles, Western counties",
     ylab="Demeaned nonfarm income, log millions",
     xlab="Demeaned drought months",
     pch=19)
reg_model<-lm(nonfarm_income_demean~drought_day_demean, data=west_ddd_decile)
reg_model
abline(reg_model, col="black")

plot(east_ddd_decile$drought_day_demean, east_ddd_decile$farm_income_demean,
     main="Farm Income by drought days: deciles, Eastern counties",
     ylab="Demeaned farm income, log millions",
     xlab="Demeaned drought months",
     pch=19)
reg_model<-lm(farm_income_demean~drought_day_demean, data=east_ddd_decile)
reg_model
abline(reg_model, col="black")

plot(east_ddd_decile$drought_day_demean, east_ddd_decile$nonfarm_income_demean,
     main="Non-farm Income by drought days: deciles, Eastern counties",
     ylab="Demeaned nonfarm income, log millions",
     xlab="Demeaned drought months",
     pch=19)
reg_model<-lm(nonfarm_income_demean~drought_day_demean, data=east_ddd_decile)
reg_model
abline(reg_model, col="black")


#Run the first regression: how does farm income change with SPEI-months?

results<-fixest::feols(log_farmincome~drought_day|year+GeoFIPS, main_df, 
                       cluster=~GeoFIPS)

summary(results)

results<-fixest::feols(log_nonfarmincome~drought_day|year+GeoFIPS, main_df, 
                       cluster=~GeoFIPS)

summary(results)

#How does total income change with SPEI-months?
main_df$log_income<-log(main_df$income)

results<-fixest::feols(log_income~drought_day|year+GeoFIPS, main_df, 
                       cluster=~GeoFIPS)

summary(results)

#Run the second regression: how does farm income change with lagged SPEI-months?

results<-fixest::feols(log_farmincome~lag1|year+GeoFIPS, main_df, 
                       cluster=~GeoFIPS)

summary(results)

results<-fixest::feols(log_farmincome~drought_day|year+GeoFIPS, data=subset(main_df, west==1), 
                       cluster=~GeoFIPS)
summary(results)

results<-fixest::feols(log_farmincome~drought_day|year+GeoFIPS, data=subset(main_df, west==0), 
                       cluster=~GeoFIPS)
summary(results)


results<-fixest::feols(log_nonfarmincome~lag1|year+GeoFIPS, main_df, 
                       cluster=~GeoFIPS)

summary(results)


results<-fixest::feols(log_nonfarmincome~drought_day|year+GeoFIPS, data=subset(main_df, west==1), 
                       cluster=~GeoFIPS)

summary(results)

results<-fixest::feols(log_nonfarmincome~drought_day|year+GeoFIPS, data=subset(main_df, west==0), 
                       cluster=~GeoFIPS)

summary(results)


