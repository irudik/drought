setwd("C:\\Users\\anton\\Dropbox\\drought")

library(tidyverse)
library(tidyr)
library(terra)
library(dplyr)
library(fixest) #Note: Description Fast and user-friendly estimation of 
                #      econometric models with multiple fixed-effects. 

#Open data
load("./data/intermediate/drought_df.RData")

results<-fixest::feols(log(income)~drought_days|year+GEOFips, main_df, 
                       vcov="cluster", cluster("county"))



