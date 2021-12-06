#install.packages('SDMTools')
setwd("C:\\Users\\anton\\Dropbox\\drought")

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(rgdal)
library(leaflet)


spei_data <- brick('./data/raw/spei01.nc')

#To plot layer 1:
plot(spei_data,1)

#To extract layer 1:
layer1 = spei_data[[1]]

#Note that the latitude and longitude go from 0 to 360 and 0 to 720

