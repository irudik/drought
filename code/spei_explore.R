#install.packages('SDMTools')
setwd("C:\\Users\\anton\\Dropbox\\drought")

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(rgdal)
library(leaflet)


spei_data <- nc_open('./data/raw/spei01.nc')

#The following commands give metadata
{
  sink('spei01.txt')
  print(spei_data)
  sink()
}

lon <- ncvar_get(spei_data, "lon")
lat <- ncvar_get(spei_data, "lat")
time <-ncvar_get(spei_data, "time") #Maybe about once a month
spei.array <- ncvar_get(spei_data, "spei")

dim(spei.array) #Dims by lon, lat, time: 720 x 360 x 1416

fillvalue #There are missing values in the dataset, denoted 1e+30

nc_close(spei_data)

#Replace missing values with R's missing values
spei.array[spei.array == fillvalue$value] <- NA

#Get only the slice where time = 1
spei.slice <- spei.array[, , 3] 

#Save data in a raster:
r <- raster(t(spei.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
ry <- flip(r, direction='y')
plot(ry)

spei_value=spei.array[-100,50,1]
spei_value=spei.array[-100,50,2]



breakpoints <- c(0,1,2,3)
colors <- c("transparent","transparent","transparent","white")
plot(m,col=colors)

mask<-spTransform(x = mask, CRSobj="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
masked <- mask(lwe.slice, mask = mask)
