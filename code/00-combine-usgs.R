#This R file combines USGS water use data into one sheet

setwd("C:\\Users\\anton\\Dropbox\\drought")

#GitHub file to save dataframes to:
int_folder = ".\\data\\intermediate\\"

library(tidyverse)
library(tidyr)
library(dplyr)
library(plyr)

################################################################################

#There are no missing values for public supply.
usgs<-data.frame(matrix(ncol = 26))
colnames<-c("GeoFIPS", "year", "PS.WGWFr", "PS.WSWFr","PS.WFrTo", "DO.WGWFr", "DO.WSWFr", "DO.WFrTo","IN.WGWFr", 
            "IN.WSWFr", "IN.WFrTo", "PT.WGWFr", "PT.WSWFr", "PT.WFrTo","MI.WGWFr", "MI.WSWFr", "MI.WFrTo",
            "LS.WGWFr", "LS.WSWFr", "LS.WFrTo", "IR.WGWFr", "IR.WSWFr","IR.WFrTo", "TO.WGWFr", "TO.WSWFr","TO.WFrTo")
colnames(usgs)<-colnames

for (year in c(1985,1990,1995, 2000,2005,2010,2015)){
  print(year)
  filename<-paste("us",substring(year,3,4),"co.txt",sep="")
  df<-read.delim(paste(".\\data\\raw\\USGS\\",filename,sep=""), header=TRUE, sep="\t")
  if (year %in% c(1985,1990)) {
    df$GeoFIPS<-df$scode*1000+df$area
  }else if (year == 1995) {
    df$GeoFIPS<-df$StateCode*1000+df$CountyCode
  } else if (year %in% c(2000,2005,2010,2015)){
    df$GeoFIPS<-df$STATEFIPS*1000+df$COUNTYFIPS
  }
  df$year<-year
  mini<-data.frame(cbind("GeoFIPS" = df$GeoFIPS, "year" = df$year , 
                         "PS.WGWFr" = df$ps.wgwfr, "PS.WSWFr" = df$ps.wswfr, "PS.WFrTo" = df$ps.wtofr, #Year = 1985
                         "DO.WGWFr" = df$do.ssgwf, "DO.WSWFr" = df$do.ssswf, "DO.WFrTo" = df$do.total,
                         "IN.WGWFr" = df$in.wgwfr, "IN.WSWFr" = df$in.wswfr, "IN.WFrTo" = df$in.wtofr,
                         "PT.WGWFr" = df$pt.wgwfr, "PT.WSWFr" = df$pt.wswfr, "PT.WFrTo" = df$pt.frtot,
                         "MI.WGWFr" = df$mi.wgwfr, "MI.WSWFr" = df$mi.wswfr, "MI.WFrTo" = df$mi.frtot,
                         "LS.WGWFr" = df$ls.gwtot, "LS.WSWFr" = df$ls.swtot, "LS.WFrTo" = df$ls.total,
                         "IR.WGWFr" = df$ir.wgwfr, "IR.WSWFr" = df$ir.wswfr, "IR.WFrTo" = df$ir.frtot,
                         "TO.WGWFr" = df$to.gwfr, "TO.WSWFr" = df$to.swfr, "TO.WFrTo" = df$to.frtot,
                         "PS.WGWFr" = df$PS.WGWFr, "PS.WSWFr" = df$PS.WSWFr, "PS.WFrTo" = df$PS.WFrTo,
                         "DO.WGWFr" = df$DO.WGWFr, "DO.WSWFr" = df$DO.WSWFr, "DO.WFrTo" = df$DO.WFrTo,
                         "IN.WGWFr" = df$IN.WGWFr, "IN.WSWFr" = df$IN.WSWFr, "IN.WFrTo" = df$IN.WFrTo,
                         "PT.WGWFr" = df$PT.WGWFr, "PT.WSWFr" = df$PT.WSWFr, "PT.WFrTo" = df$PT.WFrTo,
                         "MI.WGWFr" = df$MI.WGWFr, "MI.WSWFr" = df$MI.WSWFr, "MI.WFrTo" = df$MI.WFrTo,
                         "LS.WGWFr" = df$LS.WGWFr, "LS.WSWFr" = df$LS.WSWFr, "LS.WFrTo" = df$LS.WFrTo,
                         "LS.WGWFr" = df$LI.WGWFr, "LS.WSWFr" = df$LI.WSWFr, "LS.WFrTo" = df$LI.WFrTo,
                         "IR.WGWFr" = df$IR.WGWFr, "IR.WSWFr" = df$IT.WGWFr, "IR.WFrTo" = df$IR.WFrTo,
                         "IR.WGWFr" = df$IT.WGWFr, "IR.WSWFr" = df$IR.WSWFr, "IR.WFrTo" = df$IT.WFrTo,
                         "TO.WGWFr" = df$TO.WGWFr, "TO.WSWFr" = df$TO.WSWFr, "TO.WFrTo" = df$TO.WFrTo))
  usgs<-bind_rows(usgs, mini)
}

save(usgs, file = paste(int_folder, "usgs.RData", sep = "", collapse = NULL))
