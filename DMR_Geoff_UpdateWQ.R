####
## This script takes the shellfish water quality data for zone WQ
## from Geoffrey Shook (see umaine.edu email - DMR) and merges it together
## with the locations CSV (also from him) AND the data that FromKate for
## Damariscotta (WQTimeseries.dbf). It also drops columns that are
## not necessary. 
##
## I only wanted Salinity, Temp, and Fecal Coliform. 
## Dataset also has several other variables, so may want to tweak in the future
## to include stuff like currents. 
##
## This data only goes as far back as 2014. I emailed Geoff and asked for an updated
## dataset for WQ that includes all data (going back to 1990).
##
## CREATED BY: Melissa Kimble
## LAST MODIFIED: 04/28/2019
###

LibraryList<-c("foreign","spdep","RColorBrewer","classInt","maptools","rgdal","spatstat","plyr")
for (TheLibrary in LibraryList)
{
  if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
}
library(foreign)
library(spdep)
library(RColorBrewer)
library(classInt)
library(maptools)
library(rgdal)
library(spatstat)
library(plyr)

TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))

setwd("D:/Dropbox/00_Dissertation/01_Data/")

WQData=read.dbf("01_Original/Aquaculture/FromKate/Damariscotta/WQTimeseries.dbf")
#sum(is.na(WQData$INITIATED1[(WQData$LOCATION_I=="WQ039.00")]))
#sum(is.na(WQData$INITIATED1))

## INITIATED_ is supposed to be the initials of the technicians/volunteers, but in some places
## this data has been transposed with INITIATED1, which is supposed to be the sampling date.
## So this line selects all NA's from INITIATED1 and adds the date from INITIATED_
#WQData$INITIATED1[is.na(WQData$INITIATED1)]<-format(as.Date(WQData$INITIATED_[is.na(WQData$INITIATED1)], "%m/%d/%Y"),"%Y-%m-%d")
#sum(is.na(WQData$INITIATED1))
## as it turns out, INITIATED1 is NOT the correct date to use. When I compared it to the dataset that was emailed
## to me from DMR, the date they used is equal to TRIP_START, NOT INITIATED1
sum(is.na(WQData$TRIP_START))

#names(WQData)
#head(WQData)
colnames(WQData)[colnames(WQData)=="LOCATION_I"] <- "Station";colnames(WQData)[colnames(WQData)=="TRIP_START"] <- "Date"
colnames(WQData)[colnames(WQData)=="TEMP_C"] <- "Temp";colnames(WQData)[colnames(WQData)=="LATITUDE_D"] <- "LATITUDE_DECIMAL"
colnames(WQData)[colnames(WQData)=="LONGITUDE_"] <- "LONGITUDE_DECIMAL";colnames(WQData)[colnames(WQData)=="COL_SCORE"] <- "Score"
colnames(WQData)[colnames(WQData)=="RAW_COL_SC"] <- "RawColScore";colnames(WQData)[colnames(WQData)=="SALINITY_P"] <- "Sal"
WQData<-WQData[,c("Station","Date","Temp","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Score","RawColScore","Sal")]
#sum(is.na(WQData$Date[(WQData$Station=="WQ039.00")]))

WQData$YrMo<-format(as.Date(WQData$Date, "%Y-%m-%d"),"%Y-%m")
WQData$Date<-format(as.Date(WQData$Date, "%Y-%m-%d"),"%d-%b-%y")
#sum(is.na(WQData$Date[(WQData$Station=="WQ039.00")]))
WQData<-WQData[WQData$YrMo<"2014-01",]

WQData_New<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/GA_WQ_Data.csv")
WQData_Loc<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/GA_WQ_Locs.csv")

WQData_NewMer<-merge(WQData_New,WQData_Loc, by.x="Station", by.y="LOCATION_ID",all=FALSE)
#names(WQData_NewMer)
#head(WQData_NewMer)
WQData_NewMer<-WQData_NewMer[,c("Station","Date","Temp","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Score","RawColScore","Sal")]
WQData_NewMer$YrMo<-format(as.Date(WQData_NewMer$Date, "%d-%b-%y"),"%Y-%m")
#sum(is.na(WQData_NewMer$Date[(WQData_NewMer$Station=="WQ039.00")]))
sum(is.na(WQData_NewMer$Date))
WQData_NewMer<-WQData_NewMer[WQData_NewMer$YrMo>="2014-01",]

#merge by row
WQData_Update <- rbind(WQData,WQData_NewMer)
## if they're not, then just drop lat/lon from the dataset that are 0
NumZeroCoords<-nrow(WQData_Update[(WQData_Update$LATITUDE_DECIMAL==0 & WQData_Update$LONGITUDE_DECIMAL==0),])
print(paste("Number of rows removed with (0,0) as (lat,long): ",NumZeroCoords,sep=""))
WQData_Update<-WQData_Update[!(WQData_Update$LATITUDE_DECIMAL==0 & WQData_Update$LONGITUDE_DECIMAL==0),]
## sometimes they put in temp NA's as 99.9 :|
Num99Temp<-nrow(WQData_Update[WQData_Update$Temp==99.9,])
print(paste("Number of Temp values == 99.9 removed: ", Num99Temp,sep=""))
WQData_Update<-WQData_Update[!WQData_Update$Temp==99.9,]
WQData_Update<-WQData_Update[!is.na(WQData_Update$Station),]
WQData_Update$Date<-format(as.Date(WQData_Update$Date, "%d-%b-%y"), "%Y-%m-%d")
WQData_Update<-RemoveDuplicates(WQData_Update)
WQData_Update<-WQData_Update[order(WQData_Update$Date),]
rownames(WQData_Update) <- seq(length=nrow(WQData_Update))

#update IDs
WQData_Update$ID <- 1:nrow(WQData_Update)

write.csv(WQData_Update,"02_Working/WaterQuality/State_DMR/WQData_Update.csv",row.names=FALSE)
