####
## This script takes the shellfish water quality data for zone WQ
## from Geoffrey Shook (see umaine.edu email - DMR) and merges it together
## with the locations CSV (also from him) AND the data that FromKate for
## Damariscotta (WQTimeseries.dbf). It also drops columns that are
## not necessary. 
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

Zone<-"WQ"


## Locations for Water Quality Zone WQ (Damariscotta) from Geoff
WQData_Loc<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/From_Geoff/GA_WQ_Locs.csv")
# Remove NA Lats and Longs
WQData_Loc<-WQData_Loc[complete.cases(WQData_Loc[ , c("LATITUDE_DECIMAL","LONGITUDE_DECIMAL")]),]
# drop columns with only NA values
WQData_Loc<-WQData_Loc[,colSums(is.na(WQData_Loc))<nrow(WQData_Loc)]

WQData=read.dbf("01_Original/Aquaculture/FromKate/Damariscotta/CSVs/WQTimeseries.dbf")
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

names(WQData)

colnames(WQData)[colnames(WQData)=="LOCATION_I"] <- "Station";colnames(WQData)[colnames(WQData)=="TRIP_START"] <- "Date"
colnames(WQData)[colnames(WQData)=="TEMP_C"] <- "TempC";colnames(WQData)[colnames(WQData)=="COL_SCORE"] <- "Score"
colnames(WQData)[colnames(WQData)=="RAW_COL_SC"] <- "RawColScore";colnames(WQData)[colnames(WQData)=="SALINITY_P"] <- "Sal"
colnames(WQData)[colnames(WQData)=="TIDE_STAGE"] <- "Tide"

WQData<-WQData[,c("Station","Date","Tide","TempC","Sal","Score","RawColScore")]
WQData$Rain24Hrs<-NA
WQData$Rain72Hrs<-NA
WQData$RainSta<-NA

#sum(is.na(WQData$Date[(WQData$Station=="WQ039.00")]))

WQData$YrMo<-format(as.Date(WQData$Date, "%Y-%m-%d"),"%Y-%m")
WQData$Date<-format(as.Date(WQData$Date, "%Y-%m-%d"),"%d-%b-%y")
#sum(is.na(WQData$Date[(WQData$Station=="WQ039.00")]))

## new DMR WQ data from Geoff
WQData_New<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/From_Geoff/GA_WQ_Data.csv")
colnames(WQData_New)[colnames(WQData_New)=="Temp"] <- "TempC"
WQData_New<-WQData_New[,c("Station","Date","Tide","TempC","Sal","Score","RawColScore","Rain24Hrs","Rain72Hrs","RainSta")]

## grab minimum date from new dataset so that the data doesn't overlap when they are merged
WQData_New$YrMo<-format(as.Date(WQData_New$Date, "%d-%b-%y"),"%Y-%m")
MinNewDate<-min(WQData_New$YrMo)
WQData<-WQData[WQData$YrMo<MinNewDate,]


## Merge WQData and New data with Locations; drop non-matching stations
WQData<-merge(WQData,WQData_Loc, by.x="Station", by.y="LOCATION_ID",all=FALSE)
WQData_New<-merge(WQData_New,WQData_Loc, by.x="Station", by.y="LOCATION_ID",all=FALSE)

#merge by row
WQData_Update <- rbind(WQData,WQData_New)


# Remove NA TempC and Sal values
## sometimes they put in TempC NA's as 99.9 :|
Num99TempC<-nrow(WQData_Update[WQData_Update$TempC>=99,])
print(paste("Number of TempC values == 99.9 removed: ", Num99TempC,sep=""))
WQData_Update<-WQData_Update[!WQData_Update$TempC>=99,]
## sometimes they put in TempC NA's as 99.9 OR 99 :|
Num99Sal<-nrow(WQData_Update[WQData_Update$Sal>=99,])
print(paste("Number of Sal values == 99.9 removed: ", Num99Sal,sep=""))
WQData_Update<-WQData_Update[!WQData_Update$Sal>=99,]


## if they're not, then just drop lat/lon from the dataset that are 0
NumZeroCoords<-nrow(WQData_Update[(WQData_Update$LATITUDE_DECIMAL==0 & WQData_Update$LONGITUDE_DECIMAL==0),])
print(paste("Number of rows removed with (0,0) as (lat,long): ",NumZeroCoords,sep=""))
WQData_Update<-WQData_Update[!(WQData_Update$LATITUDE_DECIMAL==0 & WQData_Update$LONGITUDE_DECIMAL==0),]

WQData_Update<-WQData_Update[!is.na(WQData_Update$Station),]
WQData_Update$Date<-format(as.Date(WQData_Update$Date, "%d-%b-%y"), "%Y-%m-%d")
WQData_Update<-RemoveDuplicates(WQData_Update)
WQData_Update<-WQData_Update[order(WQData_Update$Date),]
rownames(WQData_Update) <- seq(length=nrow(WQData_Update))

#update IDs
WQData_Update$ID <- 1:nrow(WQData_Update)
WQData_Update<-WQData_Update[,c("ID","Station","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Date","YrMo","Tide","TempC","Sal","Score","RawColScore","Rain24Hrs","Rain72Hrs","RainSta")]
WQData_Update$Zone<-Zone
write.csv(WQData_Update,"02_Working/WaterQuality/State_DMR/From_Geoff/WQData_Update.csv",row.names=FALSE)
