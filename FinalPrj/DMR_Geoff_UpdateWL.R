####
## This script takes the shellfish water quality data for zone WL 
## from Geoffrey Shook (see umaine.edu email - DMR) and merges it together
## with the locations CSV (also from him). It also drops columns that are
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

Zone<-"WL"

WLData_New<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/From_Geoff/GA_WL_Data.csv")
WLData_Loc<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/From_Geoff/GA_WL_Locs.csv")

# Remove NA Lats and Longs
WLData_Loc<-WLData_Loc[complete.cases(WLData_Loc[ , c("LATITUDE_DECIMAL","LONGITUDE_DECIMAL")]),]
# drop columns with only NA values
WLData_Loc<-WLData_Loc[,colSums(is.na(WLData_Loc))<nrow(WLData_Loc)]

## rename Temp to TempC
colnames(WLData_New)[colnames(WLData_New)=="Temp"] <- "TempC"
# Remove NA TempC and Sal values
## sometimes they put in TempC NA's as 99.9 :|
Num99TempC<-nrow(WLData_New[WLData_New$TempC>=99,])
print(paste("Number of TempC values == 99.9 removed: ", Num99TempC,sep=""))
WLData_New<-WLData_New[!WLData_New$TempC>=99,]
## sometimes they put in TempC NA's as 99.9 OR 99 :|
Num99Sal<-nrow(WLData_New[WLData_New$Sal>=99,])
print(paste("Number of Sal values == 99.9 removed: ", Num99Sal,sep=""))
WLData_New<-WLData_New[!WLData_New$Sal>=99,]

#merge by row - if there is not a matching Station and LOCATION_ID, they are dropped from the emrged dataset (all=FALSE)
WLData_NewMer<-merge(WLData_New,WLData_Loc, by.x="Station", by.y="LOCATION_ID",all=FALSE)
## subset columns
WLData_NewMer<-WLData_NewMer[,c("Station","Date","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Tide","TempC","Sal","Score","RawColScore","Rain24Hrs","Rain72Hrs","RainSta")]
## add year-mo column
WLData_NewMer$YrMo<-format(as.Date(WLData_NewMer$Date, "%d-%b-%y"),"%Y-%m")
#sum(is.na(WLData_NewMer$Date[(WLData_NewMer$Station=="WL039.00")]))
sum(is.na(WLData_NewMer$Date))
#WLData_NewMer<-WLData_NewMer[WLData_NewMer$YrMo>="2014-01",]


WLData_Update <- WLData_NewMer
## if they're not, then just drop lat/lon from the dataset that are 0
NumZeroCoords<-nrow(WLData_Update[(WLData_Update$LATITUDE_DECIMAL==0 & WLData_Update$LONGITUDE_DECIMAL==0),])
print(paste("Number of rows removed with (0,0) as (lat,long): ",NumZeroCoords,sep=""))
WLData_Update<-WLData_Update[!(WLData_Update$LATITUDE_DECIMAL==0 & WLData_Update$LONGITUDE_DECIMAL==0),]


WLData_Update<-WLData_Update[!is.na(WLData_Update$Station),]
WLData_Update$Date<-format(as.Date(WLData_Update$Date, "%d-%b-%y"), "%Y-%m-%d")
WLData_Update<-RemoveDuplicates(WLData_Update)
WLData_Update<-WLData_Update[order(WLData_Update$Date),]
rownames(WLData_Update) <- seq(length=nrow(WLData_Update))

#update IDs
WLData_Update$ID <- 1:nrow(WLData_Update)
WLData_Update<-WLData_Update[,c("ID","Station","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Date","YrMo","Tide","TempC","Sal","Score","RawColScore","Rain24Hrs","Rain72Hrs","RainSta")]
WLData_Update$Zone<-Zone
write.csv(WLData_Update,"02_Working/WaterQuality/State_DMR/From_Geoff/WLData_Update.csv",row.names=FALSE)
