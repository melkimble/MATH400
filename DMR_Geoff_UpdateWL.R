####
## This script takes the shellfish water quality data for zone WL 
## from Geoffrey Shook (see umaine.edu email - DMR) and merges it together
## with the locations CSV (also from him). It also drops columns that are
## not necessary.  
##
## I only wanted Salinity, Temp, and Fecal Coliform. 
## Dataset also has several other variables, so may want to tweak in the future
## to include stuff like currents. 
## 
## The WQ update merges data FromKate, but I didn't have a WL dbf file from her.
## Which is fine, because the WL data sent from Geoff includes ALL of the data
## for zone WL (back to 1990).
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

WLData_New<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/GA_WL_Data.csv")
WLData_Loc<-read.csv("01_Original/WaterQuality/State_DMR/ShellfishMonitoring/GA_WL_Locs.csv")

WLData_NewMer<-merge(WLData_New,WLData_Loc, by.x="Station", by.y="LOCATION_ID",all=FALSE)
#names(WLData_NewMer)
#head(WLData_NewMer)
WLData_NewMer<-WLData_NewMer[,c("Station","Date","Temp","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Score","RawColScore","Sal")]
WLData_NewMer$YrMo<-format(as.Date(WLData_NewMer$Date, "%d-%b-%y"),"%Y-%m")
#sum(is.na(WLData_NewMer$Date[(WLData_NewMer$Station=="WL039.00")]))
sum(is.na(WLData_NewMer$Date))
WLData_NewMer<-WLData_NewMer[WLData_NewMer$YrMo>="2014-01",]

#merge by row
WLData_Update <- WLData_NewMer
## if they're not, then just drop lat/lon from the dataset that are 0
NumZeroCoords<-nrow(WLData_Update[(WLData_Update$LATITUDE_DECIMAL==0 & WLData_Update$LONGITUDE_DECIMAL==0),])
print(paste("Number of rows removed with (0,0) as (lat,long): ",NumZeroCoords,sep=""))
WLData_Update<-WLData_Update[!(WLData_Update$LATITUDE_DECIMAL==0 & WLData_Update$LONGITUDE_DECIMAL==0),]
## sometimes they put in temp NA's as 99.9 :|
Num99Temp<-nrow(WLData_Update[WLData_Update$Temp==99.9,])
print(paste("Number of Temp values == 99.9 removed: ", Num99Temp,sep=""))
WLData_Update<-WLData_Update[!WLData_Update$Temp==99.9,]
WLData_Update<-WLData_Update[!is.na(WLData_Update$Station),]
WLData_Update$Date<-format(as.Date(WLData_Update$Date, "%d-%b-%y"), "%Y-%m-%d")
WLData_Update<-RemoveDuplicates(WLData_Update)
WLData_Update<-WLData_Update[order(WLData_Update$Date),]
rownames(WLData_Update) <- seq(length=nrow(WLData_Update))

#update IDs
WLData_Update$ID <- 1:nrow(WLData_Update)

write.csv(WLData_Update,"02_Working/WaterQuality/State_DMR/WLData_Update.csv",row.names=FALSE)
