####
## This script looks at coincident dates between WQ, WL, and 
## the LSAT8 derived SST from the CoastalSatLab
##
## **RESULTS - 36 DATES
## "2014-05-05" "2014-05-06" "2014-05-13" "2014-05-14" "2014-05-20" "2014-05-28"
## "2014-05-30" "2014-06-03" "2014-06-10" "2014-06-11" "2014-06-25" "2014-07-16"
## "2014-07-28" "2014-07-29" "2014-08-04" "2014-08-05" "2014-08-06" "2014-08-10"
## "2014-08-11" "2014-08-20" "2014-08-21" "2014-08-25" "2014-09-02" "2014-09-03"
## "2014-09-10" "2014-09-15" "2014-09-16" "2014-09-19" "2014-09-29" "2014-10-01"
## "2014-10-20" "2014-10-29" "2014-11-03" "2014-11-05" "2014-11-17" "2014-11-18"
##
## CREATED BY: Melissa Kimble
## LAST MODIFIED: 04/28/2019
##
## LSAT8 Naming Convention:
################################################
## Scene ID
## LXSPPPRRRYYYYDDDGSIVV
## L = Landsat
## X = Sensor
## S = Satellite
## PPP = WRS Path
## RRR = WRS Row
## YYYY = Year
## DDD = Julian day of year
## GSI = Ground station identifier
## VV = Archive version number
################################
## LSAT Product Identifier
## LXSS_LLLL_PPPRRR_YYYYMMDD_yyyymmdd_CC_TX
## L = Landsat
## X = Sensor ("C" = OLI/TIRS Combined, "O" = OLI-only, "T" = TIRS-only, "E" = ETM+, "T" = TM, "M" = MSS)
## SS = Satellite ("07" = Landsat 7, "08"= Landsat8)
## LLLL = Processing correction level ("L!TP": Precision Terrain, "L1GT": Systematic Terrain, "L1GS": Systematic)
## PPP = WRS path
## RRR = WRS row
## YYYYMMDD = Acquisition (YYYY)/Month(MM)/Day(DD)
## yyyymmdd = Processing year (yyyy) / Month (mm)/Day (dd)
## CC = Collection Number ("01","02")
## TX = Collection category: ("RT" for Real-Time, "T1" for Tier 1, or "T2" for Tier 2)


LibraryList<-c("foreign","spdep","RColorBrewer","classInt","maptools","rgdal","spatstat","plyr","raster","rstudioapi","ggplot2","reshape2","Cairo")
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
library(raster)
library(rstudioapi)
library(ggplot2)
library(reshape2)
library(Cairo)


###################################################################################
# Additional files (classes) that have functions that are used in this script.    #
###################################################################################
TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))
setwd("D:/Dropbox/00_Dissertation/01_Data/")

###################################################################################
## setup
###################################################################################
#OutputDataFolderPathPNG<-"02_Working/SST_DMR_LSAT8/Figures/"
#InputDataFolderPath<-"01_Original/CoastalSatLab/SST/"
## grab the GCS prj from WQpolyjoin
#TheGCSprj<-projection(readOGR("01_Original/FromKate/Damariscotta/WQpolyjoin.shp"))
## grab the prj of the raster
#UTMRaster<-raster("02_Working/CoastalSatLab/LC80110302013195LGN00_SST_WQMask.tif")
#TheUTMprj<-projection(UTMRaster)

DMRDataWQ<-read.csv("02_Working/WaterQuality/State_DMR/WQData_Update.csv")
DMRDataWL<-read.csv("02_Working/WaterQuality/State_DMR/WLData_Update.csv")
DMRDataWQ$Zone<-"WQ"
DMRDataWL$Zone<-"WL"
DMRData<-rbind(DMRDataWQ,DMRDataWL)

SST_Summary<-read.csv("01_Original/SeaSurfaceTemp/CoastalSatLab/SST_Summary.csv")
SST_Summary<-SST_Summary[SST_Summary$Region=="Mid Maine Coast",]
SST_Summary$YrMo<-format(as.Date(SST_Summary$Date, "%m/%d/%Y"),"%Y-%m")
SST_Summary$Date<-format(as.Date(SST_Summary$Date, "%m/%d/%Y"),"%Y-%m-%d")

STTSummary_YrMoSub<-SST_Summary[(SST_Summary$YrMo>="2014-05" & SST_Summary$YrMo<="2014-11"),]

## Function that appends PCS coordinates to GCS prjs.
#DMRData<-AddEastNorthCoords(DMRData,"LONGITUDE_DECIMAL","LATITUDE_DECIMAL",TheGCSprj,TheUTMprj)
## We only want WQ samples for the same month and year as the SST
#DateField="Date"
SST_Summary$FileName<-as.character(SST_Summary$FileName)
#TheRasters<-unique(SST_Summary$FileName)
#TheRaster="LC80110302015105LGN00_SSTiterate.tif"
#"LC80110302015313LGN00_SSTiterate.tif"

###################################################################################
DMRData$YrMo2<-format(as.Date(DMRData$Date, "%Y-%m-%d"),"%Y-%m")
DMRData_YrMoSub<-DMRData[(DMRData$YrMo2>="2014-05" & DMRData$YrMo2<="2014-11"),]
names(DMRData)
unique(DMRData_YrMoSub$Zone)
###################################################################################

DMRDates<-as.vector(unique(DMRData_YrMoSub$Date))
STTDates<-as.vector(unique(STTSummary_YrMoSub$Date))

CombinedDates<-sort(unique(c(DMRDates,STTDates)),decreasing=FALSE)

