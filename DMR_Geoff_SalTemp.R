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


FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
TodayDate<-as.character(format(Sys.Date(), "%m-%d-%Y"))

InputDataFolderPath<-"02_Working/Aquaculture/State_DMR/From_Flora/"
OutputCSVFolderPath<-"02_Working/Aquaculture/State_DMR/From_Flora/"
OutputDataFolderPathPNG<-paste("04_Figures/Aquaculture/State_DMR/From_Geoff/",FileNameDate,"/",sep="")

TheDirs=c(OutputDataFolderPathPNG)

## checks if the the directories exist, if they don't it creates them.
for (TheDir in TheDirs){
  DirectoryExists<-dir.exists(file.path(TheDir))
  if (DirectoryExists==FALSE) dir.create(file.path(TheDir), recursive = TRUE)
}

DMRDataWQ<-read.csv("02_Working/WaterQuality/State_DMR/WQData_Update.csv")
DMRDataWL<-read.csv("02_Working/WaterQuality/State_DMR/WLData_Update.csv")
DMRDataWQ$Zone<-"WQ"
DMRDataWL$Zone<-"WL"
DMRData<-rbind(DMRDataWQ,DMRDataWL)

###################################################################################
DMRData$YrMo2<-format(as.Date(DMRData$Date, "%Y-%m-%d"),"%Y-%m")
DMRData$Month<-format(as.Date(DMRData$Date, "%Y-%m-%d"),"%b")
DMRData$Year<-format(as.Date(DMRData$Date, "%Y-%m-%d"),"%Y")

Months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
DMRData$Month<-factor(DMRData$Month, levels = Months)

DMRData_YrMoSub<-DMRData[(DMRData$YrMo2>="2014-05" & DMRData$YrMo2<="2014-11"),]
#names(DMRData)
#unique(DMRData_YrMoSub$Zone)
###################################################################################
#plot(DMRData$Temp~DMRData$Sal, xlim=c(0,35))

FileName<-paste("DMRWQ_SalTemp",sep="")
ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
Cairo(ThePlotOutputFile,type="png",units="in",width=7,height=6,res=300, pointsize=12,bg="white")
ggplot(data = DMRData, aes(x = as.numeric(Temp), y = as.numeric(Sal), fill=Zone, colour=Zone)) +  
  geom_point() + 
  geom_smooth() + 
  xlab("Temperature (C)") + 
  scale_x_continuous(breaks=seq(0,35,5)) +
  scale_y_continuous(limits=c(0,35),breaks=seq(0,35,5)) +
  ylab("Salinity (PPT)") +
  ggtitle("DMR Salinity ~ Temperature") +
  theme(plot.title = element_text(hjust=0.5))
#######################################################################
dev.off()

FileName<-paste("DMRWQ_Sal_Hist",sep="")
ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
Cairo(ThePlotOutputFile,type="png",units="in",width=5,height=8,res=300, pointsize=12,bg="white")
ggplot(DMRData,aes(x=as.numeric(Sal))) + 
  xlim(0,35) +
  ylab("Density") +
  xlab("Salinity (PPT)") +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_grid(Zone ~ .) 
  
#######################################################################
dev.off()

FileName<-paste("DMRWQ_Temp_Hist",sep="")
ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
Cairo(ThePlotOutputFile,type="png",units="in",width=5,height=8,res=300, pointsize=12,bg="white")
ggplot(DMRData,aes(x=as.numeric(Temp))) + 
  xlim(0,35) +
  ylab("Density") +
  xlab("Temperature (C)") +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_grid(Zone ~ .) 
#######################################################################
dev.off()