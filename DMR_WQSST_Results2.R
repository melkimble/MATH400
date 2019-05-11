####
## Script to compile results of DMRData_Analysis.R!
##
## Use DMRData_Result2.R
## - the write.csv areas are commented out in DMRData_Results2. (i uncommented them)
## - the newly created data.frames have more columns
## CREATED BY: Melissa Kimble
## LAST MODIFIED: 04-20-2019
###

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


LibraryList<-c("foreign","spdep","RColorBrewer","classInt","maptools","rgdal","spatstat","plyr",
               "raster","rstudioapi","ggplot2","reshape2","Cairo","reshape2")
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
library(reshape2)

###################################################################################
# Additional files (classes) that have functions that are used in this script.    #
###################################################################################
TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))
setwd("D:/Dropbox/00_Dissertation/01_Data/")

###################################################################################
# Log all print statemenets to txt document - also includes folder paths
###################################################################################
FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
TodayDate<-as.character(format(Sys.Date(), "%m-%d-%Y"))

InputDataFolderPath<-"02_Working/SeaSurfaceTemp/CoastalSatLab/NARm/"
OutputDataFolderPath<-"02_Working/SeaSurfaceTemp/CoastalSatLab/NARm/"
CoordFolderPath<-paste(OutputDataFolderPath,"Coords",sep="")
BufferFolderPath<-paste(OutputDataFolderPath,"Buffer",sep="")

## had to add this in so that new csvs will go into a dated folder
InputCSVFolderPath<-paste(OutputDataFolderPath,"CSV_ExtrTempVal/",FileNameDate,"/",sep="")
OutputCSVFolderPath<-paste(OutputDataFolderPath,"CSV_ExtrTempVal/",FileNameDate,"/Results2/",sep="")
OutputDataFolderPathPNG<-paste("04_Figures/SeaSurfaceTemp/CoastalSatLab/NARm/",FileNameDate,"/Results2/",sep="")

TheDirs=c(OutputDataFolderPath,CoordFolderPath,BufferFolderPath,InputCSVFolderPath,OutputCSVFolderPath,OutputDataFolderPathPNG)
## checks if the the directories exist, if they don't it creates them.
for (TheDir in TheDirs){
  DirectoryExists<-dir.exists(file.path(TheDir))
  if (DirectoryExists==FALSE) dir.create(file.path(TheDir), recursive = TRUE)
}

SinkFilename<-paste("00_PrintOutput_DMRData_Results2_",FileNameDate,sep="")
OutputCSVFile<-paste(OutputCSVFolderPath,SinkFilename,".txt",sep="")
sink(OutputCSVFile,append=FALSE,split=TRUE)
###################################################################################
## setup
###################################################################################
OverWriteFiles = "no"

## Function that appends PCS coordinates to GCS prjs.
#DMRData<-AddEastNorthCoords(DMRData,"LONGITUDE_DECIMAL","LATITUDE_DECIMAL",TheGCSprj,TheUTMprj)
#DateField="Date"

TheScriptStartTime=Sys.time()

##DOES EXIST?
FileName<-"All_SSTDMR_ExtrTempVal.csv"
OutputCSVFile<-paste(OutputCSVFolderPath, FileName, sep="")

if (file.exists(OutputCSVFile) & OverWriteFiles=="no") {
  print (paste("Already Exists: ",FileName, sep=""))
  DMRData_Results<-read.csv(OutputCSVFile)
} else {
  print (paste("Does Not Exist: ",FileName, sep=""))
  DMRData_Results<-data.frame()
  TheFilePaths <- list.files(path=InputCSVFolderPath, pattern="\\.csv$", full.names=F, recursive=FALSE)
  #TheFilePaths <- TheFilePaths[25:25] ## 25 only has 2 points you have to worry about, so it runs faster.
  for (TheExtrData in TheFilePaths) {
    TheFileStartTime<-Sys.time()
    TheExtrData<-as.character(TheExtrData)
    TheExtrDataParse<-strsplit(TheExtrData,"_")
    TheRaster<-TheExtrDataParse[[1]][1]
    #TheRaster<-trimws(TheRaster)
    print(paste("Starting: ", TheExtrData,sep=""))
    TheExtrDatarFile<-paste(InputCSVFolderPath,TheExtrData,sep="")
    TheExtrData_df<-read.csv(TheExtrDatarFile)
    TheExtrData_df$TheRaster<-TheRaster
    DMRData_Results<-rbind(DMRData_Results,TheExtrData_df)
  }
  
  write.csv(DMRData_Results, file=OutputCSVFile,quote=FALSE,row.names=FALSE)
}

StationTbl<-unique(DMRData_Results[c("Station","Eastings","Northings")])

FileName<-"All_SSTDMR_TempDiff.csv"
OutputCSVFile<-paste(OutputCSVFolderPath, FileName, sep="")
if (file.exists(OutputCSVFile) & OverWriteFiles=="no") {
  print (paste("Already Exists: ",FileName, sep=""))
  DMRData_MedExtr_merge<-read.csv(OutputCSVFile)
  colremv<-c("Eastings","Northings")
  DMRData_MedExtr<-DMRData_MedExtr_merge[, !(names(DMRData_MedExtr_merge) %in% colremv)]
} else {
  
  #names(DMRData_Results)
  DMRData_Results$DMRDate2<-as.Date(DMRData_Results$DMRDate, "%Y-%m-%d")
  DMRData_Results$SatYYYYMMDD2<-as.Date(DMRData_Results$SatYYYYMMDD, "%Y-%m-%d")
  #DMRData_Results$YrMo<-format(as.Date(DMRData_Results$SatYYYYMMDD2, "%Y-%m-%d"),"%Y-%m")
  #DMRData_Results$Yr<-format(as.Date(DMRData_Results$SatYYYYMMDD2, "%Y-%m-%d"),"%Y")
  #DMRData_Results$Mo<-format(as.Date(DMRData_Results$SatYYYYMMDD2, "%Y-%m-%d"),"%b")
  #names(DMRData_Results)
  
  ## all extracted DMR values that overlapped with SST
  DMRData_ExtrSub<-DMRData_Results[!is.na(DMRData_Results$SSTExtracTemp),]
  DMRData_ExtrSub$buffDist<-0
  DMRData_ExtrSub<-DMRData_ExtrSub[,c("Station","DMRDate2","DMRTemp","SatYYYYMMDD2","buffDist","SSTExtracTemp")]
  colnames(DMRData_ExtrSub)<-c("Station","DMRDate2","DMRTemp","SatYYYYMMDD2","buffDist","SST")
  
  ## all extracted DMR values that did NOT overlap with SST - we also don't want ones that are super far away
  ## i.e. 300m
  DMRData_DistSub<-DMRData_Results[is.na(DMRData_Results$SSTExtracTemp),]
  DMRData_DistSub<-DMRData_DistSub[as.numeric(as.character(DMRData_DistSub$buffDist))<300,]
  sum(is.na(DMRData_DistSub$SSTBuffTemp))
  
  #mean(as.numeric(DMRData_DistSub$buffDist[DMRData_DistSub$Yr == "2016"]))
  #mean(as.numeric(DMRData_DistSub$buffDist[DMRData_DistSub$Yr == "2015"]))
  #mean(as.numeric(DMRData_DistSub$buffDist[DMRData_DistSub$Yr == "2014"]))
  #mean(as.numeric(DMRData_DistSub$buffDist[DMRData_DistSub$Yr == "2013"]))
  
  ## there were 34 0's from the few sites that were directly overlapping
  #mean(c(as.numeric(DMRData_DistSub$buffDist),rep(0, 34)))
  #min(c(as.numeric(DMRData_DistSub$buffDist),rep(0, 34)))
  #max(c(as.numeric(DMRData_DistSub$buffDist),rep(0, 34)))
  
  ## aggregate the buffered values
  DMRData_DistMed<-aggregate(as.numeric(as.character(DMRData_DistSub$SSTBuffTemp))~as.character(DMRData_DistSub$Station)+
                               as.numeric(as.character(DMRData_DistSub$DMRTemp))+as.Date(DMRData_DistSub$DMRDate2)+
                               as.Date(DMRData_DistSub$SatYYYYMMDD2)+DMRData_DistSub$buffDist,data=DMRData_DistSub, median)

  #names(DMRData_DistMed)
  colnames(DMRData_DistMed)<-c("Station","DMRTemp","DMRDate2","SatYYYYMMDD2","buffDist", "SST")
  DMRData_DistMed<-DMRData_DistMed[,c("Station","DMRDate2","DMRTemp","SatYYYYMMDD2","buffDist", "SST")]
  names(DMRData_DistMed)
  
  mean(DMRData_DistMed$DMRTemp-DMRData_DistMed$SST)
  
  DMRData_MedExtr<-rbind(DMRData_ExtrSub,DMRData_DistMed)
  
  ## write out to file with tempdiff and coordinates
  DMRData_MedExtr_merge<-merge(StationTbl,DMRData_MedExtr,by.x="Station", by.y="Station")
  
  DMRData_MedExtr_merge$TempDiff<-DMRData_MedExtr_merge$DMRTemp-DMRData_MedExtr_merge$SST
  DMRData_MedExtr_merge$buffDist<-as.character(round(as.numeric(DMRData_MedExtr_merge$buffDist),2))
  DMRData_MedExtr_merge$SST<-as.character(round(as.numeric(DMRData_MedExtr_merge$SST),2))
  DMRData_MedExtr_merge$TempDiff<-as.character(round(as.numeric(DMRData_MedExtr_merge$TempDiff),2))
  
  write.csv(DMRData_MedExtr_merge, file=OutputCSVFile,quote=FALSE,row.names=FALSE)
}


## Some Summary stats
AvgDist<-as.character(DMRData_Results$buffDist)
AvgDist[AvgDist=="isExtracted"]<-0.00
AvgDist<-as.double(AvgDist)
## 300m dist chosen because data freq ends at 300m, then re-spikes at 6000~8000m
hist(AvgDist, breaks=1000, xlim=c(0,500))
# this is the raw data, so I'm just redoing a step that's done above with buffdist. Since the column is dropped, 
# I don't have it and have to redo the subset here. We didn't include any distances greater than 300.
AvgDist<-AvgDist[AvgDist<300.00]
AvgDist<-na.omit(AvgDist)
print(paste("Median Distance:",median(AvgDist)))
print(paste("Mean Distance:",mean(AvgDist)))

# median distance approximately 60.99369 meters
# mean distance is approximately 71.24303 meters

DMRData_MedExtr_merge$Mo<-format(as.Date(DMRData_MedExtr_merge$SatYYYYMMDD2, "%Y-%m-%d"),"%m")
Mos<-c("01","02","03","04","05","06","07","08","09","10","11","12")
for (Month in Mos){
  DMRData_MedExtr_mosub<-DMRData_MedExtr_merge[DMRData_MedExtr_merge$Mo==Month,]
  NumOverlappingSites<-nrow(DMRData_MedExtr_mosub)
  NumScenes<-length(unique(DMRData_MedExtr_mosub$SatYYYYMMDD2))
  #mediantemp<-median(abs(DMRData_MedExtr_mosub$TempDiff))
  #meantemp<-mean(abs(DMRData_MedExtr_mosub$TempDiff))
  avgtemp<-round(mean(as.numeric(na.omit(DMRData_MedExtr_mosub$TempDiff))),2)
  #print(paste(Month,": Median ", mediantemp,", Mean ", meantemp,", Avg ",avgtemp, sep=""))
  print(paste(Month,": Avg ",avgtemp,", Num Sites: ", NumOverlappingSites, ", Num Scenes: ", NumScenes, sep=""))
  
}

#median(abs(DMRData_MedExtr_merge$TempDiff))
#mean(abs(DMRData_MedExtr_merge$TempDiff))
round(mean(as.numeric(DMRData_MedExtr_merge$TempDiff)),2)
#median temp difference is 6 deg C 

## distances


### PLOTS 

DMRData_MedExtr$YrMo<-format(as.Date(DMRData_MedExtr$SatYYYYMMDD2, "%Y-%m-%d"),"%Y-%m")
names(DMRData_MedExtr)

DMRData_MedExtr2<-DMRData_MedExtr[,c("YrMo","DMRTemp","SST")]
melt_df<-melt(DMRData_MedExtr2, id="YrMo")
names(melt_df)

FileName<-"DMRSSTData_Results_TempPlotYrMo"
ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
Cairo(ThePlotOutputFile,type="png",units="in",width=15,height=4,res=300, pointsize=12,bg="white")
PlotTitle<-paste("DMR Data Results for LSAT8 SST",sep="")
ThePlot<-ggplot(melt_df, aes(x=YrMo, y=value, group=variable)) +
  geom_line(size=1, aes(linetype=variable, colour=variable)) +
  ggtitle(PlotTitle) +
  #  scale_x_date(format="%b %Y") +
  #  scale_x_date(date_breaks = "1 month", 
  #               date_labels =  "%b %Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  xlab("Date") + 
  ylab("Temp (C)")
print(ThePlot)
dev.off()

FileName<-"DMRSSTData_Results_TempBoxPlotYrMo"
ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
Cairo(ThePlotOutputFile,type="png",units="in",width=15,height=4,res=300, pointsize=12,bg="white")
PlotTitle<-paste("DMR Data Results for LSAT8 SST",sep="")
ThePlot<-ggplot(aes(y = value, x = YrMo, fill = variable), data = melt_df) + 
  geom_boxplot() +
  ggtitle(PlotTitle) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  xlab("Date") + 
  ylab("Temp (C)")
print(ThePlot)
dev.off()

DMRData_MedExtr$Mo<-format(as.Date(DMRData_MedExtr$SatYYYYMMDD2, "%Y-%m-%d"),"%m")
names(DMRData_MedExtr)
DMRData_MedExtr3<-DMRData_MedExtr[,c("Mo","DMRTemp","SST")]
meltmo_df<-melt(DMRData_MedExtr3, id="Mo")
names(meltmo_df)

FileName<-"DMRSSTData_Results_TempPlotMo"
ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
Cairo(ThePlotOutputFile,type="png",units="in",width=15,height=4,res=300, pointsize=12,bg="white")
PlotTitle<-paste("DMR Data Results for LSAT8 SST",sep="")
ThePlot<-ggplot(meltmo_df, aes(x=Mo, y=value, group=variable)) +
  geom_line(size=1, aes(linetype=variable, colour=variable)) +
  ggtitle(PlotTitle) +
  #  scale_x_date(format="%b %Y") +
  #  scale_x_date(date_breaks = "1 month", 
  #               date_labels =  "%b %Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  xlab("Month") + 
  ylab("Temp (C)")
print(ThePlot)
dev.off()

FileName<-"DMRSSTData_Results_TempBoxPlotMo"
ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
Cairo(ThePlotOutputFile,type="png",units="in",width=15,height=4,res=300, pointsize=12,bg="white")
PlotTitle<-paste("DMR Data Results for LSAT8 SST",sep="")
ThePlot<-ggplot(aes(y = value, x = Mo, fill = variable), data = meltmo_df) + 
  geom_boxplot() +
  ggtitle(PlotTitle) +
  xlab("Month") + 
  ylab("Temp (C)")
print(ThePlot)
dev.off()

## End logging and write to file.
sink()