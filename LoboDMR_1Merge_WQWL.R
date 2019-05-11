####
## This reads LOBO and DMR data and merges them together
##
## CREATED BY: Melissa Kimble
## LAST MODIFIED: 04/28/2019
###
LibraryList<-c("ggplot2","dplyr")
for (TheLibrary in LibraryList)
{
  if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
}
library(ggplot2)
library(dplyr)

###################################################################################
## setup
###################################################################################
TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
#source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))
setwd(TheSourceDir)

FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
TodayDate<-as.character(format(Sys.Date(), "%m-%d-%Y"))

InputDataFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/WaterQuality/State_DMR/From_Geoff/"
OutputCSVFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/WaterQuality/State_DMR/From_Geoff/"
OutputDataFolderPathPNG<-paste("04_Figures/Aquaculture/State_DMR/From_Geoff/",FileNameDate,"/",sep="")

TheDirs=c(OutputCSVFolderPath,OutputDataFolderPathPNG)

## checks if the the directories exist, if they don't it creates them.
for (TheDir in TheDirs){
  DirectoryExists<-dir.exists(file.path(TheDir))
  if (DirectoryExists==FALSE) dir.create(file.path(TheDir), recursive = TRUE)
}

## WQ and WL were cleaned and updated (appended new dates) in a separate R script
DMRDataWQ<-read.csv(paste(InputDataFolderPath,"WQData_Update.csv",sep=""))
DMRDataWL<-read.csv(paste(InputDataFolderPath,"WLData_Update.csv",sep=""))
DMRData<-rbind(DMRDataWQ,DMRDataWL)
names(DMRData)
DMRData<-DMRData[,c("Station","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Date","YrMo","TempC","Sal","Zone")]
DMRData$Depth_m<-0.25
DMRData$Type<-"Sample"

BuoyDataWQ<-read.csv("D:/Dropbox/00_Dissertation/01_Data/02_Working/LoboBuoys/Damariscotta/dam_BuoyMerge.csv")
BuoyDataWL<-read.csv("D:/Dropbox/00_Dissertation/01_Data/02_Working/LoboBuoys/Casco/casco_BuoyMerge.csv")
BuoyDataWQ<-BuoyDataWQ[,c("Station","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Date","YrMo","TempC","Sal","Depth_m","Zone")]
BuoyDataWL<-BuoyDataWL[,c("Station","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Date","YrMo","TempC","Sal","Depth_m","Zone")]
BuoyData<-rbind(BuoyDataWQ,BuoyDataWL)
BuoyData$Type<-"Buoy"

DMRBuoyData<-rbind(DMRData,BuoyData)
## removing buoy UMO_D0301_sbe37 because it's not in the estuary.
DMRBuoyData<-DMRBuoyData[!DMRBuoyData$Station == "UMO_D0301_sbe37",]

outputCSV<-paste(OutputCSVFolderPath,"WQWL_DMRBuoyMerge.csv",sep="")
write.csv(DMRBuoyData,outputCSV,row.names=FALSE)