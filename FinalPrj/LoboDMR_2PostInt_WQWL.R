####
## This is the merged data from DMR Water Quality (WQ WL) and the Lobo Buoys (WQ WL)
## To merge all satellite temp, WGOM, and euclidean distance dbfs.
##
## CREATED BY: Melissa Kimble
## LAST MODIFIED: 04/28/2019
###
LibraryList<-c("ggplot2","dplyr","foreign")
for (TheLibrary in LibraryList)
{
  if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
}
library(ggplot2)
library(dplyr)
library(foreign)

###################################################################################
## setup
###################################################################################
TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
#source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))
setwd(TheSourceDir)

FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
TodayDate<-as.character(format(Sys.Date(), "%m-%d-%Y"))
InputDataFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/WaterQuality/State_DMR/From_Geoff/"
InputZonalCoastalDBFFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/SeaSurfaceTemp/CoastalSatLab/NARm/ZonalStats/"
InputZonalStreamsDBFFile<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/WaterQuality/FromKate/ShellfishWaterQuality/ZonalStats/CoastalStreams_EucDist_WQWL.dbf"
InputZonalWGOMDBFFile<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/WaterQuality/FromKate/ShellfishWaterQuality/ZonalStats/WGOM8m_Bathy_WQWL.dbf"
OutputCSVFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/WaterQuality/"
OutputDataFolderPathPNG<-paste("04_Figures/Aquaculture/State_DMR/From_Geoff/",FileNameDate,"/",sep="")

TheDirs=c(OutputCSVFolderPath,OutputDataFolderPathPNG)

## checks if the the directories exist, if they don't it creates them.
for (TheDir in TheDirs){
  DirectoryExists<-dir.exists(file.path(TheDir))
  if (DirectoryExists==FALSE) dir.create(file.path(TheDir), recursive = TRUE)
}

## WQ and WL were cleaned and updated (appended new dates) in a separate R script
DMRDataWQ_int<-read.csv(paste(InputDataFolderPath,"WQWL_DMRBuoyMerge_FishInt.csv",sep=""))
#identical(DMRDataWQ_int[['SMA']],DMRDataWQ_int[['Zone']])
DMRDataWQ_int<-DMRDataWQ_int[,c("Id","SMAFishId","Station","LATITUDE_D","LONGITUDE_","Date","YrMo","TempC","Sal","Zone","Depth_m","Type","DMRBuoyID")]
colnames(DMRDataWQ_int)[colnames(DMRDataWQ_int)=="LATITUDE_D"] <- "LATITUDE_DECIMAL"
colnames(DMRDataWQ_int)[colnames(DMRDataWQ_int)=="LONGITUDE_"] <- "LONGITUDE_DECIMAL"
DMRDataWQ_int$DateTime<-as.POSIXlt(DMRDataWQ_int$Date, tz="GMT", format="%m/%d/%Y %H:%M:%S")
DMRDataWQ_int$Date2<- strftime(DMRDataWQ_int$DateTime,"%Y-%m-%d")
DMRDataWQ_int$Date<-DMRDataWQ_int$Date2
ColDrop <- c("Date2","DateTime")
DMRDataWQ_int<-DMRDataWQ_int[ , !(names(DMRDataWQ_int) %in% ColDrop)]

TheScriptStartTime=Sys.time()
TheFilePaths <- list.files(path=InputZonalCoastalDBFFolderPath, pattern="\\.dbf$", full.names=F, recursive=FALSE)
TheDBFs<-data.frame()
for (TheDBFFileName in TheFilePaths) {
  TheFileStartTime<-Sys.time()
  ###################################################################################
  ## Sea surface temperature DBFs SETUP
  ###################################################################################
  #TheDBFFileName<-TheFilePaths[1]
  TheDBFFileName<-as.character(TheDBFFileName)

  print(paste("Starting :", TheDBFFileName,sep=""))
  TheDBFFile<-paste(InputZonalCoastalDBFFolderPath,TheDBFFileName,sep="")
  TheDBF<-read.dbf(TheDBFFile)
  
  #################################################################################
  # Setup date time fields by parsing LSAT filename
  # We only want WQ samples for the same month and year as the SST
  #################################################################################
  TheDBFParse<-strsplit(TheDBFFileName,"")
  PPPRRR<-as.character(paste(TheDBFParse[[1]][4:9],collapse=""))
  FullDate<-as.character(paste(TheDBFParse[[1]][10:16], collapse=""))
  #TheDate<-format(as.Date(FullDate, "%Y%j"),"%Y%m%d")
  SatYYYYMMDD<-format(as.Date(FullDate, "%Y%j"),"%Y-%m-%d")
  SatYrMo<-format(as.Date(FullDate, "%Y%j"),"%Y-%m")
  print(paste("Starting new SST: ",SatYrMo, " | ",TheDBFFileName,sep=""))
  
  TheDBF<-TheDBF[,c("Id","MEAN")]
  colnames(TheDBF)[colnames(TheDBF)=="MEAN"] <- "SatTempC"
  TheDBF$Date<-SatYYYYMMDD
  
  TheDBFs<-rbind(TheDBFs,TheDBF)
}

DBFMerge<-left_join(DMRDataWQ_int, TheDBFs, by = c("Id","Date"))

StreamsDBF<-read.dbf(InputZonalStreamsDBFFile)
StreamsDBF$MEAN
colnames(StreamsDBF)[colnames(StreamsDBF)=="MEAN"] <- "StreamDist_m"
StreamsDBF<-StreamsDBF[,c("Id","StreamDist_m")]
StreamsDBF$StreamDist_m<-round(StreamsDBF$StreamDist_m,2)

DBFMerge<-merge(DBFMerge,StreamsDBF, by=c("Id"),all.x=TRUE)

WGOMDBF<-read.dbf(InputZonalWGOMDBFFile)
colnames(WGOMDBF)[colnames(WGOMDBF)=="MEAN"] <- "WGOM_m"
WGOMDBF<-WGOMDBF[,c("Id","WGOM_m")]
WGOMDBF$WGOM_m<-round(WGOMDBF$WGOM_m,2)

DBFMerge<-merge(DBFMerge,WGOMDBF, by=c("Id"),all.x=TRUE)
DBFMerge$WGOM_m[is.na(DBFMerge$WGOM_m)] <- -0.1

outputCSV<-paste(OutputCSVFolderPath,"all_DMRBuoyRast.csv",sep="")
write.csv(DBFMerge,outputCSV,row.names=FALSE)
