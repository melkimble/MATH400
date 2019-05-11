####
## This reads LOBO data from the New Meadows (Casco Bay)
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
source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))
setwd(TheSourceDir)

FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
TodayDate<-as.character(format(Sys.Date(), "%m-%d-%Y"))

InputDataFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/01_Original/LoboBuoys/Casco/"
OutputCSVFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/LoboBuoys/Casco/"
OutputDataFolderPathPNG<-paste("04_Figures/LoboBuoys/",FileNameDate,"/",sep="")

TheDirs=c(OutputCSVFolderPath,OutputDataFolderPathPNG)

## checks if the the directories exist, if they don't it creates them.
for (TheDir in TheDirs){
  DirectoryExists<-dir.exists(file.path(TheDir))
  if (DirectoryExists==FALSE) dir.create(file.path(TheDir), recursive = TRUE)
}

# Read D0301 data - there's no 'historical' data in this dataset, so commented out just in case it gets added in later.
#casco_D0301_2m_hist<-read.csv(paste(InputDataFolderPath,"Loboviz/D0301/E05_sbe37_historical_2m.csv",sep=""))
#casco_D0301_2m<-rbind(casco_D0301_2m_hist,casco_D0301_2m_curr)
#casco_diagD0301_hist<-read.csv(paste(InputDataFolderPath,"Loboviz/D0301/E05_diagnostics_short_historical.csv",sep=""))
#casco_diagD0301<-rbind(casco_diagD0301_hist,casco_diagD0301_curr)

casco_D0301_2m<-read.csv(paste(InputDataFolderPath,"Loboviz/D0301/D0301_sbe37_realtime_2m.csv",sep=""))
casco_diagD0301<-read.csv(paste(InputDataFolderPath,"Loboviz/D0301/D0301_diagnostics_short_realtime.csv",sep=""))

casco_diagD0301$DateTime<-as.POSIXlt(casco_diagD0301$Time_GMT, tz="GMT", format="%Y-%m-%d %H:%M")
casco_diagD0301$Date<- strftime(casco_diagD0301$DateTime,"%Y-%m-%d")

casco_D0301_2m$DateTime<-as.POSIXlt(casco_D0301_2m$Time.GMT., tz="GMT", format="%Y-%m-%d %H:%M")
casco_D0301_2m$Date<- strftime(casco_D0301_2m$DateTime,"%Y-%m-%d")

TempSalAvg<-casco_D0301_2m %>% 
  group_by(Date) %>% 
  summarise(mean(salinity.psu..2m.), mean(temperature.C..2m.))
TempSalAvg<-as.data.frame(TempSalAvg)

DiagAvg<-casco_diagD0301 %>% 
  group_by(Date) %>% 
  summarise(mean(gps_lat), mean(gps_lon))
DiagAvg<-as.data.frame(DiagAvg)

DiagAvg<-DiagAvg[complete.cases(DiagAvg[ , c("mean(gps_lat)","mean(gps_lon)" )]),]

WL_Casco_D0301_2m<-merge(TempSalAvg,DiagAvg, by.x="Date", by.y="Date",all=FALSE)

colnames(WL_Casco_D0301_2m)[colnames(WL_Casco_D0301_2m)=="mean(salinity.psu..2m.)"] <- "Sal"
colnames(WL_Casco_D0301_2m)[colnames(WL_Casco_D0301_2m)=="mean(temperature.C..2m.)"] <- "TempC"
colnames(WL_Casco_D0301_2m)[colnames(WL_Casco_D0301_2m)=="mean(gps_lat)"] <- "LATITUDE_DECIMAL"
colnames(WL_Casco_D0301_2m)[colnames(WL_Casco_D0301_2m)=="mean(gps_lon)"] <- "LONGITUDE_DECIMAL"
WL_Casco_D0301_2m$YrMo<-format(as.Date(WL_Casco_D0301_2m$Date, "%Y-%m-%d"),"%Y-%m")

WL_Casco_D0301_2m$Depth_m<-"2"
WL_Casco_D0301_2m$Station<-"UMO_D0301_sbe37"
#WL_Casco_D0301_2m$water_depth<-NA
head(WL_Casco_D0301_2m)

############################ Sensor 0070 & 0071 - 
casco_LOBO1_Upper =read.table(paste(InputDataFolderPath,"Loboviz/Sensor0070-20190510215554.tsv",sep=""),sep="\t", header=TRUE, skip=2)
casco_LOBO2_Mouth =read.table(paste(InputDataFolderPath,"Loboviz/Sensor0071-20190510215556.tsv",sep=""),sep="\t", header=TRUE, skip=2)

#casco_LOBO1 <- read.csv(paste(InputDataFolderPath,"NERACOOS/LOBO_CSV_70.csv",sep=""))
#casco_LOBO1_Upper<-read.csv(paste(InputDataFolderPath,"NERACOOS/LOBO_CSV_66.csv",sep=""))
names(casco_LOBO1_Upper)
names(casco_LOBO2_Mouth)

casco_LOBO2_Mouth$DateTime<-as.POSIXlt(casco_LOBO2_Mouth$date..Eastern., tz="EST", format="%Y-%m-%d %H:%M")
casco_LOBO2_Mouth$Date<- strftime(casco_LOBO2_Mouth$DateTime,"%Y-%m-%d")
casco_LOBO2_Mouth$Station<-"casco2_Mouth71"


casco_LOBO1_Upper$DateTime<-as.POSIXlt(casco_LOBO1_Upper$date..Eastern., tz="EST", format="%Y-%m-%d %H:%M")
casco_LOBO1_Upper$Date<- strftime(casco_LOBO1_Upper$DateTime,"%Y-%m-%d")
casco_LOBO1_Upper$Station<-"casco1_Upper70"

LOBOAvg1 <- casco_LOBO2_Mouth %>% 
  group_by(Date,Station) %>%
  summarise(mean(salinity..PSU.), mean(temperature..C.), mean(depth..m.))
LOBOAvg1<-as.data.frame(LOBOAvg1)
LOBOAvg1$LATITUDE_DECIMAL<-"43.776431"
LOBOAvg1$LONGITUDE_DECIMAL<-"-69.891969"

LOBOAvg2 <- casco_LOBO1_Upper %>% 
  group_by(Date,Station) %>%
  summarise(mean(salinity..PSU.), mean(temperature..C.), mean(depth..m.))
LOBOAvg2<-as.data.frame(LOBOAvg2)
LOBOAvg2$LATITUDE_DECIMAL<-"43.863669"
LOBOAvg2$LONGITUDE_DECIMAL<-"-69.898405"

casco_LOBOs_MU<-rbind(LOBOAvg1,LOBOAvg2)


casco_LOBOs_MU<-casco_LOBOs_MU[complete.cases(casco_LOBOs_MU[ , c("mean(salinity..PSU.)","mean(temperature..C.)" )]),]

colnames(casco_LOBOs_MU)[colnames(casco_LOBOs_MU)=="mean(salinity..PSU.)"] <- "Sal"
colnames(casco_LOBOs_MU)[colnames(casco_LOBOs_MU)=="mean(temperature..C.)"] <- "TempC"
colnames(casco_LOBOs_MU)[colnames(casco_LOBOs_MU)=="mean(depth..m.)"] <- "Depth_m"
#colnames(LOBOAvg)[colnames(LOBOAvg)=="mean(water_depth)"] <- "water_depth"

casco_LOBOs_MU$YrMo<-format(as.Date(casco_LOBOs_MU$Date, "%Y-%m-%d"),"%Y-%m")

############################ 
head(casco_LOBOs_MU)
head(WL_Casco_D0301_2m)
casco_BuoyMerge<-rbind(casco_LOBOs_MU,WL_Casco_D0301_2m)

casco_BuoyMerge<-RemoveDuplicates(casco_BuoyMerge)
casco_BuoyMerge<-casco_BuoyMerge[order(casco_BuoyMerge$Date),]
rownames(casco_BuoyMerge) <- seq(length=nrow(casco_BuoyMerge))
#update IDs
casco_BuoyMerge$ID <- 1:nrow(casco_BuoyMerge)

casco_BuoyMerge$Zone<-"WL"
casco_BuoyMerge<-casco_BuoyMerge[,c("ID","Station","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Date","YrMo","TempC","Sal","Depth_m","Zone")]

casco_BuoyMerge$LATITUDE_DECIMAL<-as.character(round(as.numeric(casco_BuoyMerge$LATITUDE_DECIMAL),6))
casco_BuoyMerge$LONGITUDE_DECIMAL<-as.character(round(as.numeric(casco_BuoyMerge$LONGITUDE_DECIMAL),6))
casco_BuoyMerge$TempC<-round(casco_BuoyMerge$TempC,2)
casco_BuoyMerge$Sal<-round(casco_BuoyMerge$Sal,2)
casco_BuoyMerge$Depth_m<-as.character(round(as.numeric(casco_BuoyMerge$Depth_m,2)))

outputCSV<-paste(OutputCSVFolderPath,"casco_BuoyMerge.csv",sep="")
write.csv(casco_BuoyMerge,outputCSV,row.names=FALSE)