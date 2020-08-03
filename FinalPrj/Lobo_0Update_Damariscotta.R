####
## This reads LOBO data from the Damariscotta
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

InputDataFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/01_Original/LoboBuoys/Damariscotta/"
OutputCSVFolderPath<-"D:/Dropbox/00_Dissertation/01_Data/02_Working/LoboBuoys/Damariscotta/"
OutputDataFolderPathPNG<-paste("04_Figures/LoboBuoys/",FileNameDate,"/",sep="")

TheDirs=c(OutputCSVFolderPath,OutputDataFolderPathPNG)

## checks if the the directories exist, if they don't it creates them.
for (TheDir in TheDirs){
  DirectoryExists<-dir.exists(file.path(TheDir))
  if (DirectoryExists==FALSE) dir.create(file.path(TheDir), recursive = TRUE)
}

# Read E0502 data
dam_E0502_2m_hist<-read.csv(paste(InputDataFolderPath,"Loboviz/E0502/E05_sbe37_historical_2m.csv",sep=""))
dam_E0502_2m_curr<-read.csv(paste(InputDataFolderPath,"Loboviz/E0502/E0502_sbe37_realtime_2m.csv",sep=""))
dam_E0502_2m<-rbind(dam_E0502_2m_hist,dam_E0502_2m_curr)

dam_diagE0502_hist<-read.csv(paste(InputDataFolderPath,"Loboviz/E0502/E05_diagnostics_short_historical.csv",sep=""))
dam_diagE0502_curr<-read.csv(paste(InputDataFolderPath,"Loboviz/E0502/E0502_diagnostics_short_realtime.csv",sep=""))
dam_diagE0502<-rbind(dam_diagE0502_hist,dam_diagE0502_curr)

dam_diagE0502$DateTime<-as.POSIXlt(dam_diagE0502$Time.GMT., tz="GMT", format="%Y-%m-%d %H:%M")
dam_diagE0502$Date<- strftime(dam_diagE0502$DateTime,"%Y-%m-%d")

dam_E0502_2m$DateTime<-as.POSIXlt(dam_E0502_2m$Time.GMT., tz="GMT", format="%Y-%m-%d %H:%M")
dam_E0502_2m$Date<- strftime(dam_E0502_2m$DateTime,"%Y-%m-%d")

TempSalAvg<-dam_E0502_2m %>% 
  group_by(Date) %>% 
  summarise(mean(salinity.psu..2m.), mean(temperature.C..2m.))
TempSalAvg<-as.data.frame(TempSalAvg)

DiagAvg<-dam_diagE0502 %>% 
  group_by(Date) %>% 
  summarise(mean(gps_lat.degrees_north...0m.), mean(gps_lon.degrees_west...0m.))
DiagAvg<-as.data.frame(DiagAvg)

DiagAvg<-DiagAvg[complete.cases(DiagAvg[ , c("mean(gps_lat.degrees_north...0m.)","mean(gps_lon.degrees_west...0m.)" )]),]

WQ_Damar_E0502_2m<-merge(TempSalAvg,DiagAvg, by.x="Date", by.y="Date",all=FALSE)

colnames(WQ_Damar_E0502_2m)[colnames(WQ_Damar_E0502_2m)=="mean(salinity.psu..2m.)"] <- "Sal"
colnames(WQ_Damar_E0502_2m)[colnames(WQ_Damar_E0502_2m)=="mean(temperature.C..2m.)"] <- "TempC"
colnames(WQ_Damar_E0502_2m)[colnames(WQ_Damar_E0502_2m)=="mean(gps_lat.degrees_north...0m.)"] <- "LATITUDE_DECIMAL"
colnames(WQ_Damar_E0502_2m)[colnames(WQ_Damar_E0502_2m)=="mean(gps_lon.degrees_west...0m.)"] <- "LONGITUDE_DECIMAL"
WQ_Damar_E0502_2m$YrMo<-format(as.Date(WQ_Damar_E0502_2m$Date, "%Y-%m-%d"),"%Y-%m")

WQ_Damar_E0502_2m$Depth_m<-"2"
WQ_Damar_E0502_2m$Station<-"UMO_E0502_sbe37"
WQ_Damar_E0502_2m$water_depth<-NA
head(WQ_Damar_E0502_2m)
############################ Sensor 0065 & 0066 - Mid and Upper LOBO Buoys
dam_LOBO2_Mid <- read.csv(paste(InputDataFolderPath,"NERACOOS/LOBO_CSV_65.csv",sep=""))
dam_LOBO1_Upper<-read.csv(paste(InputDataFolderPath,"NERACOOS/LOBO_CSV_66.csv",sep=""))
head(dam_LOBO1_Upper)
head(dam_LOBO2_Mid)

dam_LOBO2_Mid$DateTime<-as.POSIXlt(dam_LOBO2_Mid$date_EST, tz="EST", format="%Y-%m-%d %H:%M")
dam_LOBO2_Mid$Date<- strftime(dam_LOBO2_Mid$DateTime,"%Y-%m-%d")
dam_LOBO2_Mid$Station<-"LOBO2_Mid65"

dam_LOBO1_Upper$DateTime<-as.POSIXlt(dam_LOBO1_Upper$date_EST, tz="EST", format="%Y-%m-%d %H:%M")
dam_LOBO1_Upper$Date<- strftime(dam_LOBO1_Upper$DateTime,"%Y-%m-%d")
dam_LOBO1_Upper$Station<-"LOBO1_Upper66"

dam_LOBOs_MU<-rbind(dam_LOBO2_Mid,dam_LOBO1_Upper)

LOBOAvg <- dam_LOBOs_MU %>% 
  group_by(Date,Station) %>%
  summarise(mean(salinity_psu), mean(temperature_celsius), mean(latitude_degrees_north), mean(longitude_degrees_east), mean(depth_m), mean(water_depth))

LOBOAvg<-as.data.frame(LOBOAvg)

LOBOAvg<-LOBOAvg[complete.cases(LOBOAvg[ , c("mean(salinity_psu)","mean(temperature_celsius)" )]),]

colnames(LOBOAvg)[colnames(LOBOAvg)=="mean(salinity_psu)"] <- "Sal"
colnames(LOBOAvg)[colnames(LOBOAvg)=="mean(temperature_celsius)"] <- "TempC"
colnames(LOBOAvg)[colnames(LOBOAvg)=="mean(latitude_degrees_north)"] <- "LATITUDE_DECIMAL"
colnames(LOBOAvg)[colnames(LOBOAvg)=="mean(longitude_degrees_east)"] <- "LONGITUDE_DECIMAL"
colnames(LOBOAvg)[colnames(LOBOAvg)=="mean(depth_m)"] <- "Depth_m"
colnames(LOBOAvg)[colnames(LOBOAvg)=="mean(water_depth)"] <- "water_depth"

LOBOAvg$YrMo<-format(as.Date(LOBOAvg$Date, "%Y-%m-%d"),"%Y-%m")

############################ 
dam_BuoyMerge<-rbind(LOBOAvg,WQ_Damar_E0502_2m)

dam_BuoyMerge<-RemoveDuplicates(dam_BuoyMerge)
dam_BuoyMerge<-dam_BuoyMerge[order(dam_BuoyMerge$Date),]
rownames(dam_BuoyMerge) <- seq(length=nrow(dam_BuoyMerge))
#update IDs
dam_BuoyMerge$ID <- 1:nrow(dam_BuoyMerge)

dam_BuoyMerge$Zone<-"WQ"
dam_BuoyMerge<-dam_BuoyMerge[,c("ID","Station","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","Date","YrMo","TempC","Sal","Depth_m","water_depth","Zone")]

outputCSV<-paste(OutputCSVFolderPath,"dam_BuoyMerge.csv",sep="")
write.csv(dam_BuoyMerge,outputCSV,row.names=FALSE)