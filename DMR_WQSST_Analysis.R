####
## Script extract LSAT8 temp values 
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
               "raster","rstudioapi","ggplot2","reshape2","Cairo","mailR")
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
library(mailR)

###################################################################################
# Additional files (classes) that have functions that are used in this script.    #
###################################################################################
TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))
setwd("D:/Dropbox/00_Dissertation/01_Data/")

###################################################################################
# Log all print statemenets to txt document - also includes folderpaths
###################################################################################
FileNameDate<-as.character(format(Sys.Date(), "%Y%m%d"))
TodayDate<-as.character(format(Sys.Date(), "%m-%d-%Y"))

InputDataFolderPath<-"01_Original/SeaSurfaceTemp/CoastalSatLab/"
OutputDataFolderPath<-"02_Working/SeaSurfaceTemp/CoastalSatLab/NARm/"
CoordFolderPath<-paste(OutputDataFolderPath,"Coords",sep="")
BufferFolderPath<-paste(OutputDataFolderPath,"Buffer",sep="")

## had to add this in so that new csvs will go into a dated folder
OutputCSVFolderPath<-paste(OutputDataFolderPath,"CSV_ExtrTempVal/",FileNameDate,"/",sep="")
OutputDataFolderPathPNG<-paste("04_Figures/SeaSurfaceTemp/CoastalSatLab/NARm/",FileNameDate,"/",sep="")

TheDirs=c(OutputDataFolderPath,CoordFolderPath,BufferFolderPath,OutputCSVFolderPath,OutputDataFolderPathPNG)
## checks if the the directories exist, if they don't it creates them.
for (TheDir in TheDirs){
  DirectoryExists<-dir.exists(file.path(TheDir))
  if (DirectoryExists==FALSE) dir.create(file.path(TheDir), recursive = TRUE)
}

SinkFilename<-paste("00_PrintOutput_DMRData_Analysis_",FileNameDate,sep="")
OutputCSVFile<-paste(OutputCSVFolderPath,SinkFilename,".txt",sep="")
sink(OutputCSVFile,append=FALSE,split=TRUE)
###################################################################################
# Setup to send email notifications when my crap finishes.    
# To search for it, ctrl+f send.mail
###################################################################################
sender <- "spatialmsk@gmail.com"
recipients <- c("melissa.kimble@maine.edu")
TheSubject <- paste("DMRData_Analysis",TodayDate)
TheBody <- paste("Hey Homie,\n",TheSubject,"is done so you should go check and see if anything blew up.")
ThePasswrd <-"On3Cod3ToRul3Th3mAll"
MyUserName<-"spatialmsk@gmail.com"

###################################################################################
## setup
###################################################################################
OverWriteFiles = "no"

## grab the GCS prj from WQpolyjoin
TheGCSprj<-projection(readOGR("01_Original/Aquaculture/FromKate/Damariscotta/WQpolyjoin.shp"))
## grab the prj of the raster
UTMRaster<-raster("02_Working/SeaSurfaceTemp/CoastalSatLab/Mask/LC80110302013195LGN00_SST_WQMask.tif")
TheUTMprj<-projection(UTMRaster)

DMRDataWQ<-read.csv("02_Working/WaterQuality/State_DMR/WQData_Update.csv")
DMRDataWL<-read.csv("02_Working/WaterQuality/State_DMR/WLData_Update.csv")
DMRDataWQ$Zone<-"WQ"
DMRDataWL$Zone<-"WL"
DMRData<-data.frame()
DMRData<-rbind(DMRDataWQ,DMRDataWL)

#HistPlotAll <- data.frame(Station=character(),
#                          YrMo=as.Date(character()),
#                          Eastings=double(),
#                          Northings=double(),
#                          Type=character(),
#                          Temp=double(),
#                          Scene=character(),
#                          stringsAsFactors=FALSE)

## Function that appends PCS coordinates to GCS prjs.
DMRData<-AddEastNorthCoords(DMRData,"LONGITUDE_DECIMAL","LATITUDE_DECIMAL",TheGCSprj,TheUTMprj)
#DateField="Date"

TheScriptStartTime=Sys.time()
TheFilePaths <- list.files(path=InputDataFolderPath, pattern="\\.tif$", full.names=F, recursive=FALSE)
#TheFilePaths <- TheFilePaths[25:25] ## 25 only has 2 points you have to worry about, so it runs faster.

###################################################################################
## truncate dates based on selected range
#DMRData$YrMo2<-format(as.Date(DMRData$Date, "%Y-%m-%d"),"%Y-%m")
#DMRData_YrMoSub<-DMRData[(DMRData$YrMo2>="2014-05" & DMRData$YrMo2<="2014-11"),]
#names(DMRData)
#Dates<-as.vector(unique(DMRData_YrMoSub$Date))
###################################################################################

for (TheRaster in TheFilePaths) {
  TheFileStartTime<-Sys.time()
  ###################################################################################
  ## Sea surface temperature rasters SETUP
  ###################################################################################
  #TheRaster<-TheRasters[2]
  TheRaster<-as.character(TheRaster)
  #TheRaster<-trimws(TheRaster)
  print(paste("Starting :", TheRaster,sep=""))
  TheRasterFile<-paste(InputDataFolderPath,TheRaster,sep="")
  ###################################################################################
  ## WQ SHP setup
  ###################################################################################
  #################################################################################
  # Setup date time fields by parsing LSAT filename
  # We only want WQ samples for the same month and year as the SST
  #################################################################################
  TheRasterParse<-strsplit(TheRaster,"")
  PPPRRR<-as.character(paste(TheRasterParse[[1]][4:9],collapse=""))
  FullDate<-as.character(paste(TheRasterParse[[1]][10:16], collapse=""))
  #TheDate<-format(as.Date(FullDate, "%Y%j"),"%Y%m%d")
  SatYYYYMMDD<-format(as.Date(FullDate, "%Y%j"),"%Y-%m-%d")
  SatYrMo<-format(as.Date(FullDate, "%Y%j"),"%Y-%m")
  print(paste("Starting new SST: ",SatYrMo, " | ",TheRaster,sep=""))
  #################################################################################
  #this section setups up the date & time fields
  #DMRData$Date<-format(as.Date(DMRData[[DateField]], "%d-%b-%y"),"%m-%d-%Y")
  DMRData$YrMo<-format(as.Date(DMRData$Date, "%Y-%m-%d"),"%Y-%m")
  #DMRData$YrMo<-as.numeric(DMRData$YrMo)
  #DMRData$Year<-format(as.Date(DMRData[[DateField]], "%d-%b-%y"),"%Y")
  #DMRData$Year<-as.numeric(DMRData$Year)
  #DMRData$Month<-format(as.Date(DMRData[[DateField]], "%d-%b-%y"),"%m")
  #DMRData$Month<-as.numeric(DMRData$Month)
  DMRData_YrMoSub<-DMRData[(DMRData$YrMo==SatYrMo),]
  
  ## if no records match the sat image, then skip
  if (nrow(DMRData_YrMoSub) == 0) {
    TheScriptEndTime=Sys.time()
    ScriptDuration<-difftime(TheScriptEndTime,TheFileStartTime,"VET",units="mins")
    message(TheRaster," - Complete, Time Elapsed: ",round(ScriptDuration,2), " mins")
    next
  }
  #################################################################################
  ## if they're not, then just drop lat/lon from the dataset that are 0
  #DMRData_YrMoSub<-DMRData_YrMoSub[!(DMRData_YrMoSub$LATITUDE_D==0 & DMRData_YrMoSub$LONGITUDE_==0),]
  #DMRData_YrMoSub<-RemoveDuplicates(DMRData_YrMoSub)
  #typeof(SST_Raster)
  
  #################################################################################
  ## TAKE THIS DATA AND REMOVE NA FECA COLIFORM VALUES AND GET UNIQUE
  ## COMBINATIONS OF EASTINGS AND NORTHINGS AND DATE
  ## FECAL COLIFORM, TEMP, SAL, STATION, 
  #################################################################################
  ## convert DMRData_YrMoSub into a dataframe to keep for df stuff
  DMRData_YrMoSub_df<-as.data.frame(DMRData_YrMoSub)
  DMRData_Sub_df<-DMRData_YrMoSub_df[!is.na(DMRData_YrMoSub_df$RawColScore),]
  DMRData_Sub_df<-unique(DMRData_Sub_df[,c("Station","Date","Eastings","Northings","LATITUDE_DECIMAL","LONGITUDE_DECIMAL",
                       "RawColScore","Sal","Temp")])
  #################################################################################
  ## ADD FOR LOOP HERE TO CHECK FOR -100 RASTERS!
  #################################################################################
  ##DOES EXIST?
  TheRasterFileName<-strsplit(TheRaster,"_")[[1]][1]
  FileName<-paste(TheRasterFileName,"_NARm",sep="")
  SST_NARm_FileName<-paste(OutputDataFolderPath,FileName,".tif",sep="")
  if (file.exists(SST_NARm_FileName) & OverWriteFiles=="no") {
    print (paste("Already Exists: ",FileName, sep=""))
    SST_Raster<-raster(SST_NARm_FileName)
  } else {
    print (paste("Does Not Exist: ",FileName, sep=""))
    ###################################################################################
    ## Sea surface temperature rasters SETUP
    ###################################################################################
    ## the SST rasters have -238 as the NA value, but it's not consistent, therefore since values less than -100 are impossible
    ## in Maine, all values less than -100 are converted to NA  
    SST_Raster<-raster(TheRasterFile)
    ## the SST rasters have -238 as the NA value, but it's not consistent, therefore since values less than -100 are impossible
    ## in Maine, all values less than -100 are converted to NA  
    SST_Raster[SST_Raster < -100] <- NA
    writeRaster(SST_Raster,SST_NARm_FileName,overwrite=TRUE)
  }

  TheUTMprj<-projection(SST_Raster)
  
  #SST_Raster@crs
  #SST_Raster@extent
  #xmin<-SST_Raster@extent[1]
  #xmax<-SST_Raster@extent[2]
  #ymin<-SST_Raster@extent[3]
  #ymax<-SST_Raster@extent[4]
  #xdim<-xmax-xmin
  #ydim<-ymax-ymin
  #SST_Raster<-setMinMax(SST_Raster)
  #MinVal<-cellStats(SST_Raster,min)
  #MaxVal<-cellStats(SST_Raster,max)
  #cellStats(SST_Raster,range)
  ## I know that the min value == NA, so..
  #SST_Raster[SST_Raster<=MinVal]<-NA
  
  ## make a histogram of SST values and a plot of it
  #hist(SST_Raster, main="Dist. of SST Values", col="blue",maxpixels=22000000)

  
  ## Convert DMRData_YrMoSub into spatial data
  ## does same thing as coordinates(DMRData_YrMoSub)
  #coords = cbind(DMRData_YrMoSub[[LonField]], DMRData_YrMoSub[[LatField]])
  #sp<-SpatialPoints(coords)
  
  coordinates(DMRData_YrMoSub)<- ~Eastings+Northings
  projection(DMRData_YrMoSub)<-TheUTMprj
  
  DMRData_YrMoSub_utm <- SpatialPoints(DMRData_YrMoSub, proj4string=CRS(TheUTMprj)) 

  
  ## subset the data to only include Eastings and Northings and remove duplicate locations. Later sites will be joined, but right
  ## now it just slows down the computation. 
  coords<-unique(DMRData_YrMoSub_df[,c("Eastings","Northings")])
  
  NumCoords<-nrow(coords)
  print(paste("Number of unique coordinates: ",NumCoords, sep=""))

## THIS IS BAD, it forces the extent of SST to overlap with the coordinate.  
# if (NumCoords > 1)
#  {
#    extent(SST_Raster)<-extent(DMRData_YrMoSub_utm)*1.25
#  } else {
#    xmin<-(DMRData_YrMoSub_utm@coords[1])-(DMRData_YrMoSub_utm@coords[1]*0.025)
#    xmax<-(DMRData_YrMoSub_utm@coords[1])+(DMRData_YrMoSub_utm@coords[1]*0.025)
#    ymin<-DMRData_YrMoSub_utm@coords[3]-(DMRData_YrMoSub_utm@coords[3]*0.025)
#    ymax<-DMRData_YrMoSub_utm@coords[3]+(DMRData_YrMoSub_utm@coords[3]*0.025)
#    extent(SST_Raster)<-extent(xmin,xmax,ymin,ymax)
#  }
  
  ## set the raster extent to the DMR extent - easier to see visually since 
  ## the extent of the raster tends to be greater than the extent of the points


  #################################################################################
  ## WRITE POINTS TO SHP
  coords_sp<- SpatialPointsDataFrame(coords, coords) 
  projection(coords_sp)<-projection(TheUTMprj)
  TheCoordFileName<-paste(TheRasterFileName,"_DMRCoords",sep="")
  writeOGR(coords_sp, CoordFolderPath, TheCoordFileName, driver="ESRI Shapefile",overwrite_layer=TRUE)
  #################################################################################
  xmin<-(DMRData_YrMoSub_utm@coords[1])-(DMRData_YrMoSub_utm@coords[1]*0.05)
  xmax<-(DMRData_YrMoSub_utm@coords[1])+(DMRData_YrMoSub_utm@coords[1]*0.05)
  ymin<-DMRData_YrMoSub_utm@coords[3]-(DMRData_YrMoSub_utm@coords[3]*0.05)
  ymax<-DMRData_YrMoSub_utm@coords[3]+(DMRData_YrMoSub_utm@coords[3]*0.05)
  
  ## if the extents of the coordinates don't overlap with The SST raster, then skip to the next raster
  DoesOverlap<-intersect(extent(SST_Raster), extent(coords_sp))
  if (is.null(DoesOverlap)) {
    TheScriptEndTime=Sys.time()
    ScriptDuration<-difftime(TheScriptEndTime,TheFileStartTime,"VET",units="mins")
    message(TheRaster," - Complete, Time Elapsed: ",round(ScriptDuration,2), " mins")
    next()
  }
  UniqueDMRDates<-as.character(unique(DMRData_Sub_df$Date))
  UniqueDMRDates<-paste(UniqueDMRDates, collapse=", ")
  
  TheRasterName<-gsub(".tif","",TheRaster)
  TheYrMoName<-gsub("-","",SatYrMo)
  FileName<-paste(TheRasterName,"_",TheYrMoName,sep="")
  ThePlotOutputFile<-paste(OutputDataFolderPathPNG,FileName,".png", sep="")
  ## nice plots with antialiasing http://gforge.se/2013/02/exporting-nice-plots-in-r/
  Cairo(ThePlotOutputFile,type="png",units="in",width=12,height=10,res=300, pointsize=12,bg="white")
  
  SST_Raster_cr <- crop(SST_Raster, extent(coords_sp)*1.5, snap="out")                    
  
  col<-brewer.pal(7,"Blues")
  PlotTitle<-paste(TheRasterName, "| Sea Surface Temp:",SatYYYYMMDD,"\n","DMR Collection Dates:",UniqueDMRDates)
  
  plot(SST_Raster_cr, main=PlotTitle, col=col)
  ## plot DMRData_YrMoSub_utm
  #points(coords, pch=19)
  #text(x = coords$Eastings, y = coords$Northings, labels = as.character(1:nrow(coords)), pos=1, cex=0.7, xpd=NA)
  #print(PlotTitle)
  
  print("Starting Extracted...")
  
  # use normal extract function to show that NAs are Extracted for some points
  Extracted = raster::extract(x = SST_Raster, y = coords)
  PercNonNA<-(sum(!is.na(Extracted))/(length(Extracted)))*100
  print(paste("% of Non NA Extracted values:",PercNonNA, sep=""))
  
  if (PercNonNA == 100)
  {
    ## all coordinates are within non-na pixels
    dev.off()
    Dist_Df<-data.frame(coords,Extracted)
    Dist_Df$NNPixel<-"isExtracted"
    Dist_Df$buffDist<-"isExtracted"
    Dist_Df$SSTBuffTemp<-"isExtracted"
    
    colnames(Dist_Df)<-c("Eastings", "Northings", "Extracted", "NNPixel", "buffDist","SSTBuffTemp")
    Output<-merge(Dist_Df,DMRData_Sub_df, by.x=c("Eastings","Northings"), by.y=c("Eastings","Northings"))
  } else if (PercNonNA == 0) {
    ## all coordinates extracted sst were NA, meaning 0 overlap
    ## If they're all NA, then we have to check if they're close at all with any pixel values
    NAcoords<-data.frame(coords)
    print("Starting buffDist...")
    
    # then take the raster value with lowest distance to point AND non-NA value in the raster
    buffDist = apply(X = NAcoords, MARGIN = 1, FUN = function(NAcoords) distanceFromPoints(SST_Raster,NAcoords)[which.min(replace(distanceFromPoints(SST_Raster, NAcoords), is.na(SST_Raster), NA))])
    
    MinBuffDist<-min(buffDist)
    print(paste("Min BuffPixel Distance:", MinBuffDist))
    
    if (MinBuffDist>500){
      NAcoords<-data.frame(NAcoords,buffDist)
      NAcoords_sp<-NAcoords
      coordinates(NAcoords_sp)<- ~Eastings+Northings
      projection(NAcoords_sp)<-TheUTMprj
      ## create a multi-distance buffer based on the distance to nearest pixel + maximum radius of pixel (based on resolution)
      buff_pnts<-gBuffer(NAcoords_sp, width=NAcoords_sp$buffDist, byid=TRUE)
      ## none of the coordinates are closer than 500 meters, so we're going to skip this scene.
      plot(buff_pnts, add=T)
      dev.off()
      TheScriptEndTime=Sys.time()
      ScriptDuration<-difftime(TheScriptEndTime,TheFileStartTime,"VET",units="mins")
      message(TheRaster," - Complete, Time Elapsed: ",round(ScriptDuration,2), " mins")
      next()
    } else {
   
      maxRad<-sqrt(yres(SST_Raster)^2+xres(SST_Raster)^2)
      ## add maximum radius to buff distance - The distance doesn't seem to encompass the next nearest pixel, but only compute
      ## distance to the edge of the nearest non-na pixel. Therefore, the maximum radius of a pixel (based on the resolution of the raster)
      ## is added to the buffered distance to avoid buffered extractions with only NA values. In the future ..
      ## incrementally increase the distance until a value is found for each pixel (ML, MCMC?)
      ## on second thought, this would take forever. It's fine the way it is.
      buffDist<-buffDist+maxRad
      buffDist<-ceiling(buffDist)
      
      print("Starting NNPixel... ")
      NNPixel = apply(X = NAcoords, MARGIN = 1, FUN = function(NAcoords) readAll(SST_Raster)[which.min(replace(distanceFromPoints(SST_Raster, NAcoords), is.na(SST_Raster), NA))])
      
      PercNA<-sum(is.na(NNPixel))/(length(NNPixel))
      print(PercNA)  
      
      ExtrNA<-rep_len(NA,nrow(NAcoords))
      Dist_Df<-data.frame(NAcoords,ExtrNA,NNPixel,buffDist)
      colnames(Dist_Df)<-c("Eastings", "Northings", "Extracted", "NNPixel", "buffDist")
      Dist_sp<-Dist_Df
      coordinates(Dist_sp)<- ~Eastings+Northings
      projection(Dist_sp)<-TheUTMprj
      ## create a multi-distance buffer based on the distance to nearest pixel + maximum radius of pixel (based on resolution)
      buff_pnts<-gBuffer(Dist_sp, width=Dist_sp$buffDist, byid=TRUE)
      
      #################################################################################
      ## WRITE BUFFER TO SHP
      TheCoordFileName<-paste(TheRasterFileName,"_DMRBuffer",sep="")
      writeOGR(buff_pnts, BufferFolderPath, TheCoordFileName, driver="ESRI Shapefile",overwrite_layer=TRUE)
      #################################################################################

      print("Starting ExtractBuff...")
      
      ## extract all pixel values within each buffer
      ExtractedBuff = raster::extract(x = SST_Raster, y = buff_pnts)
      ## remove all NA's from the extracted values
      ExtractedBuff<-lapply(ExtractedBuff, function(x) x[!is.na(x)])
      #################################################################################
      
      
      #################################################################################
      #################################################################################
      ## median each extracted set of buffered values
      ExtractedBuff_median<-ExtractedBuff
      ExtractedBuff_median[] <- lapply(ExtractedBuff,median)
      ## convert list of lists to a vector
      ExtractedBuff_median<-unlist(ExtractedBuff_median)
      ## add averaged multi-buffered values to dist_df
      Dist_Df_plot<-Dist_Df
      Dist_Df_plot$BuffMedian<-ExtractedBuff_median
      Dist_Df_plot<-merge(Dist_Df_plot,DMRData_Sub_df, by.x=c("Eastings","Northings"), by.y=c("Eastings","Northings"))
      
      Dist_Df_plot$TempDiff<-ifelse(is.na(Dist_Df_plot$Extracted), as.double(Dist_Df_plot$Temp) - as.double(Dist_Df_plot$BuffMedian), as.double(Dist_Df_plot$Temp) - as.double(Dist_Df_plot$Extracted))
      Dist_Df_plot$TempDiff<-round(Dist_Df_plot$TempDiff,2)
      ## plot DMRData_YrMoSub_utm
      #Create a function to generate a continuous color palette
      rbPal <- colorRampPalette(c('blue','white','red'))
      #This adds a column of color values
      # based on the y values
      Dist_Df_plot$Col <- rbPal(10)[as.numeric(cut(Dist_Df_plot$TempDiff,breaks = 10))]
      points(Dist_Df_plot$Eastings,Dist_Df_plot$Northings , pch=20, col = Dist_Df_plot$Col)
      text(x = Dist_Df_plot$Eastings, y = Dist_Df_plot$Northings, labels = as.character(Dist_Df_plot$TempDiff), pos=1, cex=0.7, xpd=NA)
      print(PlotTitle)
      plot(buff_pnts, add=T)
      dev.off()
      #################################################################################
      #################################################################################
      
      
      ## TAKE THESE VALUES, FOR LOOP THROUGH THE LIST [[]] AND CBIND ADD VALUES TO BE DUPLICATED
      ## AS "ROWS", THEN RBIND TO NEW DATAFRAME
      rows<-data.frame()
      ExtractedBuff_df<-data.frame()
      #coords
      #NAcoords
      for (i in 1:length(ExtractedBuff)){
        rows<-cbind(ExtractedBuff[[i]])
        if (nrow(rows) == 0) {
          rows<-merge(Dist_Df[i,],NA)
          colnames(rows)<-c("Eastings","Northings","Extracted","NNPixel","buffDist","SSTBuffTemp")
          ExtractedBuff_df<-rbind(ExtractedBuff_df,rows)
        } else {
          rows<-merge(Dist_Df[i,],rows)
          colnames(rows)<-c("Eastings","Northings","Extracted","NNPixel","buffDist","SSTBuffTemp")
          ExtractedBuff_df<-rbind(ExtractedBuff_df,rows)
        }
      }
      #################################################################################
      ## MERGE 
      Output<-merge(ExtractedBuff_df,DMRData_Sub_df, by.x=c("Eastings","Northings"), by.y=c("Eastings","Northings"))
    } 
  } else {
  ## More than one coordinate within the year-mo subset, so the data needs to be split into two data.frames (non-na/na) 
  ## and buffers need to be made. I split na and non-na because it speeds up the script by like 80%
  #################################################################################
  ## SUBSET AND ONLY RUN ON NA COORDINATES, SINCE I ALREADY HAVE VALS FOR OTHERS!!
  NAcoords<-data.frame(coords,Extracted)
  NotNACoords<-NAcoords[!is.na(Extracted),]
  NAcoords<-NAcoords[is.na(Extracted),]
  NAcoords<-NAcoords[,1:2]
  #################################################################################
  
  print("Starting buffDist...")
  
  # then take the raster value with lowest distance to point AND non-NA value in the raster
  buffDist = apply(X = NAcoords, MARGIN = 1, FUN = function(NAcoords) distanceFromPoints(SST_Raster,NAcoords)[which.min(replace(distanceFromPoints(SST_Raster, NAcoords), is.na(SST_Raster), NA))])
  
  maxRad<-sqrt(yres(SST_Raster)^2+xres(SST_Raster)^2)/2
  ## add maximum radius to buff distance - The distance doesn't seem to encompass the next nearest pixel, but only compute
  ## distance to the edge of the nearest non-na pixel. Therefore, the maximum radius of a pixel (based on the resolution of the raster)
  ## is added to the buffered distance to avoid buffered extractions with only NA values. In the future ..
  ## incrementally increase the distance until a value is found for each pixel (ML, MCMC?)
  ## on second thought, this would take forever. It's fine the way it is.
  buffDist<-buffDist+maxRad
  
  print("Starting NNPixel...")
  NNPixel = apply(X = NAcoords, MARGIN = 1, FUN = function(NAcoords) readAll(SST_Raster)[which.min(replace(distanceFromPoints(SST_Raster, NAcoords), is.na(SST_Raster), NA))])
 
  PercNA<-sum(is.na(NNPixel))/(length(NNPixel))
  print(PercNA)
  
  ExtrNA<-rep_len(NA,nrow(NAcoords))
  Dist_Df<-data.frame(NAcoords,ExtrNA,NNPixel,buffDist)
  colnames(Dist_Df)<-c("Eastings", "Northings", "Extracted", "NNPixel", "buffDist")
  Dist_sp<-Dist_Df
  coordinates(Dist_sp)<- ~Eastings+Northings
  projection(Dist_sp)<-TheUTMprj
  ## create a multi-distance buffer based on the distance to nearest pixel + maximum radius of pixel (based on resolution)
  buff_pnts<-gBuffer(Dist_sp, width=Dist_sp$buffDist, byid=TRUE)
  
  #################################################################################
  ## WRITE BUFFER TO SHP
  TheCoordFileName<-paste(TheRasterFileName,"_DMRBuffer",sep="")
  writeOGR(buff_pnts, BufferFolderPath, TheCoordFileName, driver="ESRI Shapefile",overwrite_layer=TRUE)
  #################################################################################
  
  print("Starting ExtractBuff...")
  
  ## extract all pixel values within each buffer
  ExtractedBuff = raster::extract(x = SST_Raster, y = buff_pnts)
  ## remove all NA's from the extracted values
  ExtractedBuff<-lapply(ExtractedBuff, function(x) x[!is.na(x)])
  
  #################################################################################
  ## TAKE THESE VALUES, FOR LOOP THROUGH THE LIST [[]] AND CBIND ADD VALUES TO BE DUPLICATED
  ## AS "ROWS", THEN RBIND TO NEW DATAFRAME
  rows<-data.frame()
  ExtractedBuff_df<-data.frame()
  AllSSTExtrTempVals_df<-data.frame()
  #coords
  #NAcoords
  for (i in 1:length(ExtractedBuff)){
    rows<-cbind(ExtractedBuff[[i]])
    if (nrow(rows) == 0) {
      rows<-merge(Dist_Df[i,],NA)
      colnames(rows)<-c("Eastings","Northings","Extracted","NNPixel","buffDist","SSTBuffTemp")
      ExtractedBuff_df<-rbind(ExtractedBuff_df,rows)
    } else {
      rows<-merge(Dist_Df[i,],rows)
      colnames(rows)<-c("Eastings","Northings","Extracted","NNPixel","buffDist","SSTBuffTemp")
      ExtractedBuff_df<-rbind(ExtractedBuff_df,rows)
    }
  }
  NotNACoords$NNPixel<-"isExtracted"
  NotNACoords$buffDist<-"isExtracted"
  NotNACoords$SSTBuffTemp<-"isExtracted"
  
  AllSSTExtrTempVals_df<-rbind(NotNACoords,ExtractedBuff_df)
  
  #################################################################################
  Dist_Df_plot_merge<-data.frame()
  #################################################################################
  ## median each extracted set of buffered values
  ExtractedBuff_median<-ExtractedBuff
  ExtractedBuff_median[] <- lapply(ExtractedBuff,median)
  ## convert list of lists to a vector
  ExtractedBuff_median<-unlist(ExtractedBuff_median)
  ## add averaged multi-buffered values to dist_df
  Dist_Df_plot<-Dist_Df
  Dist_Df_plot$BuffMedian<-ExtractedBuff_median
  colnames(Dist_Df_plot)<-c("Eastings","Northings","Extracted","NNPixel","buffDist","SSTBuffTemp")
  
  Dist_Df_plot_merge<-rbind(NotNACoords,Dist_Df_plot)
  
  Dist_Df_plot_merge<-merge(Dist_Df_plot_merge,DMRData_Sub_df, by.x=c("Eastings","Northings"), by.y=c("Eastings","Northings"))
  
  Dist_Df_plot_merge$TempDiff<-ifelse(is.na(Dist_Df_plot_merge$Extracted), as.double(Dist_Df_plot_merge$Temp) - as.double(Dist_Df_plot_merge$SSTBuffTemp), as.double(Dist_Df_plot_merge$Temp) - as.double(Dist_Df_plot_merge$Extracted))
  Dist_Df_plot_merge$TempDiff<-round(Dist_Df_plot_merge$TempDiff,2)
  
  ## plot DMRData_YrMoSub_utm
  #Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('blue','white','red'))
  #This adds a column of color values
  # based on the y values
  Dist_Df_plot_merge$Col <- rbPal(10)[as.numeric(cut(Dist_Df_plot_merge$TempDiff,breaks = 10))]
  points(Dist_Df_plot_merge$Eastings,Dist_Df_plot_merge$Northings , pch=20, col = Dist_Df_plot_merge$Col)
  text(x = Dist_Df_plot_merge$Eastings, y = Dist_Df_plot_merge$Northings, labels = as.character(Dist_Df_plot_merge$TempDiff), pos=1, cex=0.7, xpd=NA)
  print(PlotTitle)
  plot(buff_pnts, add=T)
  dev.off()
  #################################################################################
  #################################################################################

  ## MERGE 
  Output<-merge(AllSSTExtrTempVals_df,DMRData_Sub_df, by.x=c("Eastings","Northings"), by.y=c("Eastings","Northings"))
  }
  
  ## add column for satellite date
  Output$SatYYYYMMDD<-SatYYYYMMDD
  #################################################################################
  rownames(Output)<-seq(length=nrow(Output))
  ## rename columns
  colnames(Output)<-c("Eastings","Northings","SSTExtracTemp","NNPixel","buffDist","SSTBuffTemp","Station","DMRDate",
                      "LATITUDE_DECIMAL","LONGITUDE_DECIMAL","DMRRawColScore","DMRSal","DMRTemp","SatYYYYMMDD")
  ## rearrange data.frame
  Output<-Output[,c("Station","Eastings","Northings","LATITUDE_DECIMAL","LONGITUDE_DECIMAL","DMRDate",
           "SatYYYYMMDD","DMRRawColScore","DMRSal","DMRTemp","SSTExtracTemp","SSTBuffTemp","NNPixel","buffDist")]
  Output$TheRaster<-TheRaster
  OutputCSVFile<-paste(OutputCSVFolderPath, TheRasterFileName, "_SSTDMR_ExtrTempVal.csv", sep="")
  
  write.csv(Output, file=OutputCSVFile,quote=FALSE,row.names=FALSE)
  
#  Output<-merge(DMRData_YrMoSub_df,Dist_Df, by.x=c("Eastings","Northings"), by.y=c("Eastings","Northings"))
#  names(Output)[names(Output) == 'Temp'] <- 'DMRTemp'
  
#  print("Starting HistPlot...")
  
#  HistPlot<-Output[c("Station","YrMo","Eastings","Northings","DMRTemp","NNPixel","BuffAvg")]
#  HistPlot<-melt(HistPlot, id.vars=c("Station","YrMo","Eastings","Northings"), measure.vars=c("DMRTemp","NNPixel","BuffAvg"),
#             value.name="Temp", variable.name="Type")
  
  ## resampled values at different dates at DMR locations will have different temp values. However,
  ## resampled values at the same locations for LSAT8 SST (NNPixel and BuffAvg) will always have duplicate
  ## values at the same location, therefore all locations were averaged. 
#  HistPlot<-aggregate(. ~ Station + YrMo + Eastings + Northings  + Type, data = HistPlot, FUN = mean)
#  HistPlot<-HistPlot[c("Station","YrMo","Eastings","Northings","Type","Temp")]
#  HistPlot$Scene<-TheRaster
  
#  HistPlotAll<-rbind(HistPlotAll,HistPlot)
  TheScriptEndTime=Sys.time()
  ScriptDuration<-difftime(TheScriptEndTime,TheFileStartTime,"VET",units="mins")
  message(TheRaster," - Complete, Time Elapsed: ",round(ScriptDuration,2), " mins")
}

# write.csv(HistPlotAll,"02_Working/SST_DMR_LSAT8/HistPlotAll.csv",row.names=FALSE)

#################################################################################
## Make it rain with some mail! Send mail notification to say that the script has finished.
mailR::send.mail(from = sender,
                 to = recipients,
                 subject = TheSubject,
                 body = TheBody,
                 smtp = list(host.name = "smtp.gmail.com", port = 465, 
                             user.name = MyUserName,            
                             passwd = ThePasswrd, ssl = TRUE),
                 authenticate = TRUE,
                 send = TRUE)
#################################################################################

TheScriptEndTime=Sys.time()
ScriptDuration<-difftime(TheScriptEndTime,TheScriptStartTime,"VET",units="mins")
message("Script Complete, Time Elapsed: ",round(ScriptDuration,2), " mins")

## End logging and write to file.
warnings()
sink()

## overlay histogram of extacted values to compare DMR to LSAT8 SST
#ggplot(HistPlot,aes(x=Temp)) + 
#  geom_histogram(data=subset(HistPlot,Type == 'DMRTemp'),fill = "red", alpha = 0.2) +
#  geom_histogram(data=subset(HistPlot,Type == 'NNPixel'),fill = "blue", alpha = 0.2) +
#  geom_histogram(data=subset(HistPlot,Type == 'BuffAvg'),fill = "green", alpha = 0.2) +
#  ggtitle(PlotTitle)

#ggplot(HistPlot, aes(Temp, fill=Type)) +
#  geom_density(alpha=0.2) +
#  ggtitle(PlotTitle)


#ggplot(HistPlot, aes(Temp, fill = Type)) + 
#  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') +
#  ggtitle(PlotTitle)

#d<-density(HistPlot$Temp[HistPlot$Type=="NNPixel"])
#plot(d, main="Kernel Density of NNPixel")
#polygon(d, col="cyan", border="black")
#hist(HistPlot$Temp[HistPlot$Type=="NNPixel"], breaks=13)


## NA's in Extracted are from points that did not fall on a raster cell with a value
## Therefore, we're not interested in making a buffer for points that fell within
## a cell, but only for those that were x distance from a cell. This is to compare
## nearest pixel distributions with a multi-buffered average of pixel distributions
#tEx<-ifelse(is.na(Extracted), 1, NA)
#buffDist<-buffDist*tEx
#buffDist[is.na(buffDist)]<-0
#buffDist

