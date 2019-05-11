########################################################################################################################
# Class of functions for working with DMR data
# Created by: mkimble
# Date Created: 10/23/2017
# LAST MODIFIED: 04/20/2019
#TheSourceDir<-dirname(rstudioapi::getActiveDocumentContext()$path)
#source(paste(TheSourceDir,"/DMRData_Functions.R",sep=""))
#######################################################################################################################
## Libraries
LibraryList<-c("rgeos","raster","rgdal","sp")
for (TheLibrary in LibraryList)
{
  if(TheLibrary %in% rownames(installed.packages()) == FALSE) install.packages(TheLibrary)
  
}
## GIS R Libraries
library(raster) # raster data
library(rgeos) # geometry ops
library(rgdal) # input/output, projections
library(sp) # vector data
#install.packages("spdep")
#library(spdep) #spatial dependence
###############################################################################################
# Functions to aid in the pre and post analysis of the DMR Shellfish Water Quality station data 
###############################################################################################
RemoveDuplicates <- function(InputData)
{
  ## check to remove any duplicate rows from the data.frame
  PreUniqueCheck_NumRow<-nrow(InputData)
  InputData<-unique(InputData)
  PostUniqueCheck_NumRow<-nrow(InputData)
  TheUniqueCheck_Diff<-PreUniqueCheck_NumRow-PostUniqueCheck_NumRow
  
  ## check for NA in data.frame
  NumNAs<-sapply(InputData, function(x) sum(is.na(x)))
  SumNumNAs<-sum(NumNAs)
  
  message("NumNAs: ", SumNumNAs, " | Duplicate Rows Removed: ",TheUniqueCheck_Diff)
  
  return(InputData)
}
###############################################################################################
# Functions to trim spaces and stuff
###############################################################################################
trim <- function (x) gsub("^\\s+|\\s+$|\r?\n|\r", "", x)

###############################################################################################
# Function to convert raster to polygon
# https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/
###############################################################################################
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}
###############################################################################################
# Functions to add random points within a defined space with NA areas excluded
###############################################################################################
CreateRandPointsNAPolySP<-function(TheUTMCoordinates,TheNAMask,NumPoints)
{
  ## create a polygon based on input coordinates
  Poly<-Polygon(TheUTMCoordinates)
  Polys<-Polygons(list(Poly),1)
  ThePolySHP <-SpatialPolygons(list(Polys))
  
  ## make sure that both the mask and circle have the same prj
  projection(ThePolySHP)<-projection(TheNAMask)
  
  ## clip the circle based on water
  TheMaskedPolySHP<-gIntersection(ThePolySHP,TheNAMask)
  #plot(TheWaterMask)
  #plot(TheMaskedCircleSHP,add=TRUE,col='red')
  
  ## select a random location within the clipped circle
  RandomPoints<-spsample(TheMaskedPolySHP,n=NumPoints,type="random",iter=100)
  
  return(RandomPoints)
}
###############################################################################################
# Functions to add random points within a defined space
###############################################################################################
CreateRandPointsPolySP<-function(TheUTMCoordinates,TheUTMPrj,NumPoints)
{
  ## create a polygon based on input coordinates
  Poly<-Polygon(TheUTMCoordinates)
  Polys<-Polygons(list(Poly),1)
  ThePolySHP <-SpatialPolygons(list(Polys))
  
  ## make sure that both the mask and circle have the same prj
  projection(ThePolySHP)<-TheUTMPrj
  
  ## select a random location within the clipped circle
  RandomPoints<-spsample(ThePolySHP,n=NumPoints,type="random",iter=100)
  
  return(RandomPoints)
}

###############################################################################################
# Functions to add field for easting/northing from lat/lon or vice versa
###############################################################################################
AddLatLonCoords<-function(InputData,EastingField,NorthingField,TheUTMPrj,TheGCSprj)
{
  ## adds lat/lon coordinates to the InputData based on wt_east and wt_north
  coords = cbind(InputData[[EastingField]], InputData[[NorthingField]])
  sp<-SpatialPoints(coords)
  sputm <- SpatialPoints(sp, proj4string=CRS(TheUTMPrj)) 
  spgeo <- spTransform(sputm, CRS(TheGCSprj))
  spgeo<-as(spgeo,"SpatialPointsDataFrame")
  InputData$lon<-spgeo$coords.x1
  InputData$lat<-spgeo$coords.x2
  return(InputData)
}
AddEastNorthCoords<-function(InputData,LonField,LatField,TheGCSprj,TheUTMPrj)
{
  ## adds easting/northing coordinates to the InputData based on wt_east and wt_north
  coords = cbind(InputData[[LonField]], InputData[[LatField]])
  sp<-SpatialPoints(coords)
  spgeo <- SpatialPoints(sp, proj4string=CRS(TheGCSprj)) 
  sputm <- spTransform(spgeo, CRS(TheUTMPrj))
  sputm<-as(sputm,"SpatialPointsDataFrame")
  InputData$Eastings<-sputm$coords.x1
  InputData$Northings<-sputm$coords.x2
  return(InputData)
}
###############################################################################################
# Function to overlay two histograms 
###############################################################################################
OverlayHist=function(OriginalRaster,Covariate, xlabel,redbrks,bluebrks,side,mtitle) 
{
  
  h1<-OriginalRaster
  h2<-Covariate
  
  # Histogram Colored (blue and red)
  hist(h1, col=rgb(1,0,0,0.5),freq=FALSE,breaks=redbrks, main=paste("Histogram Overlay",mtitle), xlab=xlabel)
  hist(h2, col=rgb(0,0,1,0.5),freq=FALSE,breaks=bluebrks, add=T)
  legend(side,c("Full Surface", "Sampled Surface"), fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
  box()
}
###############################################################################################
# Plot functions
