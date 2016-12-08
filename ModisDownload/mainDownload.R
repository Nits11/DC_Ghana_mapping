################################################################################  
# Download modis
# 
# date: 03/25/2015
# author: Daniel Perondi (dperondi@ufl.edu)
#
################################################################################

# clean process
rm(list=ls(all=TRUE))

#libraries
library(raster)
library(RCurl)
library(plyr)
library(rgdal)
library(maptools)
library(rts)
require(sp)
library(xts)
library(doBy)
library(parallel)
library(base)
library(png)
library(jpeg)
library(grid)
library(lattice)
library(rgeos)        
library(latticeExtra)
library(rjson)


################################################################################ 
############################### Config file ##############################

jsonConfig <- "~/Documents/ModisDownload/modisDownload.json"

################################################################################ 

################################################################################ 
############################### Functions ######################################

# Get the destination directory
getDestinationDirectory <- function(){
  return(destinationDirectory)
}

# Set the destination directory
setDestinationDirectory <- function(directory){
  destinationDirectory <<- directory
}

################################################################################ 


# Read config data
modisConfig <- fromJSON(paste(readLines(jsonConfig), collapse=""))
  
# setting the working directory:
setwd(modisConfig$modis$wd)

# needs when MRT not find the dir (Bug of linux)
Sys.setenv(MRT_DATA_DIR  = modisConfig$modis$mrt$dataDir)

# Global variable with directory of data
destinationDirectory <<- NULL

# loading the source of function (the script file should be copied in the working directory):
source('ModisDownload.R')

# product list:
#modisProducts()
#x=1 # or x="MOD13Q1"                                      
#x2=52 # x2="MYD13Q1"

# MODIS processing: 
# TERRA ; NDVI/EVI,blue,red,NIR,MIR,day composition
# documentation : http://r-gis.net/?q=ModisDownload

for(i in 1:length(modisConfig$modis$download)){
  
  print(i)
  
  product              <- modisConfig$modis$download[[i]]$product
  version              <- modisConfig$modis$download[[i]]$version
  horizontalTileNumber <- modisConfig$modis$download[[i]]$horizontalTileNumber
  verticalTileNumber   <- modisConfig$modis$download[[i]]$verticalTileNumber
  mrtPath              <- modisConfig$modis$mrt$path
  ul                   <- modisConfig$modis$download[[i]]$ul
  lr                   <- modisConfig$modis$download[[i]]$lr
  bandsSubset          <- modisConfig$modis$download[[i]]$bandsSubset
  pixelSize            <- modisConfig$modis$download[[i]]$pixelSize
  satellite            <- modisConfig$modis$download[[i]]$satellite
  
  setDestinationDirectory(modisConfig$modis$download[[i]]$destination)
  
  if(file.exists(getDestinationDirectory())){
    filesOfDirectory <- list.files(path = getDestinationDirectory(), pattern = glob2rx(paste(satellite,"*.tif$",sep="")))
    if(length(filesOfDirectory) == 0){
      lastDateWithData <-  as.POSIXlt("2014-09-01")
    }else{
      datesOfDirectory <- strptime(substr(filesOfDirectory,9,18),"%Y-%m-%d",tz = "UTC")
      lastDateWithData <- max(datesOfDirectory)
      lastDateWithData$mday <- lastDateWithData$mday + 1
    }
    
   }else{
    dir.create(file.path(getDestinationDirectory()), showWarnings = FALSE)
    lastDateWithData <-  as.POSIXlt("2014-12-31")
  }
  
  today <- as.POSIXlt(Sys.Date())
  
  # List of dates that will be searched
  daysToBeSearched <- seq(lastDateWithData, today, by = "days")
  
  yyyymm <- unique(format(daysToBeSearched,"%Y-%m"))

  if(length(yyyymm) > 0){
    
    for(k in 1:length(yyyymm)){
      
      beginDate <- strptime(paste(yyyymm[k],"-01",sep=""),"%Y-%m-%d")
      nextMonth <- seq(beginDate, length=2, by='1 month')[2]
      endDate <- seq(nextMonth, length=2, by='-1 day')[2]
      
      print(yyyymm[k])
      
      beginDate <- as.character(format(beginDate,"%Y.%m.%d"))
      endDate <- as.character(format(endDate,"%Y.%m.%d"))
      
      try(
        ModisDownload(
          x = product,
          h = horizontalTileNumber,
          v = verticalTileNumber,
          version = version,
          dates = c(beginDate,endDate),
          MRTpath = mrtPath,
          mosaic = TRUE,
#         delete = TRUE,
          proj = TRUE,
          UL = ul,
          LR = lr,
          bands_subset = bandsSubset, 
          proj_type = "GEO",
          datum = "WGS84",
          pixel_size = pixelSize
        )
      )
      filesToRemove <- list.files(path = getDestinationDirectory(), pattern = "\\.(prm|hdf)$")
      file.remove(file.path(getDestinationDirectory(), filesToRemove))  
    }  
  }
}

########################
# 
# ModisDownload(
#   x = 1,
#   h = c(8,9,10,11,12,13),
#   v = c(4,5,6),
#   version = "006",
#   dates = "2016.01.01"
# )
# 
# hdfFiles <- list.files("all-paraguay/")
# 
# mosaicHDF(hdfNames=hdfFiles, filename='2016-01-01.hdf', MRTpath=mrtPath, bands_subset="1 1 0 0 0 0 0 0 0 0 1 0", delete=FALSE)
# 
# reprojectHDF( 
#              proj_type = "GEO",
#              datum = "WGS84",
#              pixel_size = pixelSize)
# 
# 
# ModisDownload(
#   x = product,
#   h = c(8,9,10,11,12),
#   v = c(4,5,6),
#   version = "006",
#   dates = "2016.01.01",
#   MRTpath = mrtPath,
#   mosaic = TRUE,
#   delete = TRUE,
#   proj = TRUE,
# #  UL = ul,
# #  LR = lr,
#   bands_subset = "1 1 0 0 0 0 0 0 0 0 1 0", 
#   proj_type = "GEO",
#   datum = "WGS84",
#   pixel_size = pixelSize
# )
