######################################################################################
############################    GIS CLEANING MIC 2011   ##############################
######################################################################################
#Notes: MICs 2011 has only been processed at this point owing to inability to automate 
#loading of MODIS datasets. 2014 tiles are being loaded and processed currently and shall be
#uploaded when available. For more information regarding automated scripts for MODIS downloads
#please see MODISDownload.zip file in the github

#Clean environment
rm(list=ls(all=TRUE))

#install appropriate packages
list.of.packages <- c("dplyr", "rgdal", "raster", "RCurl", "tools", "gtools", "sp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Import libraries
library(dplyr)
library(rgdal)
library(raster)
library(RCurl)
library(tools)
library(gtools)
library(sp)
#Bring in original raster data
#NOTES: the data is messy in the sense they are not all cropped, they aren't in the same extent or projection
#before anything all the above needs to be handled
#a) Crop rasters
#b) resample to same extent (raster for altitude has the correct extent)
#c) reproject to appropriate projection (raster for Population Density has the correct one)

#Call the rasters in a list
rasters <-list.files("Data/Rasters/Original/", ".tif", full.names = T, recursive = T)
#Keep raster names
raster_names <- file_path_sans_ext(c( unlist( lapply(strsplit(rasters,"[/]"), FUN=function(x) { x[4] }))))
#Import Ghana Shapefile
Ghana <- shapefile("Data/Rasters/GHA_adm/GHA_adm0.shp")

#Use for loop to crop, resample extent and project raster and export to cleaned folder
#NOTE: this loops takes a while, an already run version is stored in "cleaned" folders
for(i in 1:length(rasters)){
  r=raster(rasters[i])
  r=crop(r, extent(Ghana))
  r=mask(r, Ghana)
  r=resample(r, raster("Data/Rasters/to_fix/Extent.tif"), resample='bilinear')   #altitude, will need to change this eventually
  r=projectRaster(r, raster("Data/Rasters/to_fix/Projection.tif"))       #pop den, will need to change this eventually
  show(r)
  destfile=paste0("Data/Rasters/cleaned/", raster_names[i],"_crop.tiff","")
  writeRaster(r,destfile , format="GTiff", overwrite=T)
}

#Create list of new cleaned rasters
list.rasters <-list.files("Data/Rasters/cleaned/", ".tif", full.names = T, recursive = T)
#Stack together the layers
all=stack(list.rasters)
#plot to see
plot(all)

#Bring in the .csv and convert to shapefile of points
MICS_gps <- read.csv("Data/GIS/MICS_mal.csv")
MICS_gps$ID=as.numeric(row.names(MICS_gps)) 
names(MICS_gps) 
projection <- crs(all) 
MICS_2011_loc <- SpatialPointsDataFrame(coords = MICS_gps[,4:5], data = MICS_gps, proj4string = projection) 
crs(MICS_2011_loc) 
plot(MICS_2011_loc, main="MICS locations") 

#Extract Raster values for the points
MICS_2011 <- extract(all, MICS_2011_loc, df=TRUE,method='simple')

#these value for GS model need to be scaled
MICS_2011=cbind(MICS_2011[1],scale(MICS_2011[,2:12]))

#Merge back to .csv file and export
MICS_gps %>% 
  left_join(MICS_2011, by="ID") %>% 
  write.csv("Data/Final_for analysis/MICS_2011.csv", row.names=F)