##This script clips mean SCF to a minimum of 25%
##Then clips fire shapefiles that lie in reference watersheds to the established 25% snowzone

library(terra)
library(raster)
library(sp)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load in mean SCF: values less than 0.25 go to NA

meanSCF<-rast("meanSCF.tif")
meanSCF[meanSCF < 0.25] <- NA 
summary(meanSCF)
writeRaster(meanSCF, "meanSCF25.tif")

#Load in Fires in ref watersheds and clip by SCF raster
fire_ref<-vect('fires_refwatersh.shp')

#trying crop method
#fire_ref_snow<-crop(fire_ref, meanSCF) # Just cropping to the extent of SCF not to specific values

#polygonize SCF
meanSCF.shp<-as.polygons(meanSCF)
#select using intersect tool
fire_ref_snow2<-crop(fire_ref,meanSCF.shp)

writeVector(fire_ref_snow2, "fire_ref_snow2.shp")
