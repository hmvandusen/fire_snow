#This is caluculating Precip for an anual basis 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(terra)
library(dplyr)


#Things to manipulate 
name<-'CASCADE COMPLEX (MONUMENTAL)' #Must be all capitals 
years<-c(2012:2017)
fire.shp_path<-'/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/fire_ref_snowqgis.shp'
watershed<-'/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/watershed_firesnow.shp'
watershed_gageID<-'13313000'
#load in streamflow metrics table if needed

#Load in PRISM data
#how to batch load in PRISM data: is there a package?
#Note: load in the .bil files 
PRISM.ls<-as.list(NA)
filez <- dir("~/Desktop/PRISM_ppt_stable_4kmM3_2006_all_bil", recursive = TRUE, pattern = '\\.bil$') #the recursive should look through all folders
pathnm <- "~/Desktop/PRISM_ppt_stable_4kmM3_2006_all_bil/"
for(i in 1:length(filez)){
  PRISM.ls[[i]]<- rast(paste0(pathnm,filez[i]))
}

#Load in shapefiles

#Fire shapefile
allfires<-vect(fire.shp_path)
fire<-allfires[allfires$Incid_Name == name]
plot(fire)

#watershed shapefile
allwatersheds<-vect(watershed)
watershed<-allwatersheds[allwatersheds$GAGE_ID == watershed_gageID]
plot(watershed)
area<-watershed$AREA #units are in meters 

#Reproject to watershed 
crs<-crs(watershed)
fire.rp<-project(fire, crs)
PRISM.ls.rp<-lapply(PRISM.ls, function(x) terra::project(x, y = crs, method = 'bilinear'))

#Double check plot
plot(mask(crop(PRISM.ls.rp[[1]], watershed),watershed))
plot(watershed, add=T)
plot(fire.rp, add=T, col=rgb(1,0,0,.4))


prismdata<-data.frame(year = years)
for (i  in 1:length(years)) {
  prismdata$year[i] <- 
  prismdata$month[i] <- 
  prismdata$annualtot[i] <- 
  prismdata$meanPRISM[i]<-mean(terra::extract(PRISM.ls.rp[[i]], fire.rp)[,2])*0.0254*area #convert to inches to meters also have to conver (cfs)
  #multiply over the area of the watershed to get total discharge
  prismdata$sumPRISM[i]<-sum(terra::extract(PRISM.ls[[i]], fire)[,2])*0.0254*4000^2 #convert to cubic units as well cubic rain over watershed 
  #multiply by the resoluation of PRISM since I am am summing over the watershed
  prismdata$maxPRISM[i]<-max(terra::extract(PRISM.ls.rp[[i]], fire.rp)[,2])*0.0254*area
}
print(prismdata) 
#I don't know why the method of summing all the pixels in the watershed does work 
