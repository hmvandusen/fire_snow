

###
### Watershed statistics 
###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))##set working directory to current rscript file 
library(sf)
library(rgdal)
library(terra)
library(data.table)
#load('.rdata')
require(tidyverse)
library(lubridate) #time series
library(plyr) #rbind.fill: rbind with different columns and fill with NA
library(data.table) #this helps with the order command
library(terra)
library(sf)


##Load in necessary shapefiles

SCFrast <- rast('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/meanSCF.tif')
watershds <- vect('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/watershed_firesnow.shp')
SCFrast<-project(SCFrast,crs(watershds))
fires <- vect('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/fire_severity2.shp')
fires<- project(fires, watershds)

#plot(SCFrast)
#plot(watershds, add=T)

##
##Percent of snow in watershed
##

for (i in 1:length(watershds)) {
      assign(paste0("GageID_",watershds$GAGE_ID[i]), terra::extract(SCFrast,watershds[i]))
}

watershed.df.ls<-mget(ls(pattern="GageID_")) #list of all data frames

breaks<-c(0,0.25,1)
tags<-c('no_snow', 'snow')
watershed_prop<- data.frame(number = 1:length(watershds), GAGE_ID = NA, no_snow=NA, snow=NA)

for (i in 1:length(watershds)) {
  breaksT<-cut(watershed.df.ls[[i]]$meanSCF, breaks = breaks, include.lowest = T, right = F, labels = tags)
  breaksT[is.na(breaksT)]<-'no_snow'
  breaksT<-table(breaksT)
  percentage<-round(prop.table(breaksT), digits = 2)
  watershed_prop$no_snow[i]<-percentage[1] #no snow percentage
  watershed_prop$snow[i]<-percentage[2]  #snow percentage
  watershed_prop$GAGE_ID[i]<- sub("GageID_", "", names(watershed.df.ls)[i])
  
}

full_join(watershds, watershed_prop, by = "Gage_ID")

watershds$no_snow <- watershed_prop$no_snow[match(watershds$GAGE_ID, watershed_prop$GAGE_ID)]
watershds$snow <- watershed_prop$snow[match(watershds$GAGE_ID, watershed_prop$GAGE_ID)]

#writeVector(watershds,'/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/watershed_snow.shp')

######
######
#Percent of fire in watershed

fire_per<-data.frame(length =  1: length(watershds), GAGE_ID = NA, fire_percentage = NA)

for (i in 1:length(watershds)) {
  intersecttst<-terra::intersect(watershds[i], fires)
  fire_percentage<-max(expanse(intersecttst))/expanse(watershds[i]) #This is saying the max intercept 
  fire_per$fire_percentage[i]<-fire_percentage
  fire_per$GAGE_ID[i]<-watershds$GAGE_ID[i]
}
fire_per$fire_percentage<-round(fire_per$fire_percentage*100, digits = 2)
summary(fire_per$fire_percentage)
fire_per.dt<-data.table(fire_per)
fire_per.dt[order(fire_percentage, GAGE_ID)] #only 31 fires that take up more than 20%
fire_per.dt[fire_per.dt$fire_percentage > 20, ] 

watershds$percentageFire <- fire_per.dt$fire_percentage[match(watershds$GAGE_ID, fire_per.dt$GAGE_ID)]

####
##merging fire dataframe and watershed dataframe
####

watershds.sf<-st_as_sf(watershds) #I will drop geometry, but for mapping purpose do not drop geometry
fires.sf<-st_as_sf(fires)

watershdfire.ls<-list(NA)
for (i in 1:length(watershds)) {
  intersect <- st_intersection(watershds.sf[i,], fires.sf)
  attArea <- intersect %>% 
    mutate(area = st_area(.) %>% as.numeric())
  largest_contrib_fire<- st_drop_geometry(attArea[attArea$area == max(attArea$area),]) #fire with the largest contributing area to the watershed
  watershdfire.ls[[i]]<-largest_contrib_fire
}

watershdfire.df<-data.table::rbindlist(watershdfire.ls, use.names = T, fill=T)

watershdfire.df
watershdfire.df$TF<-floor(log10(watershdfire.df$GAGE_ID)) + 1 < 8

#Add a zero to GAGE_ID if shorting than 8 digets
watershdfire.df$GAGE_ID<-ifelse(floor(log10(watershdfire.df$GAGE_ID)) + 1 < 8, paste0("0", watershdfire.df$GAGE_ID), watershdfire.df$GAGE_ID)
watershdfire.df2 <- subset(watershdfire.df, select = -c(X.3,X.2,X.1,X))

#have to write as txt file in order to not drop the leading 0 in the gage ID
write.csv(watershdfire.df2, '/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/whole_watershedfire.txt')


