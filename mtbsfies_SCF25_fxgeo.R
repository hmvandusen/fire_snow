###Percent burn severity calculation 
###workflow: 
#load mtbsfire_SCF25_2000_2018.shp 
#load in 2000 - 2018 burn mosaic from mtbs
#clip burn seversity by shapefile
#create a function to calculate percent high severity with in each shapefile

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))##set working directory to current rscript file 
library(sf)
library(rgdal)
library(terra)
library(data.table)
#load('.rdata')
require(tidyverse)
library(lubridate) #time series
library(plyr) #rbind.fill: rbind with different collumns and fill with NA
library(data.table) #this helps with the order command

##
##load fire perimeter shape file
##

# for all files: mtbsfire_SCF25_2000_2018.shp 
#fire.vect<-vect("./mtbsfire_SCF25_2000_2019.shp")
#plot(fire.vect)
#summary(fire.vect) #Lat Long NAD83

#load in just fires with USGS gages and in the Snow Zone fires from 2000 - 2019
fire.shp<-readOGR("fire_ref_snowqgis.shp") #only loading in with OGR bc I want to plot it 
#plot(fire.shp)
fire.vect<-vect(fire.shp)
#plot(fire.vect) plot function is not working 
summary(fire.vect)
#Lat Long NAD83
fire.vect$Ig_Date[1]
fire.vect$Ig_Date<-as.Date(fire.vect$Ig_Date)
fire.vect$year<-lubridate::year(fire.vect$Ig_Date)

#reproject fire perimeter shape file to match burn severity
fire.vect<-project(fire.vect, firerastls_raw[[1]]) #transform fire perimeter to rasts
crs(fire.vect)


#Load in burn severity for years 2001 to 2019
fire_pathnm<-"./composite_data/all_years/mtbs_allyears_tif"
firesls<-list.files(path=fire_pathnm, pattern='tif$', full.names=TRUE)
firerastls_raw<-lapply(firesls, rast) #Albers Equal Area Conic USGS, NAD1983

for (i in 1:length(firerastls_raw)) {
  names(firerastls_raw[[i]])<-gsub("mtbs_CONUS_",'',names(firerastls_raw[[i]]))
}


for (year in 1:length(firerastls_raw)) {
for (fire in 1:length(fire.vect)) {
  
   if (fire.vect[fire]$year==names(firerastls_raw[[year]])){
        assign(paste0("fireID_",fire), terra::extract(firerastls_raw[[year]], fire.vect[fire]))
  }
}
  }

mtbs_severity.df.ls<-mget(ls(pattern="fireID_")) #list of all data frames
mtbs_severity_freq.tbl<-vector('list',length = length(mtbs_severity.df.ls)) #create list for for loop

#MTBS band classification from metadata
#1 Unburned / Underburned to Low Burn Severity Producer defined
#2 Low burn severity
#3 Moderate burn severity
#4 High Burn Severity
#5 Increased Greenness/Increased Vegetation Response
#6 Non-Processing Area Mask


# find the amount 1:4 bands for each year
system.time(
for (i in 1:length(mtbs_severity.df.ls)) {
  mtbs_severity_freq.tbl[[i]]<-table(mtbs_severity.df.ls[[i]]) #use data.table for faster processing
}
)
names(mtbs_severity_freq.tbl)<-names(mtbs_severity.df.ls)

#turn tables into data_frames
mtbs_severity_freq.df<-lapply(mtbs_severity_freq.tbl, as.data.frame.matrix)

#making an ID column and moving it toward the front
system.time(
  for (i in 1:length(mtbs_severity.df.ls)) {
    mtbs_severity_freq.df[[i]]$ID<-as.numeric(gsub("fireID_", "", names(mtbs_severity_freq.df[i])))
  }
)

mtbs_severity_combined<- do.call(rbind.fill, mtbs_severity_freq.df) #absolutely gorgeous 
burnseverity_total <- mtbs_severity_combined[order(mtbs_severity_combined[, "ID"]), , drop = FALSE]
burnseverity_total[is.na(burnseverity_total)] = 0

#percentage of burn seversity classification
severity_per<-function(x,y){x/sum(y)}

burnseverity_total$Hprop<-NA
burnseverity_total$Mprop<-NA
burnseverity_total$Lprop<-NA

for (i in 1:dim(burnseverity_total)[1]) {
  burnseverity_total$Hprop[i]<- severity_per(burnseverity_total$`4`[i], burnseverity_total[i,c('1','2','3','4')])
  burnseverity_total$Mprop[i]<-severity_per(burnseverity_total$`3`[i], burnseverity_total[i,c('1','2','3','4')])
  burnseverity_total$Lprop[i]<-severity_per(burnseverity_total$`2`[i], burnseverity_total[i,c('1','2','3','4')])
  
}


#Attaching to the shape file
fire.vect$Hseverity<-burnseverity_total[,c('Hprop')]
fire.vect$Mseverity<-burnseverity_total[,c('Mprop')]
fire.vect$Lseverity<-burnseverity_total[,c('Lprop')]

writeVector(fire.vect,"fire_severity2.shp")


