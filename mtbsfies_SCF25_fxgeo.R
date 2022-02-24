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
library(dyplr)
library(data.table)
#load('.rdata')
require(tidyverse)
library(lubridate)

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




#method 1: extract pixel values for each fire year by fire perimeters
#only fires that lie within the perimeter should be counted
#potential source of error: counts pixel values that were not during period of ignition 
system.time(
for (i in 1:length(firerastls_raw)) {
assign(names(firerastls_raw[[i]]), terra::extract(firerastls_raw[[i]], fire.vect))
}
)

mtbs_severity.df.ls<-mget(ls(pattern="mtbs_CONUS_")) #list of all dataframes
mtbs_severity_freq.tbl<-vector('list',length = 19) #create list for for loop

#MTBS band classification from metadata
#1 Unburned/Underburned to Low Burn Severity Producer defined
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

#turn tables into dataframes
mtbs_severity_freq.df<-lapply(mtbs_severity_freq.tbl, as.data.frame.matrix)

#making an ID column and moving it toward the front
system.time(
  for (i in 1:length(mtbs_severity.df.ls)) {
    mtbs_severity_freq.df[[i]]$ID<- row.names(mtbs_severity_freq.df[[i]])
    mtbs_severity_freq.df[[i]]<-mtbs_severity_freq.df[[i]][,c(7,1:6)]
  }
)

mtbs_severity_combined<-reduce(mtbs_severity_freq.df, left_join, by='ID')

prefixes = unique(sub("\\..*", "", colnames(mtbs_severity_combined)))#only uses the first number in each column name
prefixes<- prefixes[2:7]
burnseverity_total<-data.frame(sapply(prefixes, function(x) rowSums(mtbs_severity_combined[,startsWith(colnames(mtbs_severity_combined), x)])))

#Adding ID number and naming bands
burnseverity_total$ID<-c(1:75)
burnseverity_total<-burnseverity_total[,c(7,1:6)]
colnames(burnseverity_total)[2:7]<-c(1:6)

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

ordered_burnseverity<-setorder(burnseverity_total, -Hprop, -Mprop) #order by burn severity

#Attaching to the shape file
fire.vect$Hseverity<-burnseverity_total[,c('Hprop')]
fire.vect$Mseverity<-burnseverity_total[,c('Mprop')]
fire.vect$Lseverity<-burnseverity_total[,c('Lprop')]

writeVector(fire.vect,"fire_severity.shp")


