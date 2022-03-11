
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
filez <- dir("~/Desktop/PRISM_Data", recursive = TRUE, pattern = '\\.bil$') #the recursive should look through all folders
pathnm <- "~/Desktop/PRISM_Data/"
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
  prismdata$meanPRISM[i]<-mean(terra::extract(PRISM.ls.rp[[i]], fire.rp)[,2])*0.0254*area #convert to inches to meters also have to conver (cfs)
                                                                                    #multiply over the area of the watershed to get total discharge
  prismdata$sumPRISM[i]<-sum(terra::extract(PRISM.ls[[i]], fire)[,2])*0.0254*4000^2 #convert to cubic units as well cubic rain over watershed 
                                                                                   #multiply by the resoluation of PRISM since I am am summing over the watershed
  prismdata$maxPRISM[i]<-max(terra::extract(PRISM.ls.rp[[i]], fire.rp)[,2])*0.0254*area
}
print(prismdata) 
#I don't know why the method of summing all the pixels in the watershed does work 

#Adding back to streamflow metrics 
streamflow_metrics_precip<-full_join(streamflow_metrics,prismdata)


streamflow_metrics_precip$runoffratio<-(streamflow_metrics_precip$peakflow_cms)/(streamflow_metrics_precip$meanPRISM /(60*60*24*365)) #cubic feet to cubic meters, converting annual to seconds
streamflow_metrics_precip$maxrunnoffratio<-(streamflow_metrics_precip$peakflow_cms)/(streamflow_metrics_precip$maxPRISM /(60*60*24*365))
streamflow_metrics_precip$rration_totalQ<-(streamflow_metrics_precip$annual_tot)/(streamflow_metrics_precip$meanPRISM ) 
streamflow_metrics_precip$rration_meanQ<-(streamflow_metrics_precip$annual_mean)/(streamflow_metrics_precip$meanPRISM /(60*60*24*365))
streamflow_metrics_precip$rration_maPeak<-(streamflow_metrics_precip$max_ma30)/(streamflow_metrics_precip$meanPRISM /(60*60*24*365))

plot(streamflow_metrics_precip$year, streamflow_metrics_precip$runoffratio, type='l', xlim=c(2011,2018)) 
plot(streamflow_metrics_precip$year, streamflow_metrics_precip$maxrunnoffratio, type='l', xlim=c(2011,2018)) #not really a difference in pattern
plot(streamflow_metrics_precip$year, streamflow_metrics_precip$rration_totalQ, type='l', xlim=c(2011,2018))
plot(streamflow_metrics_precip$year, streamflow_metrics_precip$rration_meanQ, type='l', xlim=c(2011,2018))

par(bg="white") # This series of code makes just plot back ground grey and outside white: for better contrast
plot(streamflow_metrics_precip$rration_maPeak, streamflow_metrics_precip$meanslope,
     xlab = "Day of spring onset", 
     ylab="Mean hydrograph slope (cfs/day)")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey")
par(new = TRUE)
points(streamflow_metrics_precip$rration_maPeak, streamflow_metrics_precip$meanslope, 
       col = ifelse(streamflow_metrics_precip$year>fire_year, 
                    postpal(10)[as.numeric(cut(streamflow_metrics[streamflow_metrics$year>fire_year,]$year, breaks = 10))],
                    "turquoise3"), 
       pch=19, cex=2)

#If df is not already in the environment read in table up top



