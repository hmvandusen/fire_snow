

###
### Watershed statistics 
###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))##set working directory to current rscript file 
library(sf)
library(rgdal)
library(rgeos)
library(terra)
library(data.table)
#load('.rdata')
require(tidyverse)
library(lubridate) #time series
library(plyr) #rbind.fill: rbind with different columns and fill with NA
library(data.table) #this helps with the order command
library(terra)
library(sf)
library(waterData)
library(dataRetrieval)
library(remotes) #USGS package for missing data
library(smwrBase)

##Load in necessary shapefiles

SCFrast <- rast('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/meanSCF.tif') #mean modis SCF raster; Ask Ari how she got this
watershds <- vect('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/ref_watersheds_WUS.shp')
SCFrast<-project(SCFrast,crs(watershds))
fires <- vect('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/fire_severity2.shp') #MTBS fires 2000 - 2019
fires<- project(fires, watershds)
EPA1<-vect('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/na_cec_eco_l1 (1)/NA_CEC_Eco_Level1.shp')
EPA1<- project(EPA1, watershds)

#plot(SCFrast)
#plot(watershds, add=T)

fire_threshold = 5 #in percentage
snow_threshold = .1 #in decimal 
test_gage = '13313000' # gage that you know has consistent gage data

parameterCd <- "00060"  # mean daily discharge in cfs
startDate <- "1959-10-01" # period of record of MODIS
endDate <- "2022-09-30" # Current year

##
##Sort fires that have burned 5% or more of the watershed and are in the seasonal snow zone 
##
watersheds_snow_fire<-function(watershds, SCFrast, fires){
  
##
##Select watersheds that contain 10% Percent of the seasonal snow zone (SCF 25%, Gleason et al. 2013) 
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

#full_join(watershds, watershed_prop, by = "Gage_ID") #doesn't work 
watershds$no_snow <- watershed_prop$no_snow[match(watershds$GAGE_ID, watershed_prop$GAGE_ID)]
watershds$snow <- watershed_prop$snow[match(watershds$GAGE_ID, watershed_prop$GAGE_ID)]
hist(watershds$snow) #two tailed: opposite of normally distributed

#subset out watersheds that have 10% seasonal snow zone
watershds<-watershds[watershds$snow > snow_threshold]

##
##Distinguishing between burned and unburned basins
##

fire_per<-data.frame(length =  1: length(watershds), GAGE_ID = NA, fire_percentage = NA, fire_year=NA, Incid_Name=NA)

for (i in 1:length(watershds)) {
  intersecttst<-terra::intersect(watershds[i], fires) #will produce Inf values if there is not intersection
  fire_percentage<-max(expanse(intersecttst))/expanse(watershds[i]) #This is saying the max intercept 
  fire_per$fire_percentage[i]<-fire_percentage
  fire_per$GAGE_ID[i]<-watershds$GAGE_ID[i]
  
  if(fire_percentage != -Inf){
  fire_row<-which(expanse(intersecttst) == max(expanse(intersecttst)))
  fire_per$fire_year[i]<- lubridate::year(as.Date(intersecttst$Ig_Date[fire_row]))
  fire_per$Incid_Name[i] <- intersecttst$Incid_Name[fire_row]
  }
}
fire_per$fire_percentage<-round(fire_per$fire_percentage*100, digits = 2)
summary(fire_per$fire_percentage)
fire_per.dt<-data.table(fire_per)
#fire_per.dt[order(fire_percentage, GAGE_ID)] #only 31 fires that take up more than 20%
#fire_per.dt[fire_per.dt$fire_percentage > 20, ] 
watershds$percentageFire <- fire_per.dt$fire_percentage[match(watershds$GAGE_ID, fire_per.dt$GAGE_ID)] #stich back to shape file
watershds$percentageFire <- ifelse(watershds$percentageFire == -Inf, 0, watershds$percentageFire) # All -inf are unburned
watershds$fire_year <- fire_per.dt$fire_year[match(watershds$GAGE_ID, fire_per.dt$GAGE_ID)]
watershds$Incid_Name <- fire_per.dt$Incid_Name[match(watershds$GAGE_ID, fire_per.dt$GAGE_ID)]


#distinguishing which watersheds are burned and unburned 0% - 5%
watershds$burn_type <- as.factor(ifelse(watershds$percentageFire > fire_threshold, 'burned','unburned'))
return(watershds)
}
watersheds<-watersheds_snow_fire(watershds, SCFrast, fires)

##
## Label by Ecoregion EPA I and III
##

#workflow: create EPA raster to find most common raster value in extracted watershed
nrow(SCFrast)
r <- rast(extent = ext(SCFrast), nrow=2000, ncol = 2000, crs = crs(SCFrast)) #create blank raster
values(r)<-1 # set all values to 1
EPA1$code <- as.numeric(EPA1$NA_L1CODE) #code is numeric 
EPA1_rast<- rasterize(EPA1, r, field = 'code', fun=mode,mask=F) # rasterize epa level 1
#define function to calculate mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
watersheds$EPA_L1<-NA
#find EPA value for each watershed
for (i in 1: length(watersheds)) {
  watersheds[i]$EPA_L1<-find_mode(terra::extract(EPA1_rast, watersheds[i])$code)
}

summary(watersheds$EPA_L1)

#####
#### Cleaning in Gage data 
###
##
watersheds_df<-as.data.frame(watersheds) #create dataframe from watershed shapefile
watersheds_df_con<-watersheds_df[1,] #use first column watershed: this will be removed

#Date threshold for data available: must be 15 years pre-fire maximum date is 1985
date<-as.Date(c('1984-10-01'))


## Selection of watersheds that are active after 1985
for (i in 1:dim(watersheds_df)[1]) {
 tryCatch({
  gage<-watersheds_df$GAGE_ID[i]
  watershed_name <- dataRetrieval::readNWISdv(gage, parameterCd, startDate, endDate)
  if(rev(watershed_name$Date)[1] >  date
  ){
    newrow<- watersheds_df[watersheds_df$GAGE_ID== gage,]
  }
  watersheds_df_con<-rbind(watersheds_df_con, newrow)
  rm(newrow)
  }, 
  
  error = function(e){
    message(paste("An error occurred for item", i, conditionMessage(e),":\n"), e)})
}
head(watersheds_df_con)
watersheds_df_con<-watersheds_df_con[-1,] # remove first row: this row does matter

## Selection of watersheds with continuous streamflow data at least 70% (80%, Williams et al. 2022) of data 
#       from 1960 - 2022 (1975 - 2020, Williams et al. 2022)
#       filling where missing streamflow values for ≤10 consecutive days were replaced with linearly interpolated values 
#       based on the 10 day before and after to and following the gap
#   Williams Selected watersheds that 80% of days for each half of water yeas

#This is measuring that it has 60% for the day for each half of the water year
streamflow_continuous_data<-function(siteNumber, parameterCd, startDate, endDate){
  watershed_name <- dataRetrieval::readNWISdv(siteNumber, parameterCd, startDate, endDate)
  head(watershed_name) # check
  colnames(watershed_name)[4]<- 'discharge_cfs'
  watershed_name<-watershed_name[,-5]
  
  #Making time series in water year
  #I am separating the date for each column 
  watershed_name$year <-lubridate::year(watershed_name$Date) # add year col
  watershed_name$month <-lubridate::month(watershed_name$Date) # add month col
  watershed_name$day <-lubridate::day(watershed_name$Date) # add day of month col
  watershed_name$doy <-lubridate::yday(watershed_name$Date) # add day of year col
  watershed_name<-addWaterYear(watershed_name)
  
  #Plots of all years from 2000 - 2021 and add in vertical line for fire
  #plot(watershed_name$Date, watershed_name$discharge_cfs, type='l', ylab='Discharge (cfs)', xlab='Time (days)')
  #abline(v=as.Date(fire_year), col='red')
  
  #Check for missing days, if so, add NA rows:
  if(as.numeric(diff(range(watershed_name$Date))) != (nrow(watershed_name)-1)){
    fullDates <- seq(from=min(watershed_name$Date),
                     to = max(watershed_name$Date), by="1 day")
    fullDates <- data.frame(Date = fullDates, 
                            agency_cd = watershed_name$agency_cd[1],
                            site_no = watershed_name$site_no[1],
                            stringsAsFactors = FALSE)
    watershed_name <- full_join(watershed_name, fullDates,
                                by=c("Date","agency_cd","site_no")) %>%
      arrange(Date)
  }
  
  #Fill in missing data for up to 10 day using linear interpoation or variance depneding on span 
  watershed_name$discharge_cfs<- smwrBase::fillMissing(watershed_name$discharge_cfs, span = 10, max.fill = 10)
  
  #Does the data have at least 60% of the data (William's has 80% for each half of water year 1975-2020)
  #I am doing 1985 because thats the max time it needs to go back  ~ 15 yers pre-fire
  date_range<-as.Date(c('1985-10-01', '2021-10-01')) #range of dates 
  watershed_name85<-watershed_name[watershed_name$Date > date_range[1] & watershed_name$Date < date_range[2],]
  half_num<-as.numeric(diff(range(date_range)))/2 #how many days in one half of the year
  
  enougth_data <-ifelse(nrow(na.omit(watershed_name85[watershed_name85$month == 10| watershed_name85$month == 11| watershed_name85$month == 12|watershed_name85$month == 01|
                                                        watershed_name85$month == 2| watershed_name85$month == 3, ]))/half_num > 0.6, 
                        ifelse(nrow(na.omit(watershed_name85[watershed_name85$month == 4 | watershed_name85$month == 5 |watershed_name85$month == 6 |watershed_name85$month == 7|
                                                               watershed_name85$month == 8|watershed_name85$month == 9, ]))/half_num > 0.6, T,F), F)
  
  return(enougth_data)
  
}

cont_data<-data.frame(GAGE_ID = watersheds_df_con$GAGE_ID, data=NA)
for (i in 1:dim(watersheds_df_con)[1]) {
  tryCatch({cont_data$data[i]<-streamflow_continuous_data(watersheds_df_con$GAGE_ID[i],parameterCd, startDate, endDate)}, 
    
    error = function(e){
      message(paste("An error occurred for item", i, conditionMessage(e),":\n"), e)})
   #Labels T if there is enough data and F if not 
}
#join back with watersheds

watersheds_df_con$data<-cont_data$data
#watershds_con <- watersheds[watersheds$data==TRUE]
summary(watersheds_df_con[watersheds_df_con$burn_type == 'burned' & watersheds_df_con$data == T,]) #only 41 burn gages with 80% of the data or 60 gages with 60% of gages 

#write csv
write.csv(watersheds_df_con, 'watersheds_df_con.csv')


##
## For burned basins, Williams has >= 15 years of data prefire and >2 years postfire 
##





##
## Selection of watersheds that are only controlled by climate: Test if annual or longer Q/P trends with precip
##




##
## Make new file: Method 1 for unbuned basins 
##

burn.sf <- st_as_sf(watershds[watershds$burn_type =='burned'])
unburn.sf <- st_as_sf(watershds[watershds$burn_type =='unburned'])

distances <- st_distance(burn.sf[1], unburn.sf)



#writeVector(watershds_con,'/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/watersheds_con_sn10_fire5.shp', overwrite=T)
save.image('data.RData')




