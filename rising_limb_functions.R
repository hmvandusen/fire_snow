

knitr::opts_chunk$set(echo = TRUE)
#install.packages('dataRetrieval')
library(dataRetrieval) # packaage for downloading USGS stream flow data
#install.packages('lubridate')
library(lubridate) # date management
library(forecast) 
library(stlplus) 
library(fpp) 
library(dplyr) 
library(reshape2) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Test 1: 
#Fawn Creek Complex: Andrews Creek near Mazama 
#Fire occurred:2003


#####
#Objects that change:: additional object to change and save product at bottom of script

# Pull in gage for fire identifying parameters for data download
siteNumber <- '12374250' # USGS gauge number
fire_year <- 2007


###Objects that do not change#### 

#window to plot in function: suggested window year 1 after fire 
plot_date<- c("2021-01-01", "2021-12-31")
parameterCd <- "00060"  # mean daily discharge in cfs
startDate <- "2000-01-01" # period of record of MODIS
endDate <- "2021-12-31" # Current year

#### Cleaning in Gage data 

# download data using readNWISdv function
streamflow_analysis<-function(siteNumber, parameterCd, startDate, endDate){
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
plot(watershed_name$Date, watershed_name$discharge_cfs, type='l', ylab='Discharge (cfs)', xlab='Time (days)')
abline(v=as.Date('2007-07-17'), col='red')

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(watershed_name$Date))) != (nrow(watershed_name)+1)){
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

watershed_name$year.fact<-as.factor(watershed_name$year)
watershed_ls<<-split(watershed_name, watershed_name$year.fact)
}

####
##function 2 for each year
###

eachyear<-function(yearperiod){

#This is calculating the 30 day running average. This is one sided instead of two 
rollmean <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=1)}
yearperiod$rollmean <- as.numeric(rollmean(yearperiod$discharge_cfs)) 

#This calculates 30 day moving average 15 days on both sides
ma30 <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=2)}
yearperiod$ma30 <- as.numeric(ma30(yearperiod$discharge_cfs)) 

#Ploting 30-day rolling mean vs 30-day moving average
## Moving average is much sooner than the rolling average. This will provide you with a very different date
plot(yearperiod$Date,yearperiod$rollmean, type='l', col='red', xlim=as.Date(plot_date))
lines(yearperiod$Date, yearperiod$ma30)

##
##Painter et al. Rising Limb
#Running mean flow since 1 January

  for (i in 1:dim(yearperiod)[1]) {
    yearperiod$runmeanPainter[i]<-sum(yearperiod$discharge_cfs[1:i])/i
  }


plot(yearperiod$Date,yearperiod$rollmean, type='l', col='red', xlim=as.Date(plot_date), ylim = c(0,5000))
lines(yearperiod$Date, yearperiod$ma30)
lines(yearperiod$Date, yearperiod$discharge_cfs, col='blue')
lines(yearperiod$Date, yearperiod$runmeanPainter, col='green')

##Find percent change of Painter running mean:

for (i in 2:dim(yearperiod)[1]) {
  yearperiod$perchange[1]<-NA
  yearperiod$perchange[i]<- ((yearperiod$runmeanPainter)[i]/yearperiod$runmeanPainter[i-1]*100)-100
  
}

plot(yearperiod$Date, yearperiod$perchange, type='l', col='red', xlim=as.Date(plot_date), ylim = c(0,20))
abline(h=1)

assign(paste0("watershed_", yearperiod$year[1]), yearperiod, envir = globalenv())

}

###Application of functions 
watershed_allyears<-streamflow_analysis(siteNumber, parameterCd, startDate, endDate)
watershed_allyears.ls<-lapply(watershed_allyears, function(y) eachyear(y))

#Creating a usable dataframe and following for loop
streamflow_metrics<-data.frame(time= 1:22,year=NA, springonset_date=as.Date(NA), peakflow_cfs=NA, meanslope=NA, meanslope_ma=NA)

for (i in 1:length(watershed_allyears.ls)) {
  streamflow_metrics$year[i]<-watershed_allyears.ls[[i]]$year[1]
  
  date<-watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$perchange >= 1 & watershed_allyears.ls[[i]]$month > 2  ,]$Date[1]
  streamflow_metrics$springonset_date[i]<- date[1]
  
  peakflow<-max(watershed_allyears.ls[[i]]$discharge_cfs)
  streamflow_metrics$peakflow_cms[i]<-peakflow*0.0283168
  streamflow_metrics$peakflow_cfs[i]<-peakflow
  
  #Find mean slope from start to peak flow 
  streamflow_metrics$meanslope[i]<- (peakflow - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$discharge_cfs)/ 
    (watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$discharge_cfs == peakflow, ]$doy - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  max_ma30<-max(watershed_allyears.ls[[i]]$ma30, na.rm=T)
  streamflow_metrics$max_ma30[i]<-max_ma30
  
  streamflow_metrics$meanslope_ma[i]<- (max_ma30 - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == date, ]$ma30)/(na.omit(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$ma30 == max_ma30, ]$doy) - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  streamflow_metrics$annual_tot[i]<-sum(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  streamflow_metrics$annual_mean[i]<-mean(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  
  
  streamflow_metrics$annual_mean[i]<-mean(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  
  streamflow_metrics$baseflow[i]<-median(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$month == 8,]$discharge_cfs)*0.0283168

} #moving average mean slope is still not working 

plot(streamflow_metrics$year, streamflow_metrics$peakflow_cfs, type = "l", xlab="Time", ylab='Streamflow (cfs)')
abline(v= fire_year, col="red", lwd =3)


###SAVE streamflow_metrics table as a more specific name!####
#Example: 
#MFrockCr_streamflow_metrics<-streamflow_metrics
#write.csv(MFrockCr_streamflow_metrics, "./streamflow_metrics/johnsoncreek_streamflow_metrics.csv")

##
##
##Plots for all Years

#Notes:
#Wrong year on springonset_md, but it don't matter for plotting purposes: just want month and day
#Red Post fire, Blue Pre-Fire
streamflow_metrics$springonset_md<-as.Date(format(streamflow_metrics$springonset_date, format="%m-%d"), "%m-%d")
plot(streamflow_metrics$springonset_md, streamflow_metrics$meanslope, col = ifelse(streamflow_metrics$year>fire_year, 'red','blue'), 
     pch=19,  xlab = "Day of spring onset", ylab="Mean hydrograph slope (cfs/day)", cex= 4)
plot(streamflow_metrics$peakflow_cfs, streamflow_metrics$meanslope, col = ifelse(streamflow_metrics$year>fire_year, 'red','blue'), 
     xlab = "Peak streamflow (cfs)", ylab="Mean hydrograph slope", pch = 19)
plot(streamflow_metrics$springonset_md, streamflow_metrics$peakflow_cfs, col = ifelse(streamflow_metrics$year>fire_year, 'red','blue'), 
     xlab = "Day of spring onset", ylab="Peak streamflow (cfs)", pch = 19, cex=4)

#Plots with gradient color
#Darker colors mean year closer to fire
library(RColorBrewer)
precols<-brewer.pal(4,"Blues")
postcols<-rev(brewer.pal(4,"Reds"))
prepal<-colorRampPalette(precols)
postpal<-colorRampPalette(postcols)

par(bg="white") # This series of code makes just plot back ground grey and outside white: for better contrast
plot(streamflow_metrics$springonset_md, streamflow_metrics$meanslope,
     xlab = "Day of spring onset", 
     ylab="Mean hydrograph slope (cfs/day)")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey")
par(new = TRUE)
points(streamflow_metrics$springonset_md, streamflow_metrics$meanslope, 
     col = ifelse(streamflow_metrics$year>fire_year, 
     postpal(10)[as.numeric(cut(streamflow_metrics[streamflow_metrics$year>fire_year,]$year, breaks = 10))],
     "turquoise3"), 
     pch=19, cex=2)
##gradient pre fire colors:: prepal(10)[as.numeric(cut(streamflow_metrics[streamflow_metrics$year<fire_year,]$year, breaks = 10))]) 


#Bar plot of mean hydro graph slope, colored by pre and post fire
streamflow_metrics_slopeordered<-streamflow_metrics[order(streamflow_metrics$meanslope, decreasing = T),]
barplot(streamflow_metrics_slopeordered$meanslope, breaks = 50, col = ifelse(streamflow_metrics_slopeordered$year>fire_year, 
    postpal(10)[as.numeric(cut(streamflow_metrics_slopeordered[streamflow_metrics_slopeordered$year>fire_year,]$year, breaks = 10))],
    prepal(10)[as.numeric(cut(streamflow_metrics_slopeordered[streamflow_metrics_slopeordered$year<fire_year,]$year, breaks = 10))]),
     xlab = "Mean slope")

#Table of top 6 years with the largest mean slope
head(streamflow_metrics_slopeordered)

#Barplot of top years 
streamflow_metrics_3y<-streamflow_metrics[streamflow_metrics$year>(fire_year-4) & streamflow_metrics$year<(fire_year+4),]
barplot(streamflow_metrics_3y$springonset_date, breaks = 50, col = ifelse(streamflow_metrics_3y$year>fire_year, 
      postpal(3)[as.numeric(cut(streamflow_metrics_3y[streamflow_metrics_3y$year>fire_year,]$year, breaks = 3))],
      prepal(4)[as.numeric(cut(streamflow_metrics_3y[streamflow_metrics_3y$year<fire_year,]$year, breaks = 4))]),
      xlab = "Mean slope")

#Mean date of spring onset (mean slope is pretty much the same)
mean(na.omit(streamflow_metrics[streamflow_metrics$year <= fire_year,]$springonset_md))
mean(na.omit(streamflow_metrics[streamflow_metrics$year > fire_year & streamflow_metrics$year <= fire_year+3,]$springonset_md))

mean(na.omit(streamflow_metrics[streamflow_metrics$year <= fire_year,]$max_ma30))
mean(na.omit(streamflow_metrics[streamflow_metrics$year > fire_year & streamflow_metrics$year <= fire_year+3,]$max_ma30))

#mean(na.omit(streamflow_metrics_precip[streamflow_metrics_precip$year <= fire_year,]$rration_maPeak))
#mean(na.omit(streamflow_metrics_precip[streamflow_metrics_precip$year > fire_year & streamflow_metrics_precip$year <= fire_year+3,]$rration_maPeak))

#creating a factor for before fire and 3 years after fire

streamflow_metrics$before_after_f<- as.factor(with(streamflow_metrics, ifelse( year <= fire_year, 'prefire', 
                                                                               ifelse(year > fire_year & streamflow_metrics$year <= fire_year+3, 'postfire_3yr', 
                                                                                      'postfire'))))
boxplot(springonset_md ~ before_after_f, data = streamflow_metrics)
boxplot(meanslope ~ before_after_f, data = streamflow_metrics, ylim = c(0,7))
boxplot(meanslope_ma ~ before_after_f, data = streamflow_metrics)
boxplot(peakflow_cms ~ before_after_f, data = streamflow_metrics)

#Niceer looking ggplots 
ggplot(streamflow_metrics %>% filter(!is.na(before_after_f)), 
       aes(x=before_after_f, y=baseflow, fill=before_after_f)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2") +
  xlab( "Pre or Post Fire") + ylab('baseflow') 


  
  
