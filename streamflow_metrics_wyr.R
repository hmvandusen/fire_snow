

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
library(data.table) #rbind list function
library(ggplot2)
library(viridis)
library(grDevices)
library(cowplot)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Test 1: 
#Fawn Creek Complex: Andrews Creek near Mazama 
#Fire occurred:2003


#####
#Objects that change:: additional object to change and save product at bottom of script
#watershdfire.df <- read.table('/Volumes/MJ_INFEWS/Hannah Files/Fire_stream_flow/whole_watershedfire.txt', sep=',', header = T)
watershdfire.df.full <- read.csv('watersheds_df_con.csv')

#adding leading 0 to ID values that are less than 8 digets
watershdfire.df.full$GAGE_ID<-ifelse(floor(log10(watershdfire.df.full$GAGE_ID)) + 1 < 8, paste0("0", watershdfire.df.full$GAGE_ID), watershdfire.df.full$GAGE_ID)
watershdfire.df<-watershdfire.df.full[watershdfire.df.full$burn_type == 'burned' & watershdfire.df.full$data == T,]

#create a list for stream metrics dataframe
streamflow_metrics.ls<-list(NA)


for (gage in 1:dim(watershdfire.df)[1]) { #

  tryCatch({
# Pull in gage for fire identifying parameters for data download
siteNumber <- watershdfire.df$GAGE_ID[gage] # USGS gauge number
fire_year <- watershdfire.df$fire_year[gage] 

siteNumber <- watershdfire.df$GAGE_ID[11] # USGS gauge number
fire_year <- watershdfire.df$fire_year[11]

#window to plot in function: suggested window year 1 after fire 
plot_date<- c("2021-10-01", "2022-09-30")
parameterCd <- "00060"  # mean daily discharge in cfs
startDate <- "1985-10-01" # period of record of MODIS
endDate <- "2022-09-31" # Current year

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
#plot(watershed_name$Date, watershed_name$discharge_cfs, type='l', ylab='Discharge (cfs)', xlab='Time (days)')
#abline(v=as.Date(fire_year), col='red')

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

watershed_name$year.fact<-as.factor(watershed_name$waterYear)
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
  jan1<-which(yearperiod$month == 1)[1]
  endyr<-dim(yearperiod)[1]
  yearperiod$runmeanPainter <- NA
  if (jan1 < endyr )
  for (i in jan1:endyr) {
    yearperiod$runmeanPainter[i]<-sum(yearperiod$discharge_cfs[jan1:i])/length(jan1:i) 
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
true<-lapply(watershed_allyears, function(x) nrow(x) > 360) #if year has atleast 200 days
u <- true == T 
watershed_allyears<-watershed_allyears[u]

watershed_allyears.ls<-lapply(watershed_allyears, function(y) eachyear(y))


#Creating a usable dataframe and following for loop
nyear<-length(watershed_allyears.ls)
#streamflow_metrics<-data.frame(time= 1:22,year=NA, springonset_date=as.Date(NA), peakflow_cfs=NA, meanslope=NA, meanslope_ma=NA)

streamflow_metrics<-data.frame(time= lubridate::year(startDate):lubridate::year(endDate), #, year=NA, 
                               springonset_date=as.Date(NA), peakflow_cfs=NA, 
                               meanslope=NA, meanslope_ma=NA, annual_tot = NA, 
                               annual_mean=NA, baseflow=NA, peakflow_cms=NA, max_ma30 =NA
)

for (i in 1:length(watershed_allyears.ls)) {
  tryCatch({
  numrow<-nrow(streamflow_metrics[streamflow_metrics$time <= names(watershed_allyears.ls)[i],]) #find row number for the correct year
  streamflow_metrics[numrow,]$year<-watershed_allyears.ls[[i]]$waterYear[i]
  
  date<-na.omit(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$perchange >= 1 & watershed_allyears.ls[[i]]$month > 1  & watershed_allyears.ls[[i]]$month < 10 ,])$Date[1]
  streamflow_metrics$springonset_date[numrow]<- date[1]
  
  peakflow<-max(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$perchange >= 1 & watershed_allyears.ls[[i]]$month > 1 & watershed_allyears.ls[[i]]$month < 10 ,]$discharge_cfs)
  streamflow_metrics$peakflow_cms[numrow]<-peakflow*0.0283168
  streamflow_metrics$peakflow_cfs[numrow]<-peakflow
  
  #Find mean slope from start to peak flow 
  streamflow_metrics$meanslope[numrow]<- (peakflow - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$discharge_cfs)/ 
    (watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$discharge_cfs == peakflow, ]$doy - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  max_ma30<-max(watershed_allyears.ls[[i]]$ma30, na.rm=T)
  streamflow_metrics$max_ma30[numrow]<-max_ma30
  
  streamflow_metrics$meanslope_ma[numrow]<- (max_ma30 - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == date, ]$ma30)/(na.omit(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$ma30 == max_ma30, ]$doy) - watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$Date == as.Date(date), ]$doy)
  
  streamflow_metrics$annual_tot[numrow]<-sum(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  streamflow_metrics$annual_mean[numrow]<-mean(watershed_allyears.ls[[i]]$discharge_cfs)*0.0283168
  
  
  streamflow_metrics$baseflow[numrow]<-median(watershed_allyears.ls[[i]][watershed_allyears.ls[[i]]$month == 8,]$discharge_cfs)*0.0283168},
  
  error = function(e){
    message(paste("An perchange occurred for item", i, conditionMessage(e),":\n"), e)})
  
}


streamflow_metrics$before_after_f<- as.factor(with(streamflow_metrics, ifelse( time <= fire_year, 'prefire', 
                                                                               ifelse(time > fire_year & streamflow_metrics$time <= fire_year+3, 'postfire_3yr', 
                                                                                      'postfire'))))
streamflow_metrics$springonset_md<-as.Date(format(streamflow_metrics$springonset_date, format="%m-%d"), "%m-%d")

streamflow_metrics$GAGE_ID <- siteNumber
streamflow_metrics$fire_year <- fire_year
streamflow_metrics$snow <- watershdfire.df$snow[gage]
streamflow_metrics$percentageFire <- watershdfire.df$percentageFire[gage]
streamflow_metrics$Hseverity <- watershdfire.df$Hseverity[gage]
streamflow_metrics$meanQtotal<-mean(na.omit(streamflow_metrics[streamflow_metrics$before_after_f == 'prefire',]$annual_tot))
streamflow_metrics$meanpeakflow_cms<-mean(na.omit(streamflow_metrics[streamflow_metrics$before_after_f == 'prefire',]$peakflow_cms))



streamflow_metrics.ls[[gage]]<- streamflow_metrics
rm(list=ls(pattern="watershed_"))}, 

  error = function(e){
    message(paste("An error occurred for item", gage, conditionMessage(e),":\n"), e)})
} #dim(watershdfire.df)[1]

#These gages are not active after 2000
numbers<-which(lengths(streamflow_metrics.ls) == 0)
inactive_gages<-vector(mode = 'character')
for (i in 1:length(numbers)){
  inactive_gages[i]<-watershdfire.df$GAGE_ID[numbers[i]]
}

####condense list to dataframe
#streamflow_metrics<-rbindlist(streamflow_metrics.ls)
#streamflow_metrics$GAGE_ID.fact<-as.factor(streamflow_metrics$GAGE_ID)
##

#by(streamflow_metrics, INDICES = streamflow_metrics[,'GAGE_ID.fact'], function(y) mean(y[y$before_after_f == 'prefire',]$annual_tot))

#Cleaning data
#Removing NULL lists 
streamflow_metrics.lsna<-Filter(function(x) length(x) > 0, streamflow_metrics.ls) #currently 214 watersheds
#lapply(streamflow_metrics.lsna, function(x) boxplotfun(x)) # This just plots all list objects

#Trying to figure out which data frames have all 3 fire status (I could combine into an lapply ifesle statement)
firestate<-lapply(streamflow_metrics.lsna, function(x) nlevels(x[['before_after_f']]) == 3)
streamflow_metrics_prepost <- streamflow_metrics.lsna
for (i in 1:length(firestate)) {
  if(firestate[[i]]==TRUE){
    streamflow_metrics_prepost[i][[1]] <- streamflow_metrics.lsna[i][[1]]
  }else{
    streamflow_metrics_prepost[i][[1]] <- 999
  }
  
}

#which ones do not have pre,3yr,post statements
which(lengths(streamflow_metrics_prepost) == 1)
streamflow_metrics_prepost<-Filter(function(x) length(x) > 1, streamflow_metrics_prepost) #144 watersheds
#lapply(streamflow_metrics_prepost, function(x) boxplotfun(x)) #Visual aid to see if there is data from pre and post fire

#Scale values 
streamflow_metrics_scaled<-streamflow_metrics_prepost
scale_fun <- function(x) {scale(x[c("peakflow_cfs","meanslope","meanslope_ma", "annual_tot" ,"annual_mean", "baseflow" , "peakflow_cms","max_ma30")])}
for (i in 1:length(streamflow_metrics_scaled)) {
  streamflow_metrics_scaled[[i]][c("peakflow_cfs","meanslope","meanslope_ma", "annual_tot" ,"annual_mean","baseflow" , "peakflow_cms","max_ma30")]<-
    scale_fun(streamflow_metrics_scaled[[i]])
}

###DONE WITH CLEANING DATA
###SAVE streamflow_metrics table as a more specific name!####
#Example: 
#streamflow_metrics_df<-rbindlist(streamflow_metrics_prepost)
#write.csv(streamflow_metrics_df,'~/Desktop/EcoForecasting/streamflow_metrics.csv')
#streamflow_metrics<-read.csv('streamflow_metrics.csv')

percentfire_snow.df<-data.frame(length=1:100,percentsnow=NA, percenfire=NA)
df_raw<-rbindlist(streamflow_metrics_scaled)
df_raw$GAGE_ID.f<-as.factor(df_raw$GAGE_ID)
df_raw$snow_per<- df_raw$snow*100



