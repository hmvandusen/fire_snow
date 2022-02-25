###
###Streamflow time series analysis
##01-28-2022


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


# identifying parameters for data download
siteNumber <- "13313000" # USGS gauge number
parameterCd <- "00060"  # mean daily discharge in cfs
startDate <- "1986-10-1" # ~ 5 years prefire: incorportating start of water year
#(this is before MODIS data):if we wanted to be in MODIS timeframe it would be 3 years pre & post
endDate <- "2022-01-01" # 5 years postfire

# download data using readNWISdv function
johnsoncreek_all <- dataRetrieval::readNWISdv(siteNumber, parameterCd, startDate, endDate)
head(johnsoncreek_all) # check
colnames(johnsoncreek_all)[4]<- 'discharge_cfs'
johnsoncreek_all<-johnsoncreek_all[,-5]

#Making time series in water year
#I am seperating the date for each column 
johnsoncreek_all$year <-lubridate::year(johnsoncreek_all$Date) # add year col
johnsoncreek_all$month <-lubridate::month(johnsoncreek_all$Date) # add month col
johnsoncreek_all$day <-lubridate::day(johnsoncreek_all$Date) # add day of month col
johnsoncreek_all$doy <-lubridate::yday(johnsoncreek_all$Date) # add day of year col

#Plots of all years from 1986 - 2021
plot(johnsoncreek_all$Date, johnsoncreek_all$discharge_cfs, type='l', ylab='Discharge (cfs)', xlab='Time (days)')
abline(v=as.Date('2007-07-17'), col='red')

#Plots of 5 years before and after
plot(johnsoncreek_all[johnsoncreek_all$Date>=as.Date('2002-01-01') & 
                        johnsoncreek_all$Date<=as.Date('2012-01-01'),]$Date, 
     johnsoncreek_all[johnsoncreek_all$Date>=as.Date('2002-01-01') & 
                        johnsoncreek_all$Date<=as.Date('2012-01-01'),]$discharge_cfs, type='l', 
     ylab='Discharge (cfs)', xlab='Time (days)')
abline(v=as.Date('2007-07-17'), col='red')

#Create only 10 year timeframe of around 
johnsoncreek_10<-johnsoncreek_all[johnsoncreek_all$Date>=as.Date('2002-01-01') & 
                                    johnsoncreek_all$Date<=as.Date('2013-01-01'),]
plot(johnsoncreek_10[johnsoncreek_10$Date>=as.Date('2005-01-01') & 
                       johnsoncreek_10$Date<=as.Date('2006-01-01'),]$Date, 
     johnsoncreek_10[johnsoncreek_10$Date>=as.Date('2005-01-01') & johnsoncreek_10$Date<=as.Date('2006-01-01'),]$discharge_cfs, 
     type='l', ylab='Discharge (cfs)', xlab='Time (days)') #This shows that peak discharge is during mid May 


#Use the time series package ts to see seasonality and an long-term trend 
#This is will all stream flow data 1986 to s2022
johnsoncreek_all.ts<-ts(johnsoncreek_all$discharge_cfs, start = c(1986, 01), end = c(2022,01), frequency = 365)
plot(johnsoncreek_all.ts)
johnson_all_add = decompose(johnsoncreek_all.ts, type = 'additive') #The moving average window is unknown
plot(johnson_all_add) #clear seasonal trend, but not a clear long-term trend. Unknown what the moving average is
plot(johnson_all_add$seasonal[1:365], type = 'l') #This seasonal trend starts in June and peaks in September. 
                                                  #A narrow stream flow seasonal peak 


#use USGS methond for "running"rolling" mean
#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(johnsoncreek_10$Date))) != (nrow(johnsoncreek_10)+1)){
  fullDates <- seq(from=min(johnsoncreek_10$Date),
                   to = max(johnsoncreek_10$Date), by="1 day")
  fullDates <- data.frame(Date = fullDates, 
                          agency_cd = johnsoncreek_10$agency_cd[1],
                          site_no = johnsoncreek_10$site_no[1],
                          stringsAsFactors = FALSE)
  johnsoncreek_10 <- full_join(johnsoncreek_10, fullDates,
                      by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

#creating 30 day average
#This averages the previous 29 day and the current day (30) to get a moving average
rollmean <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=1)}
johnsoncreek_10$rollmean <- as.numeric(rollmean(johnsoncreek_10$discharge_cfs)) 

##calculating 30 day moving average from ts package. Too see a difference
#This is just with the 10 year window 2002-10-01 to 2012-10-01
johnsoncreek_10.ts<-ts(johnsoncreek_10$discharge_cfs, start = c(2002, 01), end = c(2013, 01), frequency = 365)
plot(johnsoncreek_10.ts)
johnson_10_add = decompose(johnsoncreek_10.ts, type = 'additive') #The moving average window is unknown
plot(johnson_10_add) #clear seasonal trend, but not a clear long-term trend. Unknown what the moving average is
#It appears that after the fire the runoff peaks are higher
plot(johnson_10_add$seasonal[1:365], type = 'l') #This seasonal doesn't when peaks occurs 

##Con. calculating 30 day moving average from ts package
johnsoncreek_10.ts.MA<-ma(johnsoncreek_10.ts, order = 30, centre = TRUE)
plot(johnsoncreek_10.ts.MA)
johnsoncreek_10.ts.MA.df<-data.frame(Y=as.matrix(johnsoncreek_10.ts.MA), date=as.Date(as.yearmon(time(johnsoncreek_10.ts.MA))))
names(johnsoncreek_10.ts.MA.df)[1]<-c('ma30')
n<-dim(johnsoncreek_10)[1]
johnsoncreek_10.ts.MA.df$Date<-johnsoncreek_10$Date[1:(n-3)]

#this is a weird step but the ts object drops the last 3 entries, so I am just dropping it for all entries
johnsoncreek_10.2<-full_join(johnsoncreek_10[(1:(n-3)),],johnsoncreek_10.ts.MA.df[,c("ma30","Date")])

#ploting 30-day rolling mean vs 30-day moving average
#It appears that the peaks are the same magnitude when the rolling average preceeds it by a bit
plot(johnsoncreek_10.2$Date,johnsoncreek_10.2$rollmean, type='l', col='red', xlim=as.Date(c("2002-01-01", "2013-01-01")))
lines(johnsoncreek_10.2$Date, johnsoncreek_10.2$ma30)


####
####
##Calculating change in rolling/running/moving average 





