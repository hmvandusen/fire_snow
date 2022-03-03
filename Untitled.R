####
####
#USGS historical quartiles 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
knitr::opts_chunk$set(echo = TRUE)
#install.packages('dataRetrieval')
library(dataRetrieval) # packaage for downloading USGS stream flow data
#install.packages('lubridate')
library(lubridate) # date management
library(forecast)
library(stlplus)
library(fpp)
library(dplyr)


#Finding Quantiles
#with dpylr 
summaryQ <- johnsoncreek_10 %>%
  group_by(doy) %>%
  summarize(p75 = quantile(rollmean, probs = .75, na.rm = TRUE),
            p25 = quantile(rollmean, probs = .25, na.rm = TRUE),
            p10 = quantile(rollmean, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rollmean, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rollmean, probs = 0, na.rm = TRUE)) 
#with base
summaryQ2<- by(johnsoncreek_10, list(johnsoncreek_10$doy), function(x)
  c(p75 = quantile(johnsoncreek_10$rollmean, probs = .75, na.rm = TRUE),
    p25 = quantile(johnsoncreek_10$rollmean, probs = .25, na.rm = TRUE),
    p10 = quantile(johnsoncreek_10$rollmean, probs = 0.1, na.rm = TRUE),
    p05 = quantile(johnsoncreek_10$rollmean, probs = 0.05, na.rm = TRUE),
    p00 = quantile(johnsoncreek_10$rollmean, probs = 0, na.rm = TRUE))
)
summaryQ2<-do.call(rbind, summaryQ2)


current.year <- as.numeric(strftime(Sys.Date(), format = "%Y"))