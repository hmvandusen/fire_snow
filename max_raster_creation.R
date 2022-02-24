# oct 7th 2020
# start of pixel wise sens slop analysis 
# need to firgure out the  best way to cut full year vertical columns out for anlaysis and stich them back

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(parallel)
library(data.table)
library(gtools)
library(terra)
library(spatialEco)
library(EnvStats)
library(Kendall)
library(remotes)
remotes::install_github("ffilipponi/rtsa")


# oct 12
# keeping data in a matrix instead of raster for processing
# this way we can avoid geolocation issues

#tempdir <- function() { "/Volumes/jt/projects/margulis/temp" }
#tempdir()

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/MJ_INFEWS/Hannah Files/Margulis/swe/hdf/" #create path
#path <-file.path( hdf_path , hdf_name ) 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm


max_raster <- function( hdf_name ) {
  
  path <-file.path( hdf_path , hdf_name ) 
  
  c1 <-h5read(path, "/SWE", index = list(1:1000,1:5701,1:365)) #load in 
  c1[ c1[] == -32768 ] <- NA #remove NA
  max_c1 <-as.matrix(apply(c1, c(1,2), max)) #creat matrix with max value on z axis
  rm(c1) 
  
  c2 <-h5read(path, "/SWE", index = list(1001:2000,1:5701,1:365))
  c2[ c2[] == -32768 ] <- NA
  max_c2 <-as.matrix(apply(c2, c(1,2), max))
  rm(c2)
  h5closeAll()
  
  c3 <-h5read(path, "/SWE", index = list(2001:3000,1:5701,1:365))
  c3[ c3[] == -32768 ] <- NA
  max_c3 <-as.matrix(apply(c3, c(1,2), max))
  rm(c3)
  h5closeAll()
  
  c4 <-h5read(path, "/SWE", index = list(3001:4000,1:5701,1:365))
  c4[ c4[] == -32768 ] <- NA
  max_c4 <-as.matrix(apply(c4, c(1,2), max))
  rm(c4)
  h5closeAll()
  
  c5 <-h5read(path, "/SWE", index = list(4001:5000,1:5701,1:365))
  c5[ c5[] == -32768 ] <- NA
  max_c5 <-as.matrix(apply(c5, c(1,2), max))
  rm(c5)
  h5closeAll()
  
  c6 <-h5read(path, "/SWE", index = list(5001:6000,1:5701,1:365))
  c6[ c6[] == -32768 ] <- NA
  max_c6 <-as.matrix(apply(c6, c(1,2), max))
  rm(c6)
  h5closeAll()
  
  c7 <-h5read(path, "/SWE", index = list(6001:6601,1:5701,1:365))
  c7[ c7[] == -32768 ] <- NA
  max_c7 <-as.matrix(apply(c7, c(1,2), max))
  rm(c7)
  h5closeAll()
  
  #bind chunks together
  full_max <-rbind(max_c1,max_c2,max_c3,max_c4,max_c5,max_c6,max_c7)
  rast <-raster(full_max, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
  plot(rast)
  hist(rast)
  
  name <- gsub(".h5", "", hdf_name)
  good_name <- gsub("SN_SWE_", "max_swe_", name)
  
  setwd("/Volumes/jt/projects/margulis/max_rasters/")
  writeRaster(rast, paste0(good_name, ".tif"))
  return(rast)
}


#### apply to hdf list

setwd("/Volumes/jt/projects/margulis/swe/hdf")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(raster_list <-lapply(hdf_list, function(x) max_raster(x)))


###### oct 13th 
# packages for doing pixelwise sen's slope

# read in rasters to stack
setwd("/Volumes/jt/projects/margulis/max_rasters/")
list <-list.files()
max_list <-lapply(list, function(x) raster(x))
max_stack <-stack(max_list) 
crs(max_stack)<-"+proj=leac +ellps=clrk66"
plot(max_stack[[9]])
max_stack

# test running on top 1000 rows
top1deg <-crop(max_stack, extent(-123.3, -117.6, 41, 42)) #convert max stack to array
top1deg_stack <-stack(top1deg)
top1deg
plot(top1deg[[9]])


writeRaster(top_5th, "/Volumes/jt/projects/margulis/mk_results/top_5th.tif")
top5 <-stack("/Volumes/jt/projects/margulis/mk_results/top_5th.tif")
crs(top5)<-"+proj=leac +ellps=clrk66"


max_results <-stack("/Volumes/jt/projects/margulis/mk_results/max_results.tif")
#crs(max_results)<-"+proj=leac +ellps=clrk66"
#plot(max_results[[1]])
#max_results

###### mk test
trend.slope <- function(y, p.value.pass = TRUE, z.pass = TRUE, 
                        tau.pass = TRUE, confidence.pass = TRUE, intercept.pass = TRUE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}

trend.slope2 <- function(y, p.value.pass = TRUE, z.pass = FALSE, 
                        tau.pass = FALSE, confidence.pass = FALSE, intercept.pass = FALSE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}


# whotle thing
beginCluster(n=3)

system.time(max_results <- clusterR(max_stack, overlay, args=list(fun=trend.slope)))

endCluster()

########### top 5th





?raster.kendall

top1deg_results2
plot(top1deg_results2[[1]])
########

plot(max_results)
max_results_stack <-stack(max_results)
crs(max_results_stack)<-"+proj=leac +ellps=clrk66"

p_value <-max_results[[2]]
slope <-max_results[[1]]
writeRaster(p_value,"/Volumes/jt/projects/margulis/mk_results/p_value.tif")
writeRaster(slope, "/Volumes/jt/projects/margulis/mk_results/slope.tif")

#?cluster
#?system.time

##### testinng with smaller size

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf" #create path
hdf_name <- "SN_SWE_WY2016.h5"
path <-file.path( hdf_path , hdf_name ) 
h5closeAll()
h5readAttributes(path, name = "SWE") #$units = mm

test_chunk <-h5read(path, "/SWE", index = list(1:100, 1:5701, 150:186)) #load in 
test_rast <-stack(brick(test_chunk))

k <- raster.kendall(test_rast, p.value=TRUE, z.value=FALSE,
                    intercept=FALSE, confidence=FALSE,
                    tau=FALSE)

beginCluster(n=3)

system.time(max_results <- clusterR(test_rast, overlay, args=list(fun=trend.slope2)))

endCluster()




#fun_kendall <-function(x){ return(unlist(MannKendall(x)))}
#kendall_result <-calc(test_rast, fun_kendall)
#kendall_result
#plot(kendall_result)
#beginCluster(n=3)
?MannKendall





test_results <- clusterR(test_rast, calc, args=list(fun=trend.slope2))

endCluster()

plot(test_results)
test_results
