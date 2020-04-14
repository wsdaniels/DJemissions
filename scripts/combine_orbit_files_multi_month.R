################################################################################
# This script is used for aggregating data from multiple netCDF (.nc) files.
# Author: Will Daniels
# Date: 2/12/20
################################################################################

# Clear workspace and include necessary libraries
rm(list = ls())
library(ncdf4)
library(lubridate)

# Change this depending on the data product you want. Options are:
#     ``methane'' -> This will give you bias corrected methane mixing ratio
#     ``carbon_monoxide'' -> This will give you total column CO
#     ``total_column_ozone'' -> This will give you total column O3
product <- "methane"

# This file path must point to the directory that holds all of the .nc files
# This path will obviously need to be changed on your machines
# I created a directory for each data product, to make things easier
file.path <- paste("/home/wdaniels/Documents/research/tropomi_project/data/",
                   product,
                   "/2019-09-01_2019-11-01/", 
                   sep = "")

# NOTE: If a netCDF file is under 1KB, then it doesn't contain data and needs to be deleted

# Set working directory
setwd(file.path)

# Name of the .RData file to hold the data
outfile.name <- paste(file.path, "data_advanced_qa.RData", sep = "")

# Create vector of .nc file names 
# This exlcudes any files in the directory that are not netCDF files
nc.files <- list.files(file.path)
nc.files <- nc.files[endsWith(nc.files, ".nc")]

# Specify spatial data ranage
# Any data located outside of this region will not be saved
lat.lb <- 20
lat.ub <- 55
lon.lb <- -130
lon.ub <- -60

# Set base time for ``delta_time'' variable. This should remain fixed.
base.time <- as_datetime("2010-01-01 00:00:00")

# List of variables to save besides lat, lon, data product, and time
# For ozone, I save the three variables used to filter out bad data (and QA flag)
# For other data products, I just save the QA flag
if (product == 'total_column_ozone'){
  vars.to.sample <- c(
    "DETAILED_RESULTS/ozone_effective_temperature",
    "DETAILED_RESULTS/ring_scale_factor",
    "DETAILED_RESULTS/effective_albedo",
    "PRODUCT/qa_value" 
  )} else {
    vars.to.sample <- c(
      "PRODUCT/qa_value"                  
    )
  }


# Create vector of variable names (for easy readability)
num.vars <- length(vars.to.sample)
var.names <- vector(mode = "character", length = num.vars)
for (i in 1:num.vars){
  this.name <- strsplit(vars.to.sample[i], split = "/")
  var.names[i] <- this.name[[1]][[2]]
  rm(this.name)
}

# Initialize vectors to hold data
lat  <- vector()
lon  <- vector()
gas  <- vector()
time <- vector()
other.vars <- vector(mode = "list", length = num.vars)

# Loop through netCDF files
for (file in nc.files){
  
  # Open individual .nc file
  this.nc <- nc_open(file)
  
  # Ozone uses delta_time rather than UTC time
  # delta time is the number of seconds since the ``base time'' plus the ``reference time''
  # The base time is defined earlier in the script. It is the same for all .nc files
  # The reference time is the time at the begining of this particular netCDF file's orbit
  # The reference time needs to be pulled from each .nc file
  if (product == "total_column_ozone"){
    
    ref.time <- update(base.time, seconds = ncvar_get(this.nc, "PRODUCT/time"))
    
    this.time <- ncvar_get(this.nc, "PRODUCT/delta_time")
    this.time <- as.vector(this.time)
    this.time <- (this.time / 1e3) + ref.time
    
    
    # All other products use UTC time  
  } else {
    this.time <- ncvar_get(this.nc, "PRODUCT/time_utc")
  }
  
  # Get lat and lon
  this.lat <- ncvar_get(this.nc, "PRODUCT/latitude")
  this.lon <- ncvar_get(this.nc, "PRODUCT/longitude")
  
  # Get the data product itself
  if (product == "carbon_monoxide"){
    this.gas <- ncvar_get(this.nc, "PRODUCT/carbonmonoxide_total_column")
  } else if (product == "methane"){
    this.gas <- ncvar_get(this.nc, "PRODUCT/methane_mixing_ratio_bias_corrected")
  } else if (product == "tropospheric_ozone"){
    this.gas <- ncvar_get(this.nc, "PRODUCT/ozone_tropospheric_vertical_column")
  } else if (product == "total_column_ozone"){
    this.gas <- ncvar_get(this.nc, "PRODUCT/ozone_total_vertical_column")
  }
  
  # Replicate times so that each data point is associated with a time
  if (!is.na(ncol(this.lat)) & product != "total_column_ozone"){
    this.time <- t(replicate(nrow(this.lat), this.time))
  }
  
  # Boolean vector indicated which data is not NA 
  to.keep <- (!is.na(this.lat)) & (!is.na(this.lon)) & (!is.na(this.gas))
  
  # Enters loop if there is any data worth keeping
  if (any(to.keep)){ 
    
    # Remove all data corresponding to an NA value
    this.lat <- this.lat[to.keep]
    this.lon <- this.lon[to.keep]
    this.gas <- this.gas[to.keep]
    
    # Remove times corresponding to an NA
    if (product == "total_column_ozone"){
      this.time <- this.time[as.vector(to.keep)]
    } else {
      this.time <- this.time[to.keep]
    }
    
    # Remove data corresponding to an NA from all other variables
    other.vars.temp <- vector(mode = "list", length = num.vars)
    for(i in 1:num.vars){
      this.var <- ncvar_get(this.nc, vars.to.sample[i])
      this.var <- this.var[to.keep]
      other.vars.temp[[i]] <- this.var
      rm(this.var)
    }
    
    # Clear .nc file from memory
    rm(this.nc)
    
    # Define region to save
    region.cut <- 
      this.lat >= lat.lb &
      this.lat <= lat.ub &
      this.lon >= lon.lb & 
      this.lon <= lon.ub
    
    # Cut out data not within region to save
    if (!all(region.cut)){
      
      this.lat  <- this.lat[region.cut]
      this.lon  <- this.lon[region.cut]
      this.gas  <- this.gas[region.cut]
      this.time <- this.time[region.cut]
      
      for(i in 1:num.vars){
        other.vars.temp[[i]] <- other.vars.temp[[i]][region.cut]
      }
    }
    
    this.time  <- ymd_hms(this.time)
    
    lat  <- c(lat,  this.lat)
    lon  <- c(lon,  this.lon)
    gas  <- c(gas,  this.gas)
    time <- c(time, this.time)
    
    for(i in 1:num.vars){
      other.vars[[i]] <- c(other.vars[[i]], other.vars.temp[[i]])
    }
    
  } # end  if (any(to.keep))
  
  rm(this.gas, this.lat, this.lon, this.time, other.vars.temp,
     region.cut, to.keep)
  
} # end for (file in nc.files)

# Put all of the vectors into a data frame
data <- data.frame(lat,lon,time,gas)
num.perm.cols <- ncol(data)

for (i in 1:num.vars){
  data <- cbind(data, other.vars[[i]])
}

# Give the columns nice names
colnames(data)[(num.perm.cols+1):(num.perm.cols+1+num.vars-1)] <- var.names

# Save to .RData file
saveRDS(data, file = outfile.name)