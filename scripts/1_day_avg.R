################################################################################
# This script is used for plotting TROPOMI data.
# Author: Will Daniels
# Date: 2/28/2020
################################################################################

rm(list = ls())

library(fields)
library(lubridate)

# This can be either "methane", "carbon_monoxide", or "total_column_ozone"
# Change it to the data product you are using.
data.product <- "methane"

# Change this directory to the directory where your monthly aggregates are.
#working.dir <- paste("/home/wdaniels/Documents/research/tropomi_project/data/", data.product, "/",
#                     "high_res_all",
#                     sep = "")
#setwd(working.dir)

working.dir <- '/home/wdaniels/Documents/research/tropomi_project/data/methane/ne_co/'
setwd(working.dir)

# Define quilt.plot grid size.
# I made the grid pretty fine to avoid averaging out subtle features.
n.x.grid <- 40
n.y.grid <- 40

# This defines the zlim. 
# The zlim is set to: mean(data) +/- sd.multiplier * sd(data)
# 2 works well, but 3 captures more data.
sd.multiplier <- 2.75

# Create structure that tells quilt.plot which days to average.
# The list is broken up into two parts because two plots are produced
avg.windows <- list()
avg.windows[[1]] <- cbind(c(1,3), c(4,6), c(7,9), c(10,12), c(13,15), c(16,18), 
                          c(19,21), c(22,24), c(25,27), c(28,31))


# Create list of monthly aggregate files
files <- list.files(path = working.dir)
files <- files[endsWith(files, ".RData")]

files <- files[2:length(files)]

# Initialize variables
running.mean <- 0
running.sd <- 0
running.size <- 0
min.lat <- 999
min.lon <- 999
max.lat <- -999
max.lon <- -999

# Quickly loop through all files and pick out important information
for (file in files){
  
  this.file <- readRDS(file)
  
  # Get spatial bound on data
  min.lat <- min(min.lat, min(this.file$lat))
  min.lon <- min(min.lon, min(this.file$lon))
  max.lat <- max(max.lat, max(this.file$lat))
  max.lon <- max(max.lon, max(this.file$lon))
  
  # Compute mean and sd without loading all data into memory at once
  this.mean <- mean(this.file$gas)
  this.sd <- sqrt(var(this.file$gas))
  this.size <- nrow(this.file)
  
  combined.mean <- (running.mean * running.size + this.mean * this.size) / (running.size + this.size)
  combined.sd <- sqrt(
    ( # begin numerator
      (running.size - 1)*running.sd^2 + # first term in numerator
        (this.size - 1)*this.sd^2 +     # second term in numerator  
        (running.size * this.size / (running.size + this.size))*(running.mean^2 + this.mean^2 - 2*running.mean*this.mean) 
    ) # end numerator
    / (running.size + this.size - 1) #denominator
  )
  combined.size <- running.size + nrow(this.file)
  
  running.mean <- combined.mean
  running.sd <- combined.sd
  running.size <- combined.size
  
}

# Clean up workspace
gas.stats <- data.frame("mean" = running.mean, "sd" = running.sd, "n" = running.size)
rm(this.file, combined.mean, combined.sd, combined.size, file, running.mean, running.sd, running.size, 
   this.mean, this.sd, this.size)


# Loop through months
for (file in files){
  
  data <- readRDS(file)
  month.num <- month(as_datetime(data$time[1]))
  year.num <- year(as_datetime(data$time[1]))
  
  for (p in 1:1){
    
    # Create plot file
    png(filename = paste("/home/wdaniels/Documents/research/tropomi_project/plots/", 
                         data.product,"/avg_",
                         year.num, "_",
                         month.num,
                         ".png",
                         sep = ""),
        width = 2400, height = 2800, res = 300)
    
    par(mfrow = c(5,2),
        oma = c(2,1,4,2),
        mar = c(2,4,2,4))
    
    # Loop through the averaging windows defined earlier. Each one gets its own plot
    for (avg.window in 1:ncol(avg.windows[[p]])){
      
      # Determine the total number of data points
      num.data <- length(data$gas[ month(as_datetime(data$time)) == month.num & 
                                     day(as_datetime(data$time)) %in% 
                                     avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] ])
      
      
      # Leave space blank if no data
      if (num.data == 0){
        plot.new()
      } else{
        
        # Plot all data
        quilt.plot(x = data$lon[ month(as_datetime(data$time)) == month.num &
                                   day(as_datetime(data$time)) %in% 
                                   avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] ],
                   
                   y = data$lat[month(as_datetime(data$time)) == month.num & 
                                  day(as_datetime(data$time)) %in% 
                                  avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] ],
                   
                   z = data$gas [month(as_datetime(data$time)) == month.num & 
                                   day(as_datetime(data$time)) %in% 
                                   avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] ],
                   
                   zlim = c(gas.stats$mean - sd.multiplier*gas.stats$sd, gas.stats$mean + sd.multiplier*gas.stats$sd),
                   #main = paste("n1 = ", format(num.data,big.mark=",",scientific=FALSE), sep = ""),
                   nx = n.x.grid,
                   ny = n.y.grid,
                   xlim = c(round(min.lon), round(max.lon)),
                   ylim = c(round(min.lat), round(max.lat)))
        US(add = T)
        world(add = T)
        
        # Add plot titles
        mtext(side = 3, line = 0, paste("Days ",
                                        avg.windows[[p]][1,avg.window],
                                        "-",
                                        avg.windows[[p]][2,avg.window],
                                        ".   ",
                                        "n = ", format(num.data,big.mark=",",scientific=FALSE), ".",
                                        sep = ""),
              cex = 0.8, font = 2)
      }
      
    } # End loop through averaging windows
    
    # Add month name to plot
    this.month.name <- month(month.num, label = T, abbr = F)
    
    mtext(paste(this.month.name, "\n", year.num, sep = ""), outer = T, cex = 1.5, line = 0)
    
    dev.off()
    
  }
  
} # End loop through months


