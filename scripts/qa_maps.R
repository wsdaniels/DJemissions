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

# Change this to the directory where your monthly aggregate .RData files are.
# working.dir <- paste("/home/wdaniels/Documents/research/tropomi_project/data/", data.product, "/",
#                      "ne_co",
#                      sep = "")

working.dir <- '/home/wdaniels/Documents/research/tropomi_project/data/methane/ne_co'

# Change this to the directory in which you want to save the plots
outfile.dir <- paste("/home/wdaniels/Documents/research/tropomi_project/plots/",
                     "ne_co_", data.product, "/",
                     sep = "")

# Define quilt.plot grid size.
# I made the grid pretty fine to avoid averaging out subtle features.
n.x.grid <- 40
n.y.grid <- 40

# This defines the zlim. 
# The zlim is set to: mean(data) +/- sd.multiplier * sd(data)
# 2 works well, but 3 captures more data.
sd.multiplier <- 2


######################################################################################
# You shouldn't have to change anything below this line
######################################################################################


# Set working directory
setwd(working.dir)

# Create structure that tells quilt.plot which days to average.
# The list is broken up into two parts because two plots are produced
avg.windows <- list()
avg.windows[[1]] <- cbind(c(1,3), c(4,6), c(7,9), c(10,12), c(13,15))
avg.windows[[2]] <- cbind(c(16,18), c(19,21), c(22,24), c(25,27), c(28,31))


# Create list of monthly aggregate files
files <- list.files(path = working.dir)
files <- files[endsWith(files, ".RData")]

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
  
  # This loops through the number of .png files per month
  # It is currently set up to make 10 plots per png, or two png files per month
  for (p in 1:2){
    
    # Create plot file
    png(filename = paste(outfile.dir, year.num, "_", month.num, "_part_", p, ".png"),
        width = 2400, height = 2800, res = 300)
    
    par(mfrow = c(5,2),
        oma = c(2,1,4,2),
        mar = c(2,4,2,4))
    
    # Loop through the averaging windows defined earlier. Each one gets its own plot
    for (avg.window in 1:ncol(avg.windows[[p]])){
      
      # Determine the total number of data points
      with.qa <- length(data$gas[ month(as_datetime(data$time)) == month.num & 
                                    day(as_datetime(data$time)) %in% 
                                    avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] ])
      
      # Determine the number of data points with QA < 0.5 
      without.qa <- length(data$gas[ month(as_datetime(data$time)) == month.num & 
                                       day(as_datetime(data$time)) %in% 
                                       avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] & 
                                       data$qa_value > 0.5])
      
      # Leave space blank if no data
      if (with.qa == 0){
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
                   main = paste("n1 = ", format(with.qa,big.mark=",",scientific=FALSE), sep = ""),
                   nx = n.x.grid,
                   ny = n.y.grid,
                   xlim = c(round(min.lon), round(max.lon)),
                   ylim = c(round(min.lat), round(max.lat)))
        US(add = T)
        world(add = T)
      }
      
      # Add column labels
      if (avg.window == 1){
        mtext(side = 3, line = 3, "With QA < 0.5", font = 2)
      }
      
      # Add row labels
      mtext(side = 2, line = 3, paste("Days ",
                                      avg.windows[[p]][1,avg.window],
                                      "-",
                                      avg.windows[[p]][2,avg.window],
                                      sep = ""),
            cex = 1, font = 2)
      
      # Leave blank if no data
      if (without.qa == 0){
        plot.new()
      } else{
        
        # Plot data with QA > 0.5
        quilt.plot(x = data$lon[month(as_datetime(data$time)) == month.num &
                                  day(as_datetime(data$time)) %in% 
                                  avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] &
                                  data$qa_value > 0.5],
                   
                   y = data$lat[month(as_datetime(data$time)) == month.num & 
                                  day(as_datetime(data$time)) %in% 
                                  avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] &
                                  data$qa_value > 0.5], 
                   
                   z = data$gas [month(as_datetime(data$time)) == month.num & 
                                   day(as_datetime(data$time)) %in% 
                                   avg.windows[[p]][1,avg.window]:avg.windows[[p]][2,avg.window] & 
                                   data$qa_value > 0.5],
                   
                   zlim = c(gas.stats$mean - sd.multiplier*gas.stats$sd, gas.stats$mean + sd.multiplier*gas.stats$sd),
                   main = paste("n2 = ", 
                                format(without.qa,big.mark=",",scientific=FALSE),
                                "  (",
                                round(100*without.qa/with.qa)/100 , " n1)",
                                sep = ""),
                   nx = n.x.grid, 
                   ny = n.y.grid,
                   xlim = c(round(min.lon), round(max.lon)),
                   ylim = c(round(min.lat), round(max.lat)))
        US( add = T)
        world( add = T)
      }
      
      # Row labels
      if (avg.window == 1){
        mtext(side = 3, line = 3, "Without QA < 0.5", font = 2)
      }
      
    } # End loop through averaging windows
    
    # Add month name to plot
    this.month.name <- month(month.num, label = T, abbr = F)
    
    mtext(paste(this.month.name, "\n", year.num, sep = ""), outer = T, cex = 1.5, line = 0)
    
    dev.off()
    
  } # End loop through number of .png files
  
} # End loop through months


