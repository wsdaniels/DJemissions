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
sd.multiplier <- 3


######################################################################################
# You shouldn't have to change anything below this line
######################################################################################


# Set working directory
setwd(working.dir)

# Create structure that tells quilt.plot which days to average.
# The list is broken up into two parts because two plots are produced
avg.windows <- list()
avg.windows[[1]] <- cbind(c(1,1), c(2,2), c(3,3), c(4,4), c(5,5), c(6,6), c(7,7))
avg.windows[[2]] <- cbind(c(8,8), c(9,9), c(10,10), c(11,11), c(12,12), c(13,13), c(14,14))
avg.windows[[3]] <- cbind(c(15,15), c(16,16), c(17,17), c(18,18), c(19,19), c(20,20), c(21,21))
avg.windows[[4]] <- cbind(c(22,22), c(23,23), c(24,24), c(25,25), c(26,26), c(27,27), c(28,28))
avg.windows[[5]] <- cbind(c(29,29), c(30,30), c(31,31))


# Create city points
city.lon <- c(-104.991309, -105.279522, -105.079865, -104.821473, -104.809701)
city.lat <- c(39.748102, 40.018350, 40.582626, 38.835280, 41.141464)
city.names <- c("Denver", "Boulder", "Fort Collins", "Colorado Springs", "Cheyenne")
cities <- data.frame("lon" = city.lon, "lat" = city.lat, "names" = city.names)

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
  
  # This loops through the number of .png files per month
  # It is currently set up to make 10 plots per png, or two png files per month
  for (p in 1:length(avg.windows)){
    
    # Create plot file
    png(filename = paste(outfile.dir, year.num, "_", month.num, "_part_", p, ".png", sep = ""),
        width = 1200, height = 2000, res = 150)
    
    par(mfrow = c(7,2),
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
      if (with.qa <= 1){
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
        points(x = cities$lon, y = cities$lat, pch = 19, cex = 0.8)
        text(x = cities$lon, y = cities$lat, labels = cities$name, pos = 4, cex = 0.8)
      }
      
      # Add column labels
      if (avg.window == 1){
        mtext(side = 3, line = 3, "All Data", font = 2)
      }
      
      # Add row labels
      mtext(side = 2, line = 3, paste("Day ",
                                      avg.windows[[p]][1,avg.window],
                                      sep = ""),
            cex = 1, font = 2)
      
      # Leave blank if no data
      if (without.qa <= 1){
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
        points(x = cities$lon, y = cities$lat, pch = 19, cex = 0.8)
        text(x = cities$lon, y = cities$lat, labels = cities$name, pos = 4, cex = 0.8)
      }
      
      # Row labels
      if (avg.window == 1){
        mtext(side = 3, line = 3, "High Quality Data Only", font = 2)
      }
      
    } # End loop through averaging windows
    
    # Add month name to plot
    this.month.name <- month(month.num, label = T, abbr = F)
    
    mtext(paste(this.month.name, "\n", year.num, sep = ""), outer = T, cex = 1.5, line = 0)
    
    dev.off()
    
  } # End loop through number of .png files
  
} # End loop through months


