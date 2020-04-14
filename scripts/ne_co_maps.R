################################################################################
# This script is used for plotting TROPOMI data.
# Author: Will Daniels
# Date: 2/28/2020
################################################################################

rm(list = ls())
library(fields)
library(lubridate)

# Change this to the directory where your monthly aggregate .RData files are.
working.dir <- "/home/wdaniels/Documents/research/tropomi_project/data/methane/ne_co"

# Change this to the directory in which you want to save the plots
outfile.dir <- "/home/wdaniels/Documents/research/tropomi_project/plots/ne_co_methane/"

# Define quilt.plot grid size.
n.x.grid <- 40
n.y.grid <- 40

# This defines the zlim. 
# The zlim is set to: mean(data) +/- sd.multiplier * sd(data)
# 2 works well, but 3 captures more data.
sd.multiplier <- 3

# Create city points
city.lon <- c(-104.991309, -105.279522, -105.079865, -104.821473, -104.809701)
city.lat <- c(39.748102, 40.018350, 40.582626, 38.835280, 41.141464)
city.names <- c("Denver", "Boulder", "Fort Collins", "Colorado Springs", "Cheyenne")
cities <- data.frame("lon" = city.lon, "lat" = city.lat, "names" = city.names)

# Set working directory
setwd(working.dir)

# Data file
file <- "all.RData"

# Get data
data <- readRDS(file)
data <- data[order(data$time),]

# Get spatial bound on data
min.lat <- min(data$lat)
min.lon <- min(data$lon)
max.lat <- max(data$lat)
max.lon <- max(data$lon)

# Get min / max gas data
min.gas <- min(data$gas)
max.gas <- max(data$gas)

# Compute mean and sd and n
this.mean <- mean(data$gas)
this.sd   <- sqrt(var(data$gas))
this.size <- nrow(data)

# Clean up workspace
gas.stats <- data.frame("mean" = this.mean, "sd" = this.sd, "n" = this.size, 
                        "min" = min.gas, "max" = max.gas)
spat.stats <- data.frame("min.lon" = min.lon, "max.lon" = max.lon, 
                         "min.lat" = min.lat, "max.lat" = max.lat)

rm(file, this.mean, this.sd, this.size, min.gas, max.gas, min.lon, max.lon, min.lat, max.lat)

seasons <- cbind(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
colnames(seasons) <- c("Winter", "Spring", "Summer", "Fall")

years <- unique(year(as_datetime(data$time)))

month.list <- vector()
year.list <- vector()

for (this.year in years){
  these.months <- unique(month(as_datetime(data$time[ year(as_datetime(data$time)) == this.year ])))
  these.years <- rep(this.year, length(these.months))
  month.list <- c(month.list, these.months)
  year.list <- c(year.list, these.years)
}

season.list <- vector()

for (m in 1:length(month.list)){
  if (month.list[m] %in% seasons[,1]) { season.list[m] <- 1 }
  if (month.list[m] %in% seasons[,2]) { season.list[m] <- 2 }
  if (month.list[m] %in% seasons[,3]) { season.list[m] <- 3 }
  if (month.list[m] %in% seasons[,4]) { season.list[m] <- 4 }
}



season.mask <- T
for (i in 2:length(season.list)){
  if (season.list[i] == season.list[i-1]){
    season.mask[i] <- F
  } else {
    season.mask[i] <- T
  }
}

unique.seasons <- season.list[season.mask]

main.season.list <- list()
temp.season <- vector()
temp.year <- vector()
temp.month <- vector()
start.index <- 1

for (i in 1:length(unique.seasons)){
  for (j in start.index:length(season.list)){
    if ( (j == start.index) || (season.list[j] == season.list[j-1]) ){
      
      temp.season <- c(temp.season, season.list[j])
      temp.year <- c(temp.year, year.list[j])
      temp.month <- c(temp.month, month.list[j])
      
    } else {
      start.index <- j
      break
    }
  }
  
  main.season.list[[i]] <- data.frame("seasons" = temp.season,
                                      "years"   = temp.year,
                                      "months"  = temp.month)
  temp.season <- vector()
  temp.year <- vector()
  temp.month <- vector()
}

rm(temp.month, temp.season, temp.year, start.index,
   season.mask, these.months, these.years, this.year,
   month.list, year.list, season.list, unique.seasons, years, i,j,m)

for (s in 1:length(main.season.list)){
  
  png(filename = paste(outfile.dir,
                       "ne_co_season_number_",
                       s,
                       "_out_of_",
                       length(main.season.list),
                       ".png",
                       sep = ""),
      width = 1400, height = 2000, res = 150)
  
  par(mfrow = c(4,2),
      oma = c(4,4,4,4),
      mar = c(4,4,4,6))
  
  num.months <- length(main.season.list[[s]]$seasons)
  start.offset <- (main.season.list[[s]]$months[1] - seasons[1,unique(main.season.list[[s]]$seasons)])[[1]]
  label.flag <- T
  
  if ( start.offset != 0){
    for (j in 1:start.offset){
      plot.new() # leave all data plot blank
      mtext(side = 3, line = 3, "All Data", font = 2)
      plot.new() # leave high quality data plot blank
      mtext(side = 3, line = 3, "High Quality Data Only", font = 2)
      label.flag = F
    }
  } 
  
  for (i in 1:num.months){
    
    # Create data mask for the specific plot
    this.mask <- (month(as_datetime(data$time)) == main.season.list[[s]]$months[i]) & 
      (year(as_datetime(data$time)) == main.season.list[[s]]$years[i])
    
    # Determine the total number of data points
    num.all.data <- length(data$gas[ this.mask ])
    
    # Determine the number of data points with QA < 0.5 
    num.good.data <- length(data$gas[ this.mask & data$qa_value > 0.5 ])
    
    # Leave space blank if no data
    if (num.all.data == 0){
      plot.new()
      if (i == 1 & label.flag){
        mtext(side = 3, line = 3, "All Data", font = 2)
      }
    } else{
      
      # Plot all data
      quilt.plot(x = data$lon[ this.mask ],
                 y = data$lat[ this.mask ],
                 z = data$gas[ this.mask ],
                 zlim = c(gas.stats$mean - sd.multiplier*gas.stats$sd, 
                          gas.stats$mean + sd.multiplier*gas.stats$sd),
                 main = paste("n1 = ", format(num.all.data,big.mark=",",scientific=FALSE), sep = ""),
                 nx = n.x.grid,
                 ny = n.y.grid,
                 xlim = c(round(spat.stats$min.lon), round(spat.stats$max.lon)),
                 ylim = c(round(spat.stats$min.lat), round(spat.stats$max.lat)),
                 add = F)
      US(add = T)
      points(x = cities$lon, y = cities$lat, pch = 19)
      text(x = cities$lon, y = cities$lat, labels = cities$name, pos = 4)
    }
    
    if (i == 1 & label.flag){
      mtext(side = 3, line = 3, "All Data", font = 2)
    }
    
    
    mtext(paste(month(main.season.list[[s]]$months[i], label = T, abbr = F),
                " ", 
                main.season.list[[s]]$years[i],
                sep = ""),
          side = 2, line = 3, cex = 1, font = 2)
    
    # Leave blank if no data
    if (num.good.data == 0){
      plot.new()
      if (i == 1 & label.flag){
        mtext(side = 3, line = 3, "All Data", font = 2)
      }
    } else{
      
      # Plot data with QA > 0.5
      quilt.plot(x = data$lon[ this.mask & data$qa_value > 0.5],
                 y = data$lat[ this.mask & data$qa_value > 0.5], 
                 z = data$gas[ this.mask & data$qa_value > 0.5],
                 zlim = c(gas.stats$mean - sd.multiplier*gas.stats$sd, 
                          gas.stats$mean + sd.multiplier*gas.stats$sd),
                 main = paste("n2 = ", 
                              format(num.good.data,big.mark=",",scientific=FALSE),
                              "  (",
                              round(100*num.good.data/num.all.data)/100 , " n1)",
                              sep = ""),
                 nx = n.x.grid, 
                 ny = n.y.grid,
                 xlim = c(round(spat.stats$min.lon), round(spat.stats$max.lon)),
                 ylim = c(round(spat.stats$min.lat), round(spat.stats$max.lat)),
                 add = F)
      US(add = T)
      points(x = cities$lon, y = cities$lat, pch = 19)
      text(x = cities$lon, y = cities$lat, labels = cities$name, pos = 4)
    }
    
    
    if (i == 1 & label.flag){
      mtext(side = 3, line = 3, "High Quality Data Only", font = 2)
    }
    
  } # end loop through months
  
  mtext(paste(colnames(seasons)[ main.season.list[[s]]$seasons[1] ], 
              "\n",
              paste(unique(main.season.list[[s]]$years), collapse = "-"),
              sep = ""),
        outer = T, cex = 1.5, line = 0)
  
  if (s == length(main.season.list)){
    end.offset <- 3 - length(main.season.list[[s]]$months)
    for (i in 1:end.offset){
      plot.new()
      plot.new()
    }
  }
  
  full.season.mask <- (month(as_datetime(data$time)) %in% main.season.list[[s]]$months) & 
    (year(as_datetime(data$time)) %in% main.season.list[[s]]$years)
  
  # Determine the total number of data points
  season.num.all.data <- length(data$gas[ full.season.mask ])
  
  # Determine the number of data points with QA < 0.5 
  season.num.good.data <- length(data$gas[ full.season.mask & data$qa_value > 0.5 ])
  
  # Plot all data
  quilt.plot(x = data$lon[ full.season.mask ],
             y = data$lat[ full.season.mask ],
             z = data$gas[ full.season.mask ],
             zlim = c(gas.stats$mean - sd.multiplier*gas.stats$sd, 
                      gas.stats$mean + sd.multiplier*gas.stats$sd),
             main = paste("n1 = ", format(season.num.all.data,big.mark=",",scientific=FALSE), sep = ""),
             nx = n.x.grid,
             ny = n.y.grid,
             xlim = c(round(spat.stats$min.lon), round(spat.stats$max.lon)),
             ylim = c(round(spat.stats$min.lat), round(spat.stats$max.lat)),
             add = F)
  US(add = T)
  points(x = cities$lon, y = cities$lat, pch = 19)
  text(x = cities$lon, y = cities$lat, labels = cities$name, pos = 4)
  
  mtext("Season Average", side = 2, line = 3, cex = 1, font = 2)
  
  # Plot data with QA > 0.5
  quilt.plot(x = data$lon[ full.season.mask & data$qa_value > 0.5],
             y = data$lat[ full.season.mask & data$qa_value > 0.5], 
             z = data$gas[ full.season.mask & data$qa_value > 0.5],
             zlim = c(gas.stats$mean - sd.multiplier*gas.stats$sd, 
                      gas.stats$mean + sd.multiplier*gas.stats$sd),
             main = paste("n2 = ", 
                          format(season.num.good.data,big.mark=",",scientific=FALSE),
                          "  (",
                          round(100*season.num.good.data/season.num.all.data)/100 , " n1)",
                          sep = ""),
             nx = n.x.grid, 
             ny = n.y.grid,
             xlim = c(round(spat.stats$min.lon), round(spat.stats$max.lon)),
             ylim = c(round(spat.stats$min.lat), round(spat.stats$max.lat)),
             add = F)
  US(add = T)
  points(x = cities$lon, y = cities$lat, pch = 19)
  text(x = cities$lon, y = cities$lat, labels = cities$name, pos = 4)
  
  dev.off()
  
} # end loop through seasons


