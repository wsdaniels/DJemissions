library(fields)
library(foreign)

rm(list = ls())

dir.name <- "/home/wdaniels/Documents/research/tropomi_project/data/methane/ne_co/"

gas.data <- readRDS(paste(dir.name, "all.RData", sep = ""))
well.data <- read.dbf(paste(dir.name, "Wells.dbf", sep = ""))

lat.bounds <- c(38,43)
lon.bounds <- c(-107, -100)
n.x.grids <- 64
n.y.grids <- 64

lat.grid <- seq(from = lat.bounds[1], to = lat.bounds[2], length.out = n.y.grids+1)
lon.grid <- seq(from = lon.bounds[1], to = lon.bounds[2], length.out = n.x.grids+1)


well.density <- matrix(nrow = n.x.grids, ncol = n.y.grids)

for (x in 1:n.x.grids){
  for (y in 1:n.y.grids){
    
    well.density[x,y] <- length(well.data$API[well.data$Longitude >= lon.grid[x] & 
                                                well.data$Longitude < lon.grid[x+1] &
                                                well.data$Latitude >= lat.grid[y] &
                                                well.data$Latitude < lat.grid[y+1]])
    
  }
}

mult <- 3

quilt.plot(x = gas.data$lon, 
           y = gas.data$lat,
           z = gas.data$gas,
           zlim = c(mean(gas.data$gas) - mult * sd(gas.data$gas) , 
                    mean(gas.data$gas) + mult * sd(gas.data$gas)))



library(plot.matrix)

plot(well.density, col = heat.colors)

# Default call (as object)
p <- ggplot(df, aes(x,y))
h3 <- p + stat_bin2d()
h3

# Default call (using qplot)
qplot(x,y,data=df, geom='bin2d')


heatmap(well.density, scale = "none")

