rm(list = ls())

library(ggmap)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(usmap)

setwd('/home/wdaniels/Documents/research/tropomi_project/scripts')
source("functions.R")

data <- readRDS('../data/methane/ne_co/all.RData')
data <- data[order(data$time),]

sd.multiplier <- 3
zlim.upper <- mean(data$gas) + sd.multiplier * sd(data$gas)
zlim.lower <- mean(data$gas) - sd.multiplier * sd(data$gas)

city.lon <- c(-104.991309, -105.279522, -105.079865, -104.821473, -104.809701)
city.lat <- c(39.748102, 40.018350, 40.582626, 38.835280, 41.141464)
city.names <- c("Denver", "Boulder", "Fort Collins", "Colorado Springs", "Cheyenne")
cities <- data.frame("lon" = city.lon, "lat" = city.lat, "names" = city.names)

ggmap::register_google(key = "AIzaSyCwpdBLfC_5uOD9bKbnI4HYkc-uzowcqdU")

lat.bound <- round(range(data$lat))
lon.bound <- round(range(data$lon))

res <- 64
lat.edges <- seq(lat.bound[1], lat.bound[2], length = res + 1)
lon.edges <- seq(lon.bound[1], lon.bound[2], length = res + 1)
lat.centers <- rollmean(lat.edges, 2)
lon.centers <- rollmean(lon.edges, 2)

grid.h <- round(lat.centers[2] - lat.centers[1], 5)
grid.w <- round(lon.centers[2] - lon.centers[1], 5)

quad.map <- generate.quad.map(res)
data <- assign.quadrants(data, lat.edges, lon.edges, res)

year.day <- assign.days(as_datetime(data$time))
year.week <- assign.weeks(as_datetime(data$time))
year.month <- assign.months(as_datetime(data$time))
year.season <- assign.seasons(as_datetime(data$time))

year.week[year.week == '2018-53'] <- '2018-52'
year.week[year.week == '2019-53'] <- '2019-52'

unique.days <- unique(year.day)
unique.weeks <- unique(year.week)
unique.months <- unique(year.month)
unique.seasons <- unique(year.season)

data.day <- data.aggregate(data, year.day, unique.days, 'Day')
data.week <- data.aggregate(data, year.week, unique.weeks, 'Week')
data.month <- data.aggregate(data, year.month, unique.months, 'Month')
data.season <- data.aggregate(data, year.season, unique.seasons, 'Season')

dec.2019 <- data.season$`Season2019-Winter`[month(as_datetime(data.season$`Season2019-Winter`$time)) == 12, ]
jan.feb.2019 <- data.season$`Season2019-Winter`[month(as_datetime(data.season$`Season2019-Winter`$time)) != 12, ]
data.season[5] <- NULL

names(data.season)[4] <- "Season2018-2019-Winter"
names(data.season)[8] <- "Season2019-2020-Winter"

data.season$`Season2018-2019-Winter` <- rbind(data.season$`Season2018-2019-Winter`, jan.feb.2019)
data.season$`Season2019-2020-Winter` <- rbind(dec.2019, data.season$`Season2019-2020-Winter`)

rm(dec.2019, jan.feb.2019)

averaged.data.day <- grid.average(data.day)
averaged.data.week <- grid.average(data.week)
averaged.data.month <- grid.average(data.month)
averaged.data.season <- grid.average(data.season)

w.correction <- 0.01
h.correction <- 0.01

wide.lon.bound <- c(lon.bound[1]-w.correction, lon.bound[2]+w.correction)
wide.lat.bound <- c(lat.bound[1]-h.correction, lat.bound[2]+h.correction)

lon.bound <- c(-107, -100)
lat.bound <- c(38, 43)

map <- get_map(location = c(mean(wide.lon.bound), mean(wide.lat.bound)),
               source = 'google', maptype = 'toner', zoom = 3)
map <- ggmap(map)
map <- map + scale_x_continuous(limits = lonBounds, expand = c(0, 0))
map <- map + scale_y_continuous(limits = latBounds, expand = c(0, 0))




library(maps)
us_states <- map_data("state")
p <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group), alpha = 0.2)

p <- p + geom_polygon(fill = "white", color = "black", alpha = 0.2)
  
p <- p + geom_rect(mapping = aes(xmin=lon.bound[1], xmax=lon.bound[2], ymin=lat.bound[1], ymax=lat.bound[2]), 
                   alpha = 0.02, fill = "palegreen")

p <- p + labs(x = "Longitude", y = "Latitude")

p



basemap <- get.basemap('google', 'toner', wide.lon.bound, wide.lat.bound); basemap

season.list <- generate.season.list(data)

for (l in 1:length(averaged.data.season)){
  
  df <- averaged.data.season[[l]]
  
  lon.quad <- vector(length = nrow(df))
  lat.quad <- vector(length = nrow(df))
  for (i in 1:nrow(df)){
    these.coords <- which(quad.map == df$quadrant[i], arr.ind = T)
    lon.quad[i] <- these.coords[2]
    lat.quad[i] <- these.coords[1]
  }
  
  lon.quad <- lon.centers[lon.quad]
  lat.quad <- lat.centers[lat.quad]
  
  df <- cbind(df, lat.quad, lon.quad)
  
  g <- basemap
  g <- g + geom_tile(data = df, height = grid.h, width = grid.w, alpha = 0.4,
                     aes(x = lon.quad, y = lat.quad, fill = gas) )
  g <- g + scale_fill_gradientn(colors = hcl.colors(16, "Spectral", rev = TRUE),
                                limits = c(zlim.lower, zlim.upper))
  
  g <- g + ggtitle("Mean Methane") + xlab('Longitude') + ylab('Latitude')
  g <- g + theme(legend.key.width = unit(2.75, 'cm'))
  g <- g + theme(legend.position = 'bottom')
  g <- g + guides(fill = guide_colorbar(title = "Mean Methane", title.position = 'top'))
  g <- g + geom_point(cities, mapping = aes(x = lon, y = lat))
  g <- g + geom_text(cities, mapping = aes(x = lon+0.05, y = lat+0.05, label=names),hjust=0, vjust=0)
  #g <- g + geom_polygon(data = these.states, mapping = aes(x = long, y = lat, group = group), 
  fill = NA, color = "black")  
#g <- g + coord_fixed(xlim = c(lon.bound[1], lon.bound[2]),  ylim = c(lat.bound[1], lat.bound[2]), 
ratio = 1.3)

png(filename = paste('../plots/ne_co_ggplot/', l, '.png', sep = ""),
    width = 2800, height = 4000, res = 300)
print(g)
dev.off()

}




