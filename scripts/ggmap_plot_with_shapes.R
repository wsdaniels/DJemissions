rm(list = ls())

library(ggmap)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(usmap)
library(foreign)
library(readxl)
library(sf)
library(ggplot2)

setwd('/home/wdaniels/Documents/research/tropomi_project/scripts')
source("functions.R")


#### Get Data
well.data <- read.dbf("../data/oil_and_gas/Wells.dbf")

sed.basin.shp <- st_read('../data/oil_and_gas/SedimentaryBasins_US_May2011_v2.shp')
denver.basin.shp <- sed.basin.shp[22,]

oil.plays.shp <- st_read('../data/oil_and_gas/ShalePlays_US_EIA_Sep2019.shp')
denver.niobara.shp <- oil.plays.shp[41,]

bw <- "BAYSWATER EXPLORATION & PRODUCTION LLC"
bw.data <- well.data[well.data$Operator == bw, ]

city.lon <- c(-104.991309, -105.279522, -105.079865, -104.821473, -104.809701)
city.lat <- c(39.748102, 40.018350, 40.582626, 38.835280, 41.141464)
city.names <- c("Denver", "Boulder", "Fort Collins", "Colorado Springs", "Cheyenne")
cities <- data.frame("lon" = city.lon, "lat" = city.lat, "names" = city.names)

ggmap::register_google(key = "AIzaSyCwpdBLfC_5uOD9bKbnI4HYkc-uzowcqdU")

data <- readRDS('../data/methane/ne_co/all.RData')
data <- data[order(data$time),]


#### Begin Analysis

sd.multiplier <- 3
zlim.upper <- mean(data$gas) + sd.multiplier * sd(data$gas)
zlim.lower <- mean(data$gas) - sd.multiplier * sd(data$gas)

lat.bound <- round(range(data$lat))
lon.bound <- round(range(data$lon))

res <- 5
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


proportion.day.greater <- generate.proportion.greater(averaged.data.day)
proportion.week.greater <- generate.proportion.greater(averaged.data.week)
proportion.month.greater <- generate.proportion.greater(averaged.data.month)
proportion.season.greater <- generate.proportion.greater(averaged.data.season)


proportion.day.max <- generate.proportion.max(averaged.data.day)
proportion.week.max <- generate.proportion.max(averaged.data.week)
proportion.month.max <- generate.proportion.max(averaged.data.month)
proportion.season.max <- generate.proportion.max(averaged.data.season)

w.correction <- 0.01
h.correction <- 0.01

wide.lon.bound <- c(lon.bound[1]-w.correction, lon.bound[2]+w.correction)
wide.lat.bound <- c(lat.bound[1]-h.correction, lat.bound[2]+h.correction)

#### Begin plots

h.val <- 1500
w.val <- 1125
r.val <- 150
m.vals <- c(1,2,1,2)


par(mar = m.vals)
png("/home/wdaniels/Desktop/region_sat_map.png", width = w.val, height = h.val, res = r.val)

basemap <- get.basemap('google', 'satellite', wide.lon.bound, wide.lat.bound)
basemap <- basemap + labs(x = "Longitude", y = "Latitude")

while (!is.null(dev.list()))  dev.off()

basemap <- get.basemap.v2(source = "google", type = "satellite", 
                          lon.bounds = wide.lon.bound, lat.bounds = wide.lat.bound)
basemap <- basemap + labs(x = "Longitude", y = "Latitude")

png('/home/wdaniels/Documents/research/tropomi_project/manuscripts/figures/basin.png',
    width = 1000, height = 800, res = 150)
basemap
dev.off()

#plot.data.avg(averaged.data.season)

par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_greater_day.png', sep = ""),
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.day.greater, 
                   "Proportion of Days Greater than Regional Average by Quadrant")
print(g)
dev.off()

par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_greater_week.png', sep = ""),
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.week.greater,
                   "Proportion of Weeks Greater than Regional Average by Quadrant")
print(g)
dev.off()

par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_greater_month.png', sep = ""), 
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.month.greater, 
                   "Proportion of Months Greater than Regional Average by Quadrant")
print(g)
dev.off()

par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_greater_season.png', sep = ""),
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.season.greater,
                   "Proportion of Seasons Greater than Regional Average by Quadrant")
print(g)
dev.off()




par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_max_day.png', sep = ""), 
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.day.max, 
                   "Proportion of Days Containing One of the Top Ten Methane Measurements\nby Quadrant")
print(g)
dev.off()

par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_max_week.png', sep = ""), 
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.week.max, 
                   "Proportion of Days Containing One of the Top Ten Methane Measurements\nby Quadrant")
print(g)
dev.off()

par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_max_month.png', sep = ""), 
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.month.max, 
                   "Proportion of Days Containing One of the Top Ten Methane Measurements\nby Quadrant")
print(g)
dev.off()

par(mar = m.vals)
png(filename = paste('../plots/ne_co_ggplot/prop_max_season.png', sep = ""), 
    width = w.val, height = h.val, res = r.val)
g <- plot.prop.mat(proportion.season.max, 
                   "Proportion of Days Containing One of the Top Ten Methane Measurements\nby Quadrant")
print(g)
dev.off()

