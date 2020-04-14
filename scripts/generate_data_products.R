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

lat.bound <- round(range(data$lat))
lon.bound <- round(range(data$lon))

res <- 40
lat.edges <- seq(lat.bound[1], lat.bound[2], length = res + 1)
lon.edges <- seq(lon.bound[1], lon.bound[2], length = res + 1)
lat.centers <- rollmean(lat.edges, 2)
lon.centers <- rollmean(lon.edges, 2)

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

season.list <- generate.season.list(data)

saveRDS(averaged.data.day, file = "../data/methane/grid_averages/day.RData")
saveRDS(averaged.data.month, file = "../data/methane/grid_averages/month.RData")
saveRDS(averaged.data.season, file = "../data/methane/grid_averages/season.RData")
saveRDS(averaged.data.week, file = "../data/methane/grid_averages/week.RData")
