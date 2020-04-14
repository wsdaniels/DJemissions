rm(list = ls())

library(foreign)
library(readxl)
library(sf)
library(ggplot2)

dbf.data <- read.dbf('/home/wdaniels/Downloads/Wells.dbf')


read.excel.allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


csv.data <- read.excel.allsheets('/home/wdaniels/Downloads/dailyActivityDashboard.xlsx')

well.status <- csv.data$Well_Status


sed.basins <- read.dbf('/home/wdaniels/Downloads/SedimentaryBasins_US_May2011_v2.dbf')

sed.basin.shp <- st_read('/home/wdaniels/Downloads/SedimentaryBasins_US_May2011_v2.shp')

denver <- sed.basin.shp[22,]

oil.plays <- st_read('/home/wdaniels/Downloads/ShalePlays_US_EIA_Sep2019.shp')

denver.niobara <- oil.plays[41,]

ggplot() + 
  geom_sf(data = denver, size = 1, color = "black", fill =  alpha("orange", alpha = 0.5) )+
  geom_sf(data = denver.niobara, size = 1, color = "black", fill = alpha("red", alpha = 0.75)) + 
  ggtitle("Denver Julesburg Basin and Niobara Gas Play") + 
  coord_sf() +
  geom_point(data = bw.data, aes(x = Longitude, y = Latitude))
