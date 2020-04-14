library(ncdf4)
library(plot3D)
library(fields)

rm(list = ls())

file.path <- "/home/wdaniels/Documents/research/tropomi_project/data/total_column_ozone/2019-09-01_2019-11-01/"

this.nc <- nc_open(paste(file.path, "S5P_OFFL_L2__O3_____20190901T160233_20190901T174402_09765_01_010107_20190907T180205.SUB.nc", sep = ""))

head(this.nc)

gas <- ncvar_get(this.nc, "PRODUCT/ozone_tropospheric_vertical_column")
lat <- ncvar_get(this.nc, "PRODUCT/latitude_ccd")
lon <- ncvar_get(this.nc, "PRODUCT/longitude_ccd")

image2D(z = gas, x = lon, y = lat)
