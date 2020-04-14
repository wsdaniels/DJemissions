rm(list = ls())

library(ggmap)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(usmap)

setwd('/home/wdaniels/Documents/research/tropomi_project/scripts')
source("functions.R")

d <- readRDS(file = "../data/methane/grid_averages/day.RData")
m <- readRDS(file = "../data/methane/grid_averages/month.RData")
s <- readRDS(file = "../data/methane/grid_averages/season.RData")
w <- readRDS(file = "../data/methane/grid_averages/week.RData")

data <- list("d" = d, "m" = m, "s" = s, "w" = w)
rm(d,m,s,w)

res <- 40

temp.res <- 3

ts <- matrix(nrow = length(data[[temp.res]]), ncol = res^2)

for (w in 1:length(data[[temp.res]])){
  for (q in 1:length(data[[temp.res]][[w]]$quadrant)){
    
    row.ind <- w
    col.ind <- data[[temp.res]][[w]]$quadrant[q]+1
    
    ts[row.ind, col.ind] <- data[[temp.res]][[w]]$gas[q]    
    
  }
}

poly.order <- 6
these.fits <- vector(mode = "list", length = res^2)

for (quad.num in 1:res^2){
  
  y <- ts[,quad.num][!is.na(ts[,quad.num])]
  x <- seq(1,length(data[[temp.res]]))[!is.na(ts[,quad.num])]
  df <- data.frame("y" = y, "x" = x)
  these.fits[[quad.num]] <- lm(y ~ poly(x, poly.order, raw = T), data = df)
  
}


xx <- seq(1,length(data[[temp.res]]))
these.max <- vector(length = res^2)
these.min <- vector(length = res^2)
for (i in 1:res^2){
  these.max[i] <- max(predict(these.fits[[i]], data.frame(x=xx)))
  these.min[i] <- min(predict(these.fits[[i]], data.frame(x=xx)))
}

ylim.vals <- c( round(min(these.min)-10), round(max(these.max)+10) )

plot(1, type="n", xlab="", ylab="", xlim=c(0, length(data[[temp.res]])), ylim=c(ylim.vals[1], ylim.vals[2]))

for(i in 1:res^2){
  
  lines(xx, predict(these.fits[[i]], data.frame(x=xx)), col = "blue")
  
}
