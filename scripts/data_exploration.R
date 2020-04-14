

co.stats <- list("mean" = mean(data$co), "sd" = sd(data$co),
                 "min" = min(data$co), "max" = max(data$co))



png(filename = "/home/wdaniels/Documents/research/tropomi_project/plots/data.png",
    width = 2400, height = 2400, res = 300)

par(mfrow = c(4,4),
    oma = c(4,4,4,4) + 0.1,
    mar = c(2,2,2,2) + 0.1)

for (day.num in 1:14){
  
  quilt.plot(x = data$lon[data$day == day.num],
             y = data$lat[data$day == day.num], 
             z = data$co[data$day == day.num],
             zlim = c(co.stats$mean - 2*co.stats$sd, co.stats$mean + 2*co.stats$sd))
  US( add = T)
  
}
dev.off()



png(filename = "/home/wdaniels/Documents/research/tropomi_project/plots/qa.png",
    width = 3000, height = 2400, res = 300)

par(mfrow = c(4,4),
    oma = c(1,1,1,1) + 0.1,
    mar = c(2,2,2,4) + 0.1)

for (day.num in 1:14){
  
  quilt.plot(x = data$lon[data$day == day.num],
             y = data$lat[data$day == day.num], 
             z = data$qa_value[data$day == day.num],
             zlim = c(0, 1))
  US( add = T)
  
}
dev.off()
