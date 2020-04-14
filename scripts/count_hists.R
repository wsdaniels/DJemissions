dir <- '/home/wdaniels/Documents/research/tropomi_project/data/methane/grid_averages'
setwd(dir)

times <- c("day", "week", "month", "season")
titles <- c("Daily", "Weekly", "Monthly", "Seasonal")

par(mar = c(5,3,3,1))
par(mfrow = c(2,2))

for(i in 1:length(times)){
  
  data <- readRDS(paste(times[i], '.RData', sep = ""))
  
  n.vals <- vector()
  
  for (j in 1:length(data)){
    n.vals <- c(n.vals, data[[j]]$n)
  }
  
  hist(x = n.vals, 
       breaks=seq(min(n.vals)-0.5, max(n.vals)+0.5, by=1),
       freq = F,
       xlab = "Number of Observations per Quadrant",
       main = paste(titles[i], " Averages", sep = ""))
  
}