rm(list = ls())

data.product <- "total_column_ozone"

data.location <- paste("/home/wdaniels/Documents/research/tropomi_project/data/",
                       data.product,
                       "/2019-09-01_2019-11-01/data_advanced_qa.RData",
                       sep = "")

data <- readRDS(file = data.location)

n <- nrow(data)

data.min <- min(data$gas)
data.max <- max(data$gas)
data.mean <- mean(data$gas)
data.sd <- sd(data$gas)

png(filename = paste("/home/wdaniels/Documents/research/tropomi_project/plots/", data.product, "/dists.png", sep = ""),
    width = 3400, height = 2400, res = 300)

par(mfrow = c(1,2),
    oma = c(0,0,3,0))

lower.bound <- c(data.min, data.mean-4*data.sd)
upper.bound <- c(data.max, data.mean+4*data.sd)

to.keep <- (data$gas >= 0 & data$gas <= 0.45) &
  (data$ozone_effective_temperature >= 180 & data$ozone_effective_temperature <= 260) &
  (data$ring_scale_factor >= 0 & data$ring_scale_factor <= 0.15) &
  (data$effective_albedo >= -0.5 & data$effective_albedo <= 1.5)

n.to.keep <- sum(to.keep, na.rm = T)
n.to.exclude <- n - n.to.keep

boxplot(data$gas[!to.keep],
        col = "orange",
        ylim = c(data.min,data.max),
        xaxt = 'n')

mtext(paste("Low Quality Ozone Data\nNumber of Observations = ", 
            format(n.to.exclude,big.mark=",",scientific=FALSE),
            " (",
            round(n.to.exclude/n, digits = 2),
            "n)",
            sep = ""),
      side = 3, line = 1)
mtext(side = 2, line = 3, "Ozone Total Column [mol m^{-2}]", font = 2, cex = 1.1)

boxplot(data$gas[to.keep],
        col = "orange",
        ylim = c(data.min, data.max),
        xaxt = 'na')

mtext(paste("High Quality Ozone Data\nNumber of Observations = ", 
            format(n.to.keep,big.mark=",",scientific=FALSE),
            " (",
            round(n.to.keep/n, digits = 2),
            "n)",
            sep = ""),
      side = 3, line = 1)
mtext(side = 2, line = 3, "Ozone Total Column [mol m^{-2}]", font = 2, cex = 1.1)



mtext(paste("Total Column Ozone Data\nn = ", format(n,big.mark=",",scientific=FALSE), " Data Points", sep = ""), outer = T, line = 0, font = 2, cex = 1.2)


dev.off()