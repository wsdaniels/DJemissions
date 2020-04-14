rm(list = ls())

data.product <- "total_column_ozone"

data.location <- paste("/home/wdaniels/Documents/research/tropomi_project/data/",
                       data.product,
                       "/2019-09-01_2019-11-01/data_advanced_qa.RData",
                       sep = "")

data <- readRDS(file = data.location)

qa.vals <- sort(unique(data$qa_value))

num.vals <- length(qa.vals)

data.min <- min(data$gas)
data.max <- max(data$gas)
data.mean <- mean(data$gas)
data.sd <- sd(data$gas)

png(filename = paste("/home/wdaniels/Documents/research/tropomi_project/plots/", data.product, "/qa_dist.png", sep = ""),
    width = 3400, height = 2400, res = 300)

par(mfrow = c(1,2),
    oma = c(0,0,3,0))

lower.bound <- c(data.min, data.mean-4*data.sd)
upper.bound <- c(data.max, data.mean+4*data.sd)

for (i in 1:2){
  
  boxplot.data <- boxplot(data$gas ~ data$qa,
                          col = "orange",
                          ylim = c(lower.bound[i],upper.bound[i]),
                          xaxt = 'na')
  if (i == 1){
    if (data.product == "methane"){
      mtext(side = 2, line = 3, "Methane Mixing Ratio Bias Corrected [1e-9]", font = 2, cex = 1.1)
    } else if (data.product == "carbon_monoxide"){
      mtext(side = 2, line = 3, "CO Total Column [mol m^{-2}]", font = 2, cex = 1.1)
    } else if (data.product == "total_column_ozone"){
      mtext(side = 2, line = 3, "Ozone Total Column [mol m^{-2}]", font = 2, cex = 1.1)
    }
  }
  
  axis(side = 1, at = 1:num.vals, labels = round(qa.vals, digits = 2))
  mtext("QA Value", side = 1, line = 2)
  
  num.data <- boxplot.data$n
  ratio.data <- boxplot.data$n / sum(boxplot.data$n)
  
  axis(side = 3, at = 1:num.vals, labels = paste(round(100*ratio.data)/100, " n", sep = ""))
  
  if (i ==1){
    mtext("Full Range\nNumber of Observations", side = 3, line = 2)
  } else {
    mtext("Zoomed In\nNumber of Observations", side = 3, line = 2)
  }
}


if (data.product == "methane"){
  mtext(paste("Methane Data\nn = ", format(nrow(data),big.mark=",",scientific=FALSE), " Data Points", sep = ""), outer = T, line = 0, font = 2, cex = 1.2)
} else if (data.product == "carbon_monoxide"){
  mtext(paste("Carbon Monoxide Data\nn = ", format(nrow(data),big.mark=",",scientific=FALSE), " Data Points", sep = ""), outer = T, line = 0, font = 2, cex = 1.2)
} else if (data.product == "total_column_ozone"){
  mtext(paste("Total Column Ozone Data\nn = ", format(nrow(data),big.mark=",",scientific=FALSE), " Data Points", sep = ""), outer = T, line = 0, font = 2, cex = 1.2)
}

dev.off()