get.basemap <- function(source, type, lonBounds, latBounds) {
  map <- get_map(location = c(mean(lonBounds), mean(latBounds)),
                 source = source, maptype = type, zoom = 6)
  map <- ggmap(map)
  map <- map + scale_x_continuous(limits = lonBounds, expand = c(0, 0))
  map <- map + scale_y_continuous(limits = latBounds, expand = c(0, 0))
  
  return(map)
}

get.basemap.v2 <- function(source, type, lon.bounds, lat.bounds) {
  map.image <- get_map(location = c(mean(lon.bounds), mean(lat.bounds)),
                       source = source, maptype = type, zoom = 6)
  
  map <- ggmap(map.image)
  
  map <- map + geom_sf(data = denver.basin.shp,
                       col = "black",
                       size = 0,
                       inherit.aes = F,
                       mapping = aes(fill = "DJ Basin"),
                       show.legend = "polygon")
  
  map <- map + geom_sf(data = denver.niobara.shp,
                       col = "black",
                       size = 0,
                       inherit.aes = F,
                       mapping = aes(fill = "Niobara Play"),
                       show.legend = "polygon")
  
  map <- map + geom_point(data = bw.data, aes(x = Longitude, y = Latitude, color = "Bayswater Wells"),
                          show.legend = T)
  
  map <- map + scale_fill_manual(values = c("DJ Basin" = alpha("orange", alpha = 0.4),
                                            "Niobara Play" = alpha("red", alpha = 0.4)), 
                                 name = NULL,
                                 guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))
  
  map <- map + scale_colour_manual(values = c("Bayswater Wells" = "black"), 
                                   name = NULL,
                                   guide = guide_legend(override.aes = list(linetype = c("blank"), 
                                                                            shape = c(19))))
  
  map <- map + geom_point(cities,
                          mapping = aes(x = lon, y = lat), 
                          color = "white")
  
  map <- map + geom_text(cities, 
                         mapping = aes(x = lon+0.05, y = lat+0.05, label=names),
                         color = "white",
                         hjust = 1, 
                         vjust = 0)
  map <- map + scale_x_continuous(limits = lon.bounds, expand = c(0, 0))
  map <- map + scale_y_continuous(limits = lat.bounds, expand = c(0, 0))
  return(map)
  
}


get.basemap.v3 <- function(source, type, lon.bounds, lat.bounds) {
  map.image <- get_map(location = c(mean(lon.bounds), mean(lat.bounds)),
                       source = source, maptype = type, zoom = 6)
  
  map <- ggmap(map.image)
  
  map <- map + geom_sf(data = denver.basin.shp,
                       col = "black",
                       size = 0,
                       inherit.aes = F,
                       fill = alpha("orange", alpha = 0.4))
  
  map <- map + geom_sf(data = denver.niobara.shp,
                       col = "black",
                       size = 0,
                       inherit.aes = F,
                       fill = alpha("red", alpha = 0.4))
  
  map <- map + geom_point(data = bw.data, 
                          aes(x = Longitude, y = Latitude),
                          color = "black")
  
  map <- map + geom_point(cities,
                          mapping = aes(x = lon, y = lat), 
                          color = "white")
  
  map <- map + geom_text(cities, 
                         mapping = aes(x = lon+0.05, y = lat+0.05, label=names),
                         color = "white",
                         hjust = 1, 
                         vjust = 0)
  return(map)
  
}



which.part <- function(x, n=30) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}



assign.quadrants <- function(data, lag.edges, lon.edges, res) {
  templat <- as.numeric(as.character(cut(data$lat, lat.edges, labels = res:1, include.lowest = TRUE)))
  templon <- as.numeric(as.character(cut(data$lon, lon.edges, labels = 1:res, include.lowest = TRUE)))
  data$quadrant <- res*(templat - 1) + (templon - 1)
  
  return(data)
}





assign.days <- function(data) {
  return( paste(year(data), '-', yday(data), sep='') )
}


assign.weeks <- function(data) {
  return( paste(year(data), '-', week(data), sep='') )
}


assign.months <- function(data) {
  return( paste(year(data), '-', month(data), sep='') )
}




assign.seasons <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- cut(numeric.date, breaks = c(0,229,0531,0831,1130,1231)) 
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  
  sep.winters <- paste(year(input.date), '-', cuts, sep = "")
  
  return(sep.winters)
}





data.aggregate <- function(X, dateTimes.period, uniqueTimes, type) {
  data.period <- vector(mode = 'list', length = length(uniqueTimes))
  names(data.period) <- paste(type, uniqueTimes, sep = '')
  for (i in 1:length(data.period)) {
    data.period[[i]] <- X[dateTimes.period == uniqueTimes[i], ]
  }
  
  return(data.period)
}





grid.average <- function(data){
  
  averaged.data <- vector(mode = "list", length = length(data))
  
  for (l in 1:length(data)){
    df <- data[[l]]
    period.avg <- mean(df$gas)
    if (nrow(df) > 10) { max.ind <- which.part(df$gas, n = 10) }
    else {max.ind <- 1:nrow(df)}
    unique.quad <- sort(unique(df$quadrant))
    averaged.df <- matrix(ncol = 5, nrow = length(unique.quad))
    
    for (i in 1:length(unique.quad)){
      this.mask <- df$quadrant == unique.quad[i]
      averaged.df[i,1] <- mean(df$gas[this.mask])
      averaged.df[i,2] <- unique.quad[i]
      averaged.df[i,3] <- length(df$gas[this.mask])
      averaged.df[i,4] <- mean(df$gas[this.mask]) - period.avg
      these.max <- vector(length = length(max.ind))
      
      for (j in 1:length(max.ind)){ these.max[j] <- this.mask[j] }
      if (any(these.max)){ averaged.df[i,5] <- T }
      else {averaged.df[i,5] <- F}
    }
    
    
    averaged.df <- as.data.frame(averaged.df)
    names(averaged.df) <- c("gas", "quadrant", "n", "quad.avg.minus.period.avg", 
                            "contains.top.ten")
    averaged.data[[l]] <- averaged.df
  }
  return(averaged.data)
}





generate.quad.map <- function(res){
  quad.vals <- matrix(nrow = res, ncol = res)
  for (y in 1:res){
    for (x in 1:res){
      quad.vals[x, y] <- res*(x - 1) + (y - 1)  
    }
  }
  return(quad.vals)
}


generate.season.list <- function(data){
  seasons <- cbind(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
  colnames(seasons) <- c("Winter", "Spring", "Summer", "Fall")
  
  years <- unique(year(as_datetime(data$time)))
  
  month.list <- vector()
  year.list <- vector()
  
  for (this.year in years){
    these.months <- unique(month(as_datetime(data$time[ year(as_datetime(data$time)) == this.year ])))
    these.years <- rep(this.year, length(these.months))
    month.list <- c(month.list, these.months)
    year.list <- c(year.list, these.years)
  }
  
  season.list <- vector()
  
  for (m in 1:length(month.list)){
    if (month.list[m] %in% seasons[,1]) { season.list[m] <- 1 }
    if (month.list[m] %in% seasons[,2]) { season.list[m] <- 2 }
    if (month.list[m] %in% seasons[,3]) { season.list[m] <- 3 }
    if (month.list[m] %in% seasons[,4]) { season.list[m] <- 4 }
  }
  
  
  
  season.mask <- T
  for (i in 2:length(season.list)){
    if (season.list[i] == season.list[i-1]){
      season.mask[i] <- F
    } else {
      season.mask[i] <- T
    }
  }
  
  unique.seasons <- season.list[season.mask]
  
  main.season.list <- list()
  temp.season <- vector()
  temp.year <- vector()
  temp.month <- vector()
  start.index <- 1
  
  for (i in 1:length(unique.seasons)){
    for (j in start.index:length(season.list)){
      if ( (j == start.index) || (season.list[j] == season.list[j-1]) ){
        
        temp.season <- c(temp.season, season.list[j])
        temp.year <- c(temp.year, year.list[j])
        temp.month <- c(temp.month, month.list[j])
        
      } else {
        start.index <- j
        break
      }
    }
    
    main.season.list[[i]] <- data.frame("seasons" = temp.season,
                                        "years"   = temp.year,
                                        "months"  = temp.month)
    temp.season <- vector()
    temp.year <- vector()
    temp.month <- vector()
  }
  
  return(main.season.list)
  
}


generate.proportion.greater <- function(avereaged.list){
  
  quad.greater <- vector(mode = "numeric", length = res^2)
  for (s in 1:length(avereaged.list)){
    
    df <- avereaged.list[[s]]
    
    for (i in 1:nrow(df)){
      if (df$quad.avg.minus.period.avg[i] > 0){
        quad.greater[df$quadrant[i] + 1] <- quad.greater[df$quadrant[i] + 1] + 1
      }
    }
    
  }
  quad.greater <- quad.greater / length(avereaged.list)
  return(quad.greater)
  
}



generate.proportion.max <- function(avereaged.list){
  
  quad.max <- vector(mode = "numeric", length = res^2)
  for (s in 1:length(avereaged.list)){
    
    df <- avereaged.list[[s]]
    
    for (i in 1:nrow(df)){
      if (df$contains.top.ten[i] == 1){
        quad.max[df$quadrant[i] + 1] <- quad.max[df$quadrant[i] + 1] + 1
      }
    }
    
  }
  quad.max <- quad.max / length(avereaged.list)
  return(quad.max)
  
}


plot.prop.mat <- function(prop.vec, plot.title){
  prop.matrix <- matrix(prop.vec, nrow = res, byrow = T)
  lon.key <- matrix(rep(lon.centers, res), nrow = res, byrow = T)
  lat.key <- matrix(rep(rev(lat.centers), res), nrow = res, byrow = F)
  
  df <- data.frame("prop" = as.vector(prop.matrix), 
                   "lon" = as.vector(lon.key),
                   "lat" = as.vector(lat.key))
  
  g <- basemap
  g <- g + geom_tile(data = df, height = grid.h, width = grid.w, alpha = 0.7,
                     aes(x = lon, y = lat, fill = prop) )
  g <- g + scale_fill_gradientn(colors = hcl.colors(16, "YlOrRd", rev = TRUE),
                                limits = c(0, 1))
  g <- g + ggtitle(plot.title) + xlab('Longitude') + ylab('Latitude')
  g <- g + theme(legend.key.width = unit(2.75, 'cm'))
  g <- g + theme(legend.position = 'bottom')
  g <- g + guides(fill = guide_colorbar(title = "Bias Correct Methane Mixing Ratio [Mole Fraction, 1e-9]",
                                        title.position = 'top'))
  g <- g + geom_point(cities, mapping = aes(x = lon, y = lat))
  g <- g + geom_text(cities, mapping = aes(x = lon+0.05, y = lat+0.05, label=names),hjust=0, vjust=0)
  return(g)
}



plot.data.avg <- function(averaged.list){
  
  for (l in 1:length(averaged.list)){
    
    df <- averaged.list[[l]]
    
    lon.quad <- vector(length = nrow(df))
    lat.quad <- vector(length = nrow(df))
    for (i in 1:nrow(df)){
      these.coords <- which(quad.map == df$quadrant[i], arr.ind = T)
      lon.quad[i] <- these.coords[2]
      lat.quad[i] <- res-these.coords[1]+1
    }
    
    lon.quad <- lon.centers[lon.quad]
    lat.quad <- lat.centers[lat.quad]
    
    df <- cbind(df, lat.quad, lon.quad)
    
    g <- basemap
    g <- g + geom_tile(data = df, height = grid.h, width = grid.w, alpha = 0.7,
                       aes(x = lon.quad, y = lat.quad, fill = gas) )
    g <- g + scale_fill_gradientn(colors = hcl.colors(16, "Spectral", rev = TRUE),
                                  limits = c(zlim.lower, zlim.upper))
    
    g <- g + ggtitle("Average Methane by Quadrant") + xlab('Longitude') + ylab('Latitude')
    g <- g + theme(legend.key.width = unit(2.75, 'cm'))
    g <- g + theme(legend.position = 'bottom')
    g <- g + guides(fill = guide_colorbar(title = "Mean Methane", title.position = 'top'))
    g <- g + geom_point(cities, mapping = aes(x = lon, y = lat))
    g <- g + geom_text(cities, mapping = aes(x = lon+0.05, y = lat+0.05, label=names),hjust=0, vjust=0)
    
    png(filename = paste('../plots/ne_co_ggplot/', l, '.png', sep = ""),
        width = 2800, height = 4000, res = 300)
    print(g)
    dev.off()
    
  }
}



