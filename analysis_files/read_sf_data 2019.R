require(lattice)
require(openair)

rm(list = ls())

myfiles <- list.files("\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\"
                      ,pattern = "Avg"
                      ,recursive = TRUE
                      ,full.names = TRUE)

sfdata <- read.csv(myfiles[1]
                ,header = FALSE
                ,sep = ","
                ,stringsAsFactors = FALSE
                ,colClasses = 'character'
)


sfdata[1:1763592,] <- NA
sfdata$file_source <- NA
sfdata$records <- NA

## Check each file has only 1 date.

running_total <- 0
my_counter <- 0
for (f in myfiles) {
  tf <- read.csv(f
                 ,header = FALSE
                 ,sep = ","
                 ,stringsAsFactors = FALSE
                 ,colClasses = 'character'
  )
  tf$file_source <- f
  records <- nrow(tf)
  tf$records <- records
  sfdata[(my_counter+1):(my_counter+records),] <- tf
  my_counter <- my_counter+records
  print(paste(f, records))
  running_total = running_total+records
}


names(sfdata) <- c('reporting_type', "ring_id","dt","time","vout","co2_ppm","setpoint","x1","x2","x3","wind_speed", "wind_direction","sector","x4","x5", "file_source","records")

sfdata <- sfdata[!is.na(sfdata$reporting_type),]

ring_ids <- data.frame(ring_id = as.character(0:7), ring_number = paste("R",c(9,14,15,16,18,22,26,30), sep = ""), stringsAsFactors = FALSE)
projects <- data.frame(project = c(rep('soy',4),rep('corn',4)),ring_number = paste("R",c(9,14,15,16,18,22,26,30), sep = ""), stringsAsFactors = FALSE)
start_dates <- data.frame(project = c('soy','corn'), start_date = c(as.POSIXct("2019-06-11"), as.POSIXct("2019-06-14")), stringsAsFactors = FALSE)

sfdata <- merge(sfdata, ring_ids, by = "ring_id")
sfdata <- merge(sfdata, projects, by = "ring_number")
sfdata <- merge(sfdata, start_dates, by = "project")

sort(unique(sfdata$dt))

sfdata[sfdata$dt == "6/11/2019",]$dt <- "06/11/2019"
sfdata[sfdata$dt == "6/18/2019",]$dt <- "06/18/2019"
sfdata[sfdata$dt == "6/19/2019",]$dt <- "06/19/2019"
sfdata[sfdata$dt == "6/20/2019",]$dt <- "06/20/2019"
sfdata[sfdata$dt == "6/22/2019",]$dt <- "06/22/2019"
sfdata[sfdata$dt == "6/24/2019",]$dt <- "06/24/2019"
sfdata[sfdata$dt == "6/26/2019",]$dt <- "06/26/2019"
sfdata[sfdata$dt == "7/14/2019",]$dt <- "07/14/2019"

sfdata$datetime <- as.POSIXct(paste(sfdata$dt, sfdata$time),tz = 'GMT', format = "%m/%d/%Y %H:%M:%S")
sfdata$datetime_trunc <- as.POSIXct(paste(sfdata$dt, sfdata$time),tz = 'GMT', format = "%m/%d/%Y %H:%M")

sfdata[is.na(sfdata$datetime),]

sfdata <- sfdata[!is.na(sfdata$datetime) & sfdata$datetime >= sfdata$start_date,]

sfdata$co2_ppm <- as.numeric(sfdata$co2_ppm)
sfdata$wind_speed <- as.numeric(sfdata$wind_speed)
sfdata$wind_direction <- as.numeric(sfdata$wind_direction)
sfdata$vout <- as.numeric(sfdata$vout)
sfdata$sector <- as.numeric(sfdata$sector)
sfdata$ring_id <- as.numeric(sfdata$ring_id)

aggregate(dt ~ file_source, data = sfdata, FUN = unique)

#Add range checks
sfdata[sfdata$vout >10,]
sfdata[sfdata$vout < 0,]
sfdata[sfdata$wind_direction < 0,]
sfdata[sfdata$wind_direction > 360,]

sfdata[sfdata$wind_direction == 750,]$wind_speed <- NA
sfdata[sfdata$wind_direction == 750,]$wind_direction <- NA
sfdata[sfdata$wind_direction == 749,]$wind_speed <- NA
sfdata[sfdata$wind_direction == 749,]$wind_direction <- NA

sfdata <- sfdata[order(sfdata$ring_id, sfdata$datetime),]



str(sfdata)

palette(gray(seq(1,0,len = 1024)))
sfdata$col = cut(sfdata$co2_ppm, breaks = seq(1, 800, length = 1024), labels = FALSE)
sfdata$col = palette()[sfdata$col]

sfdataday <- sfdata[as.POSIXlt(sfdata$datetime)$hour >= 8 & as.POSIXlt(sfdata$datetime)$hour < 17,]

o3data <- sfdata[!sfdata$ring_id %in% c(0,1,2,3),]
co2data <- sfdata[sfdata$ring_id %in% c(0,1,2,3),]

o3dataday <- sfdataday[!sfdataday$ring_id %in% c(0,1,2,3),]
co2dataday <- sfdataday[sfdataday$ring_id %in% c(0,1,2,3),]


by(sfdata,sfdata$ring_id, FUN = function(x)
  by(x, x$dt, FUN = function(y){
    plot(y$datetime, y$co2_ppm, type = 'b'
         ,ylim = c(0,850)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime, as.numeric(y$sector)*100
           ,col = 'purple')
    lines(y$datetime, y$vout*100
          ,col = 'red')
  })
)



by(co2dataday,co2dataday$ring_id, FUN = function(x){
  by(x, x$dt, FUN = function(y){
    plot(y$datetime, y$co2_ppm, type = 'b'
         ,ylim = c(0,1000)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime, as.numeric(y$sector)*100
           ,col = 'purple'
           ,pch = 15)
    lines(y$datetime, y$vout*100
          ,col = 'red')
  })
}
)


by(o3dataday,o3dataday$ring_id, FUN = function(x){
  by(x, x$dt, FUN = function(y){
    plot(y$datetime, y$co2_ppm, type = 'b'
         ,ylim = c(0,200)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime, as.numeric(y$sector)*10
           ,col = 'purple'
           ,pch = 15)
    lines(y$datetime, y$vout*10
          ,col = 'red')
    abline(h = 100, col = 'red', lty = 2)
    abline(h = c(80,120), col = 'red', lty = 4)
  })
}
)

pdf("2019_Co2_Rings_daily.pdf", height = 8.5, width = 11)
by(co2dataday, co2dataday$dt, FUN = function(x){
  par(mfrow=c(4,1), mar =  c(1, 2, 2, 1) + 0.1)
  by(x,x$ring_id, FUN = function(y){
    plot(y$datetime, y$co2_ppm, type = 'b'
         ,ylim = c(0,1000)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime, y$sector*100
           ,col = 'purple'
           ,pch = 15)
    lines(y$datetime, y$vout*100
          ,col = 'red')
    abline(h = 600, col = 'blue', lty = 2)
    abline(h = c(480,720), col = 'blue', lty = 4)
  })
}
)
dev.off()

pdf("2019_o3_Rings_daily.pdf", height = 8.5, width = 11)
by(o3dataday, o3dataday$dt, FUN = function(x){
  par(mfrow=c(4,1), mar =  c(1, 2, 2, 1) + 0.1)
  by(x,x$ring_id, FUN = function(y){
    plot(y$datetime, y$co2_ppm, type = 'b'
         ,ylim = c(0,200)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime, y$sector*10
           ,col = 'purple'
           ,pch = 15)
    lines(y$datetime, y$vout*10
          ,col = 'red')
    abline(h = 100, col = 'red', lty = 2)
    abline(h = c(80,120), col = 'red', lty = 4)
  })
}
)
dev.off()


by(o3dataday, o3dataday$ring_id, FUN = function(x){
  summary(x)
})

temp <- co2dataday[co2dataday$vout > 0,]

by(temp, temp$ring_number, FUN = function(x){
  plot(x$wind_speed, x$vout
       ,main = unique(x$ring_number)
       ,xlim = c(0,15)
       ,ylim = c(0,10))
})

pdf("2019_co2_rings_vout_by_ws_sector_panels.pdf", height = 8.5, width = 11)
par(mfrow = c(2,4), pty = 's',mar =  c(1, 2, 1, 1) + 0.1)
by(temp, temp$ring_number, FUN = function(x){
  by(x,x$sector, FUN = function(x){
    plot(x$wind_speed, x$vout
         ,main = paste(unique(x$ring_number), unique(x$sector))
         ,xlim = c(0,12)
         ,ylim = c(0,10.5))
  }
  )
})
dev.off()


# IS there a way to add the baseline concentration to this?
pdf("2019_co2_rings_vout_by_ws_sector_panels_by_day.pdf", height = 8.5, width = 11)
par(mfrow = c(2,4), pty = 's',mar =  c(1, 2, 1, 1) + 0.1)
by(temp, temp$ring_number, FUN = function(q){
  by(q, q$dt, FUN = function(r){
    par(mfrow=c(1,1))
    par(mfrow=c(2,4))
    for(i in 1:8){
      x = r[r$sector == i,]
      plot.new()
      plot.window(xlim = c(0,12)
                  ,ylim = c(0,10.5))
      axis(1)
      axis(2)
      points(x$wind_speed, x$vout, col = x$col)
      title(main = paste(unique(x$ring_number), unique(x$sector),unique(x$dt)))
      title(xlab = "Wind Speed (m/s)")
      title(ylab = "Vout (V)")
      box()
    }
  })
})
dev.off()

test <- aggregate(vout ~ ring_number + dt, co2data, FUN = sum)

xyplot(test$vout ~ as.Date(test$dt, format = "%m/%d/%Y") | test$ring_number, data = test)
xyplot(test$vout ~ as.Date(test$dt, format = "%m/%d/%Y"), groups = test$ring_number, data = test, type = 'b', auto.key = TRUE)


testo3 <- aggregate(vout ~ ring_number + dt, o3data, FUN = sum)

xyplot(vout ~ as.POSIXct(dt, format = "%m/%d/%Y") | ring_number, data = testo3)
xyplot(vout ~ as.POSIXct(dt, format = "%m/%d/%Y"), groups = ring_number, data = testo3, type = 'b', auto.key = TRUE)


#### Plots using truncated dattimes ####
my_mins = seq(from = min(sfdata$datetime_trunc),  to = max(sfdata$datetime_trunc), by = 'min')

all_mins <- data.frame(datetime_trunc = my_mins, ring_id = rep(unique(sfdata$ring_id), each = length(my_mins)))

sfdataday <- merge(sfdata, all_mins, by = c("ring_id",'datetime_trunc'), all.y= TRUE)

sfdataday$ring_number <- NULL

sfdataday <- merge(sfdataday, ring_ids, by = "ring_id")

sfdataday <- sfdataday[order(sfdataday$ring_id, sfdataday$datetime_trunc),]

sfdataday$dt <- as.character(as.Date(sfdataday$datetime_trunc))

sfdataday <- sfdataday[as.POSIXlt(sfdataday$datetime_trunc)$hour >= 8 & as.POSIXlt(sfdataday$datetime_trunc)$hour < 17,]

o3data <- sfdata[!sfdata$ring_id %in% c(0,1,2,3),]
co2data <- sfdata[sfdata$ring_id %in% c(0,1,2,3),]

o3dataday <- sfdataday[!sfdataday$ring_id %in% c(0,1,2,3),]
co2dataday <- sfdataday[sfdataday$ring_id %in% c(0,1,2,3),]



by(co2dataday,co2dataday$ring_id, FUN = function(x){
  by(x, x$dt, FUN = function(y){
    plot(y$datetime_trunc, y$co2_ppm, type = 'b'
         ,ylim = c(0,1000)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime_trunc, as.numeric(y$sector)*100
           ,col = 'purple')
    lines(y$datetime_trunc, y$vout*100
          ,col = 'red')
  })
}
)


by(o3dataday,o3dataday$ring_id, FUN = function(x){
  by(x, x$dt, FUN = function(y){
    plot(y$datetime_trunc, y$co2_ppm, type = 'b'
         ,ylim = c(0,200)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime_trunc, as.numeric(y$sector)*10
           ,col = 'purple')
    lines(y$datetime_trunc, y$vout*10
          ,col = 'red')
  })
}
)


pdf("2019_Co2_Rings_daily_gaps_showns.pdf", height = 8.5, width = 11)
by(co2dataday, co2dataday$dt, FUN = function(x){
  par(mfrow=c(4,1), mar =  c(1, 2, 2, 1) + 0.1)
  by(x,x$ring_id, FUN = function(y){
    plot(y$datetime_trunc, y$co2_ppm, type = 'b'
         ,ylim = c(0,1000)
         ,main = paste(unique(y$ring_number),unique(y$dt))
         ,cex = .8
    )
    points(y$datetime_trunc, y$wind_speed*100
           ,col = 'purple'
           ,cex = .8)
    lines(y$datetime_trunc, y$vout*100
          ,col = 'red'
          ,type = 'b'
          ,cex = .2)
    abline(h = 600, lty = 2)
  })
}
)
dev.off()

pdf("2019_O3_Rings_daily_gaps_showns.pdf", height = 8.5, width = 11)
by(o3dataday, o3dataday$dt, FUN = function(x){
  par(mfrow=c(4,1), mar =  c(1, 2, 2, 1) + 0.1)
  by(x,x$ring_id, FUN = function(y){
    plot(y$datetime_trunc, y$co2_ppm, type = 'b'
         ,ylim = c(0,200)
         ,main = paste(unique(y$ring_number),unique(y$dt))
    )
    points(y$datetime_trunc, y$sector*10
           ,col = 'purple'
           ,pch = 15)
    lines(y$datetime_trunc, y$vout*10
          ,col = 'red')
    abline(h = 100, col = 'red', lty = 2)
    abline(h = c(80,120), col = 'red', lty = 4)
  })
}
)
dev.off()

by(o3dataday, o3dataday$ring_id, FUN = function(x){
  summary(x)
})

temp <- co2dataday[co2dataday$vout > 0,]

by(temp, temp$ring_number, FUN = function(x){
  plot(x$wind_speed, x$vout
       ,main = unique(x$ring_number))
})


test <- aggregate(vout ~ ring_number + dt, co2data, FUN = sum)

xyplot(test$vout ~ as.Date(test$dt, format = "%m/%d/%Y") | test$ring_number, data = test)
xyplot(test$vout ~ as.Date(test$dt, format = "%m/%d/%Y"), groups = test$ring_number, data = test, type = 'b', auto.key = TRUE)


testo3 <- aggregate(vout ~ ring_number + dt, o3data, FUN = sum)

xyplot(vout ~ as.POSIXct(dt, format = "%m/%d/%Y") | ring_number, data = testo3)
xyplot(vout ~ as.POSIXct(dt, format = "%m/%d/%Y"), groups = ring_number, data = testo3, type = 'b', auto.key = TRUE)


control_stats_co2 <- aggregate(co2_ppm ~ ring_number + dt, co2dataday, FUN = mean, simplify = TRUE)
control_stats_co2 <- merge(control_stats_co2, aggregate(co2_ppm ~ ring_number + dt, co2dataday, FUN = sd), by = c("ring_number", "dt"))

xyplot(co2_ppm.x ~ as.Date(dt) | ring_number, control_stats_co2, type = 'b')
xyplot(co2_ppm.x ~ as.Date(dt), groups = ring_number, control_stats_co2, type = 'b', auto.key = TRUE)
xyplot(co2_ppm.y ~ as.Date(dt), groups = ring_number, control_stats_co2, type = 'b', auto.key = TRUE)

control_stats_o3 <- aggregate(co2_ppm ~ ring_number + dt, o3dataday, FUN = mean, simplify = TRUE)
control_stats_o3 <- merge(control_stats_o3, aggregate(co2_ppm ~ ring_number + dt, o3dataday, FUN = sd), by = c("ring_number", "dt"))

xyplot(co2_ppm.x ~ as.Date(dt) | ring_number, control_stats_o3, type = 'b')
xyplot(co2_ppm.x ~ as.Date(dt), groups = ring_number, control_stats_o3, type = 'b', auto.key = TRUE)
xyplot(co2_ppm.y ~ as.Date(dt), groups = ring_number, control_stats_o3, type = 'b', auto.key = TRUE)
