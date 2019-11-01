# Make summary stats for SoyFACE fumigation
# Average treatment level
# Percent time within 10%
# Percent time within 20%

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


names(sfdata) <- c('reporting_type', "ring_id","dt","time","vout","co2_ppm","set_point","x1","x2","x3","wind_speed", "wind_direction","sector","x4","x5", "file_source","records")

sfdata <- sfdata[!is.na(sfdata$reporting_type),]

ring_ids <- data.frame(ring_id = as.character(0:7), ring_number = paste("R",c("09","14","15","16","18","22","26","30"), sep = ""), stringsAsFactors = FALSE)
projects <- data.frame(project = c(rep('soy',4),rep('corn',4)),ring_number = paste("R",c("09","14","15","16","18","22","26","30"), sep = ""), stringsAsFactors = FALSE)
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

save(sfdata, file = "\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\processed_r_data\\sfdata.Rdata")


if(!exists("sfdata")){
  rm(list = ls())
  load(file = "\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\processed_r_data\\sfdata.Rdata")
}

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
sfdata$set_point <- as.numeric(sfdata$set_point)

View(aggregate(dt ~ file_source, data = sfdata, FUN = unique))

#Add range checks
View(sfdata[sfdata$vout >10,])
View(sfdata[sfdata$vout < 0,])
View(sfdata[sfdata$wind_direction < 0,])
View(sfdata[sfdata$wind_direction > 360,])

sfdata[sfdata$co2_ppm < 0 | is.na(sfdata$co2_ppm),]$co2_ppm <- NA

sfdata[sfdata$wind_direction >= 750,]$wind_speed <- NA
sfdata[sfdata$wind_direction >= 750,]$wind_direction <- NA
sfdata[sfdata$wind_direction == 749,]$wind_speed <- NA
sfdata[sfdata$wind_direction == 749,]$wind_direction <- NA

sfdata$within20 <- sfdata$co2_ppm >= (100-20)*.01*sfdata$set_point & sfdata$co2_ppm <= (100+20)*.01*sfdata$set_point
sfdata$within10 <- sfdata$co2_ppm >= (100-10)*.01*sfdata$set_point & sfdata$co2_ppm <= (100+10)*.01*sfdata$set_point

sfdata$hour <- as.POSIXlt(sfdata$datetime)$hour
sfdata$date <- sfdata$datetime
sfdata$ws <- sfdata$wind_speed
sfdata$wd <- sfdata$wind_direction
sfdata$site <- sfdata$ring_number

sfdata <- sfdata[order(sfdata$ring_id, sfdata$datetime),]

sfdata_hourly_openair <- timeAverage(sfdata
                                     ,avg.time = 'hour'
                                     ,data.thresh = 0
                                     ,statistic = 'mean'
                                     ,vector.ws = TRUE
                                     ,type = 'site')


sfdata_hourly_openair <- as.data.frame(sfdata_hourly_openair)
sfdata_hourly_openair$ring_number <- sfdata_hourly_openair$site

sfdata_hourly_openair <- merge(sfdata_hourly_openair, projects, by = "ring_number")
sfdata_hourly_openair <- merge(sfdata_hourly_openair, start_dates, by = "project")


sf_fum_on <- sfdata[sfdata$vout > 0,]
sf_fum_should_be_on <- sfdata[(sfdata$project == "soy" & sfdata$hour >= 8 & sfdata$hour <= 17) | (sfdata$project == "corn" & sfdata$hour >= 9 & sfdata$hour <= 17),]

sf_fum_on_hourly <- sfdata_hourly_openair[sfdata_hourly_openair$vout > 0,]
sf_fum_should_be_on_hourly <- sfdata_hourly_openair[(sfdata_hourly_openair$project == "soy" & sfdata_hourly_openair$hour >= 8 & sfdata_hourly_openair$hour <= 17) | (sfdata_hourly_openair$project == "corn" & sfdata_hourly_openair$hour >= 9 & sfdata_hourly_openair$hour <= 17),]




time_within_target <- function(x,set_point, target_percent){
  percent_low <- (100-target_percent)*.01*set_point
  percent_high <- (100+target_percent)*.01*set_point
  within_target <- x[x >= percent_low & x <= percent_high]
  length(within_target)/length(x)*100
}

make_face_stats <- function(face_data, set_point){
  
  d1 <- aggregate(co2_ppm ~ ring_number + project, data = face_data, FUN = mean)
  d2 <- merge(d1, aggregate(co2_ppm ~ ring_number + project, data = face_data, FUN = time_within_target,set_point = set_point, target_percent = 20)
              ,by = c("ring_number","project")
              ,all = TRUE)
  d3 <- merge(d2, aggregate(co2_ppm ~ ring_number + project, data = face_data, FUN = time_within_target,set_point = set_point, target_percent = 10)
              ,by = c("ring_number","project")
              ,all = TRUE)
  d4 <- merge(d3, aggregate(vout ~ ring_number + project, data = face_data, FUN = function(x){
    sum(x > 0, na.rm = TRUE)/length(x)*100
  })
              ,by = c("ring_number","project")
              ,all = TRUE)
  
  names(d4) <- c("ring_number", "project", "average", "within_20%",  "within_10%", "uptime")
  d4
}

#### Minutely Stats ####
### When fumigation is on ###

aggregate(co2_ppm ~ ring_number, data = sf_fum_on, FUN = mean)
aggregate(co2_ppm ~ ring_number, data = sfdata, FUN = mean)

d1 <- aggregate(co2_ppm ~ ring_number + project, data = sf_fum_on[sf_fum_on$project == "soy",], FUN = mean)
d2 <- merge(d1, aggregate(co2_ppm ~ ring_number + project, data = sf_fum_on[sf_fum_on$project == "soy",], FUN = time_within_target,set_point = 600, target_percent = 20)
            ,by = c("ring_number","project")
            ,all = TRUE)
d3 <- merge(d2, aggregate(co2_ppm ~ ring_number + project, data = sf_fum_on[sf_fum_on$project == "soy",], FUN = time_within_target,set_point = 600, target_percent = 10)
            ,by = c("ring_number","project")
            ,all = TRUE)

names(d3) <- c("ring_number", "project", "average", "within_20%",  "within_10%")

d4 <- aggregate(co2_ppm ~ ring_number + project, data = sf_fum_on[sf_fum_on$project == "corn",], FUN = mean)
d5 <- merge(d4, aggregate(co2_ppm ~ ring_number + project, data = sf_fum_on[sf_fum_on$project == "corn",], FUN = time_within_target,set_point = 100, target_percent = 20)
            ,by = c("ring_number","project")
            ,all = TRUE)
d6 <- merge(d5, aggregate(co2_ppm ~ ring_number + project, data = sf_fum_on[sf_fum_on$project == "corn",], FUN = time_within_target,set_point = 100, target_percent = 10)
            ,by = c("ring_number","project")
            ,all = TRUE)

names(d6) <- names(d3) <- c("ring_number", "project", "average", "within_20%",  "within_10%")

summary_stats_fumigation <- rbind(d3,d6)
rm(d1,d2,d3,d4,d5,d6)


d1 <- aggregate(co2_ppm ~ ring_number + dt  + project, data = sf_fum_on[sf_fum_on$project == "soy",], FUN = mean)
d2 <- merge(d1,aggregate(co2_ppm ~ ring_number + dt  + project, data = sf_fum_on[sf_fum_on$project == "soy",], FUN = time_within_target,set_point = 600, target_percent = 20)
            ,by = c("ring_number","project", "dt")
            ,all = TRUE)
d3 <- merge(d2,aggregate(co2_ppm ~ ring_number + dt  + project, data = sf_fum_on[sf_fum_on$project == "soy",], FUN = time_within_target,set_point = 600, target_percent = 10)
            ,by = c("ring_number","project","dt")
            ,all = TRUE)

names(d3) <- c("ring_number", "project", "dt", "average", "within_20%",  "within_10%")



d4 <- aggregate(co2_ppm ~ ring_number + dt  + project, data = sf_fum_on[sf_fum_on$project == "corn",], FUN = mean)
d5 <- merge(d4,aggregate(co2_ppm ~ ring_number + dt  + project, data = sf_fum_on[sf_fum_on$project == "corn",], FUN = time_within_target,set_point = 100, target_percent = 20)
            ,by = c("ring_number","project", "dt")
            ,all = TRUE)
d6 <- merge(d5,aggregate(co2_ppm ~ ring_number + dt  + project, data = sf_fum_on[sf_fum_on$project == "corn",], FUN = time_within_target,set_point = 100, target_percent = 10)
            ,by = c("ring_number","project","dt")
            ,all = TRUE)

names(d6) <- c("ring_number", "project", "dt", "average", "within_20%",  "within_10%")

summary_stats_daily_fumigation <- rbind(d3,d6)
rm(d1,d2,d3,d4,d5,d6)

summary_stats_daily_fumigation$datetime <- as.POSIXct(summary_stats_daily_fumigation$dt,tz = 'GMT', format = "%m/%d/%Y")

xyplot(average ~ datetime | project, groups = ring_number, data = summary_stats_daily_fumigation[summary_stats_daily_fumigation$project == "soy",]
       ,type = 'l'
       ,auto.key = TRUE)


xyplot(average ~ datetime | project, groups = ring_number, data = summary_stats_daily_fumigation[summary_stats_daily_fumigation$project == "corn",]
       ,type = 'l'
       ,auto.key = TRUE)



xyplot(average ~ datetime | project, groups = ring_number, data = summary_stats_daily_fumigation[summary_stats_daily_fumigation$project == "soy",]
       ,type = 'l'
       ,auto.key = TRUE
       ,ylim = c(550,650)
       )


xyplot(average ~ datetime | project, groups = ring_number, data = summary_stats_daily_fumigation[summary_stats_daily_fumigation$project == "corn",]
       ,type = 'l'
       ,auto.key = TRUE
       ,ylim = c(70,110)
       )


xyplot(`within_10%` + `within_20%` ~ datetime | project, groups = ring_number, data = summary_stats_daily_fumigation[summary_stats_daily_fumigation$project == "soy",]
       ,type = 'l'
       ,auto.key = TRUE
)

xyplot(`within_10%` + `within_20%` ~ datetime | project, groups = ring_number, data = summary_stats_daily_fumigation[summary_stats_daily_fumigation$project == "corn",]
       ,type = 'l'
       ,auto.key = TRUE
)

### For the entire time period ###

d3 <- make_face_stats(sf_fum_should_be_on[sf_fum_should_be_on$project == "soy",], set_point = 600)
d6 <- make_face_stats(sf_fum_should_be_on[sf_fum_should_be_on$project == "corn",], set_point = 100)

summary_stats_fumigation_should_be_on <- rbind(d3,d6)

#### Hourly Stats ####


d3 <- make_face_stats(sf_fum_on_hourly[sf_fum_on_hourly$project == "soy",], set_point = 600)
d6 <- make_face_stats(sf_fum_on_hourly[sf_fum_on_hourly$project == "corn",], set_point = 100)

summary_stats_hourly_fum_on <- rbind(d3,d6)


d3 <- make_face_stats(sf_fum_should_be_on_hourly[sf_fum_should_be_on_hourly$project == "soy",], set_point = 600)
d6 <- make_face_stats(sf_fum_should_be_on_hourly[sf_fum_should_be_on_hourly$project == "corn",], set_point = 100)

summary_stats_hourly_fum_should_be_on <- rbind(d3,d6)
