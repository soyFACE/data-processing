# SoyFACE Data Processing Functions

raw_sfdata_avg_to_dataframe <- function(source_file_location){
  
  source_file_location <- "\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\"
  
  myfiles <- list.files(source_file_location
                        ,pattern = "Avg"
                        ,recursive = TRUE
                        ,full.names = TRUE)
  
  sfdata <- read.csv(myfiles[1]
                     ,header = FALSE
                     ,sep = ","
                     ,stringsAsFactors = FALSE
                     ,colClasses = 'character'
  )
  
  record_number_estimate <- length(myfiles)*1440*1.1 # Estimate how big the total data frame will need to be based on how many files will be read, times the minutes in a day with 10% leeway.
  
  sfdata[1:record_number_estimate,] <- NA
  sfdata$file_source <- NA
  sfdata$number_of_records <- NA
  
  running_total <- 0
  my_counter <- 0
  for (f in myfiles[1:100]) {
    tf <- read.csv(f
                   ,header = FALSE
                   ,sep = ","
                   ,stringsAsFactors = FALSE
                   ,colClasses = 'character'
    )
    tf$file_source <- f
    number_of_records <- nrow(tf)
    tf$number_of_records <- number_of_records
    sfdata[(my_counter+1):(my_counter+number_of_records),] <- tf
    my_counter <- my_counter+number_of_records
    print(paste(f, number_of_records))
    running_total = running_total+number_of_records
  }
  
  sfdata_header <- as.character(read.csv("../metadata/minute_average_header_for_r.csv"
                                         ,header = FALSE
                                         ,sep = ","
                                         ,stringsAsFactors = FALSE
                                         ,colClasses = 'character')[1,])
  
  
  names(sfdata) <- sfdata_header
  sfdata <- sfdata[!is.na(sfdata$reporting_type),]
  sfdata$datetime <- as.POSIXct(paste(sfdata$dt, sfdata$time)
                                ,tz = 'GMT'
                                ,format = "%m/%d/%Y %H:%M:%S")
  sfdata$datetime_trunc <- as.POSIXct(paste(sfdata$dt, sfdata$time)
                                      ,tz = 'GMT'
                                      ,format = "%m/%d/%Y %H:%M")
  sfdata$year <- as.character(as.POSIXlt(sfdata$datetime)$year+1900)
  
}



read_sfdata_metadata <- function(){
  ring_ids <- read.csv("../metadata/ring_ids.csv"
                       ,stringsAsFactors = FALSE
                       ,colClasses = 'character'
  )
  
  projects <- read.csv("../metadata/projects.csv"
                       ,stringsAsFactors = FALSE
                       ,colClasses = 'character'
  )
  start_dates <- read.csv("../metadata/start_dates.csv"
                          ,stringsAsFactors = FALSE
                          ,colClasses = 'character'
  )
  end_dates <- read.csv("../metadata/end_dates.csv"
                        ,stringsAsFactors = FALSE
                        ,colClasses = 'character'
  )
  fumigation_type <- read.csv("../metadata/fumigation_type.csv"
                              ,stringsAsFactors = FALSE
                              ,colClasses = 'character'
  )
}

add_sfdata_metadata <- function(sfdata){
  sfdatat1 <- merge(sfdata, ring_ids, by = c("ring_id", "year"))
  sfdatat2 <- merge(sfdatat1, projects, by = c("ring_number", "year"))
  sfdatat3 <- merge(sfdatat2, start_dates, by = c("project","year"))
  sfdatat4 <- merge(sfdatat3, fumigation_type, by = c("ring_number","year"))
  sfdatat5 <- merge(sfdatat4, end_dates, by = c("fumigation_type","year"))
}

check_ranges <- funciton(my_sfdata,column_name){
  temp_col <- "wind_speed"
  my_sfdata <- sfdatat5
  interval_file <- read.csv("../metadata/valid_ranges.csv"
                             ,stringsAsFactors = FALSE
                             ,colClasses = c('character')
  )
  
  range_type <- interval_file[interval_file$variable == temp_col,"type"]
  
  lower_limit <- interval_file[interval_file$variable == temp_col,"lower_limit"]
  upper_limit <- interval_file[interval_file$variable == temp_col,"upper_limit"]
  
  lower_limit <- as(lower_limit,range_type)
  upper_limit <- as(upper_limit,range_type)
  
  my_sfdata[temp_col] <- as.numeric(unlist(my_sfdata[temp_col]))
  
  
  outlier <- my_sfdata[which(my_sfdata[temp_col] < lower_limit),]
  outlier2 <- my_sfdata[which(my_sfdata[temp_col] > 12),]
  
  result = outlier
}




check_sfdata_ranges <- function(sfdata){
  View(sfdata[sfdata$layer_1_vout >10,])
  View(sfdata[sfdata$layer_1_vout < 0,])
  View(sfdata[sfdata$wind_direction < 0,])
  View(sfdata[sfdata$wind_direction > 360,])
  
  sfdata[sfdata$layer_1_concentration < 0 | is.na(sfdata$layer_1_concentration),]$layer_1_concentration <- NA
  sfdata[sfdata$wind_direction >= 750,]$wind_speed <- NA
  sfdata[sfdata$wind_direction >= 750,]$wind_direction <- NA
  sfdata[sfdata$wind_direction == 749,]$wind_speed <- NA
  sfdata[sfdata$wind_direction == 749,]$wind_direction <- NA
  
}



convert_sfdata_variable_types <- function(sfdata){
  sort(unique(sfdata$dt))
  
  sfdata$datetime <- as.POSIXct(paste(sfdata$dt, sfdata$time),tz = 'GMT', format = "%m/%d/%Y %H:%M:%S")
  sfdata$datetime_trunc <- as.POSIXct(paste(sfdata$dt, sfdata$time),tz = 'GMT', format = "%m/%d/%Y %H:%M")
  sfdata[is.na(sfdata$datetime),]
  
  sfdata <- sfdata[!is.na(sfdata$datetime) & sfdata$datetime >= sfdata$start_date,]
  
  sfdata$layer_1_concentration <- as.numeric(sfdata$layer_1_concentration)
  sfdata$wind_speed <- as.numeric(sfdata$wind_speed)
  sfdata$wind_direction <- as.numeric(sfdata$wind_direction)
  sfdata$layer_1_vout <- as.numeric(sfdata$layer_1_vout)
  sfdata$sector <- as.numeric(sfdata$sector)
  sfdata$ring_id <- as.numeric(sfdata$ring_id)
  sfdata$layer_1_setpoint <- as.numeric(sfdata$layer_1_setpoint)
}

check_sfdata_dates <- function(sfdata){
  View(aggregate(dt ~ file_source, data = sfdata, FUN = unique))
}

average_sf_data

investigate_statistical_properties

calc_sf_data_time_within_target

make_face_stats

#plotting functions

