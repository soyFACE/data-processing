# SoyFACE Data Processing Functions

############################load data###############################
load("../processed_r_data/sfdata_unchecked.rdata")
load("../processed_r_data/sfdata.Rdata")
valid_range <- read.csv("../metadata/valid_ranges.csv"
                        ,stringsAsFactors = FALSE
                        ,colClasses = c('character'))

sfdata_unchecked$layer_2_concentration <-  0
sfdata_unchecked$layer_2_vout <-  0
sfdata_unchecked$layer_2_setpoint <-  0


test_data <- sfdata_unchecked
test_data[2,]$wind_speed <- "error"
test_data[4,]$layer_1_concentration <- "error"

out_of_range_data <- sfdata
out_of_range_data = na.omit(out_of_range_data)
###################################################################

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
  for (f in myfiles) {
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
}

check_sfdata_types <- function(unchecked_df){
  unchecked_df <- test_data
  error_row <- data.frame(cbind(unchecked_df, flag = "text")) 
  error_row <- error_row[0,]
  
  for(i in names(unchecked_df)){
    my_type <- valid_range[valid_range$variable == i,"type"]
    error_row_i <- check_types_convertible(i,my_type,unchecked_df)
    
    if(nrow(error_row_i)!= 0){
      error_row_i$flag <- i
      error_row = rbind(error_row,error_row_i)
    }
  }
  
  return(error_row)
}

check_types_convertible <- function(columname, my_type,unchecked_df){
  converted_column = lapply(unchecked_df[columname], function(x) as(x,my_type))
  converted_column = data.frame(unlist(converted_column))
  converted_data = unchecked_df
  converted_data[columname] = converted_column[,1]
  error_row_by_column <- converted_data[is.na(converted_data[columname]),]
  return(error_row_by_column)
}


check_sfdata_range <- function(my_df){
  my_df <- out_of_range_data
  out_of_range_row <- data.frame(cbind(my_df, type_flag = "text")) 
  out_of_range_row <- out_of_range_row[0,]
  
  for(i in names(my_df)){
    
    out_of_range_row_i <- check_ranges(i, my_df)
    
    if(nrow(out_of_range_row_i)!= 0){
      out_of_range_row = rbind(out_of_range_row,out_of_range_row_i)
    }
  }
  
  return(out_of_range_row)
}



check_ranges <- function(column_name,my_sfdata){
  outlier <- data.frame(cbind(my_sfdata, Range_flag = "text")) 
  outlier <- outlier[0,]
  
  range_type <- valid_range[valid_range$variable == column_name,"type"]
  
  empty_return <- data.frame(cbind(my_sfdata, Range_flag = "TEXT"))
  empty_return <- empty_return[0,]
  
  if(range_type != "numeric") return(empty_return)
  
  lower_limit <- valid_range[valid_range$variable == column_name,"lower_limit"]
  upper_limit <- valid_range[valid_range$variable == column_name,"upper_limit"]
  
  lower_limit <- as(lower_limit,range_type)
  upper_limit <- as(upper_limit,range_type)
  
  
  converted_column <- lapply(my_sfdata[column_name], function(x) as(x,range_type))
  converted_column <- data.frame(unlist(converted_column))
  
  my_sfdata[column_name] <- converted_column
  
  
  outlier1 <- my_sfdata[which(my_sfdata[column_name] < lower_limit),]
  outlier2 <- my_sfdata[which(my_sfdata[column_name] > upper_limit),]
  
  outlier <- rbind(outlier1, outlier2)
  if(nrow(outlier)!=0){
    outlier$Range_flag = column_name
  }
  
  return(outlier)
  
}


make_error_template <- function(error_df, original_df, default_placeholder){
  
  # dummy data
  sfdata_unchecked_errors <- sfdata_unchecked[seq(1,76, by = 10),]
  sfdata_unchecked_errors$original_df_row <- seq(1,76, by = 10)
  sfdata_unchecked_errors$bad_var_name <- "layer_1_concentration"
  
  error_df <- sfdata_unchecked_errors
  original_df <- sfdata_unchecked
  default_placeholder <- "NA"
  # end dummy data
  
  
  original_df_name <- deparse(substitute(original_df)) # this might not do what I want inside a function
  bad_variable_vector <- error_df$bad_var_name
  bad_row_vector <- error_df$original_df_row
  
  writeClipboard(paste(original_df_name,"[",bad_row_vector,",]$",bad_variable_vector," <- ", default_placeholder, sep = "", collapse = "\n"))
  
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

add_sfdata_metadata <- function(my_data){
  my_data = 
    sfdatat1 <- merge(sfdata, ring_ids, by = c("ring_id", "year"))
  sfdatat2 <- merge(sfdatat1, projects, by = c("ring_number", "year"))
  sfdatat3 <- merge(sfdatat2, start_dates, by = c("project","year"))
  sfdatat4 <- merge(sfdatat3, fumigation_type, by = c("ring_number","year"))
  sfdatat5 <- merge(sfdatat4, end_dates, by = c("fumigation_type","year"))
  return(sfdatat5)
}

check_sfdata_dates <- function(sfdata){
  View(aggregate(dt ~ file_source, data = sfdata, FUN = unique))
}

convert_sfdata_variable_types <- function(my_sfdata){
  
  # TO-DO check if conversion is valid
  
  my_sfdata$datetime <- as.POSIXct(paste(my_sfdata$dt, my_sfdata$time)
                                   ,tz = 'GMT'
                                   ,format = "%m/%d/%Y %H:%M:%S")
  my_sfdata$datetime_trunc <- as.POSIXct(paste(my_sfdata$dt, my_sfdata$time)
                                         ,tz = 'GMT'
                                         ,format = "%m/%d/%Y %H:%M")
  my_sfdata$year <- as.character(as.POSIXlt(my_sfdata$datetime)$year+1900)
  
  my_sfdata$layer_1_concentration <- as.numeric(my_sfdata$layer_1_concentration)
  my_sfdata$wind_speed <- as.numeric(my_sfdata$wind_speed)
  my_sfdata$wind_direction <- as.numeric(my_sfdata$wind_direction)
  my_sfdata$layer_1_vout <- as.numeric(my_sfdata$layer_1_vout)
  my_sfdata$sector <- as.numeric(my_sfdata$sector)
  my_sfdata$ring_id <- as.numeric(my_sfdata$ring_id)
  my_sfdata$layer_1_setpoint <- as.numeric(my_sfdata$layer_1_setpoint)
  
  return(my_sfdata)
}


# my_sfdata <- my_sfdata[!is.na(my_sfdata$datetime) & my_sfdata$datetime >= my_sfdata$start_date,]


average_sf_data

investigate_statistical_properties

calc_sf_data_time_within_target

make_face_stats

# TO-DO add plotting functions


check = out_of_range_row %>%
  group_by(Range_flag)%>%
  summarise(n = n())
