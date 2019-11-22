# SoyFACE Data Processing Functions

############################load data###############################
if (FALSE){
  sfdata_without_na = sfdata[rowSums(is.na(sfdata)) != ncol(sfdata),]
  
  load("processed_r_data/sfdata_unchecked.rdata")
  load("processed_r_data/sfdata.Rdata")
  valid_range <- read.csv("metadata/valid_ranges.csv"
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
}

###################################################################

raw_sfdata_avg_to_dataframe <- function(source_file_location){
  # Dummy Data
  if (FALSE){
    source_file_location <- "\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\"
  }
  # End dummy data
  
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
  
  sfdata_header <- as.character(read.csv("metadata/minute_average_header_for_r.csv"
                                         ,header = FALSE
                                         ,sep = ","
                                         ,stringsAsFactors = FALSE
                                         ,colClasses = 'character')[1,])
  
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
  
  names(sfdata) <- sfdata_header
  return(sfdata)
}

#This needs the valid ranges passed to it - JAM
check_sfdata_types <- function(unchecked_df,my_range){
  # Dummy data
  if (FALSE){
    unchecked_df <- sfdata_without_na
    my_range <- valid_range
  }
  # End dummy data
  error_row <- data.frame(cbind(unchecked_df, flag = "text")) 
  error_row <- error_row[0,]
  
  
  for(i in names(unchecked_df)){
    my_type <- my_range[my_range$variable == i,"type"] # valid_range needs to be passed explicitly to this and all controlling functions.
    error_row_i <- check_types_convertible(i,my_type,unchecked_df)
    
    if(nrow(error_row_i)!= 0){
      error_row_i$flag <- i
      error_row = rbind(error_row,error_row_i)
    }
  }
  
  return(error_row)
}

check_types_convertible <- function(columname, my_type,unchecked_df){
  if(FALSE){
    columname <- "wind_speed"
    my_type <- "numeric"
    unchecked_df <-test_data
  }
  
  converted_column = lapply(unchecked_df[columname], FUN = function(x){as(x,my_type)})[[1]]
  converted_data = unchecked_df
  converted_data$converted_value = converted_column
  error_row_by_column <- converted_data[is.na(converted_data$converted_value),]
  error_row_by_column$converted_value <-  NULL
  return(error_row_by_column)
}

check_sfdata_range <- function(my_df,my_range){
  # Dummy Data
  if (FALSE){
    my_df <- out_of_range_data
    my_range <-  valid_range
  }
  # End dummy data
  my_df <- na.omit(my_df)
  out_of_range_row <- data.frame(cbind(my_df, type_flag = "text")) 
  out_of_range_row <- out_of_range_row[0,]
  
  for(i in names(my_df)){
    
    out_of_range_row_i <- check_ranges(i, my_df,my_range)
    
    if(nrow(out_of_range_row_i)!= 0){
      out_of_range_row = rbind(out_of_range_row,out_of_range_row_i)
    }
  }
  
  return(out_of_range_row)
}

check_ranges <- function(column_name,my_sfdata,my_range){
  if(FALSE){
    my_range <- valid_range
  }
  
  outlier <- data.frame(cbind(my_sfdata, Range_flag = "text")) 
  outlier <- outlier[0,]
  
  range_type <- my_range[my_range$variable == column_name,"type"] # valid_range needs to be passed explicitly to this and all controlling functions.
  
  empty_return <- data.frame(cbind(my_sfdata, Range_flag = "TEXT"))
  empty_return <- empty_return[0,]
  
  if(range_type != "numeric") return(empty_return)
  
  lower_limit <- my_range[my_range$variable == column_name,"lower_limit"] # valid_range needs to be passed explicitly to this and all controlling functions.
  upper_limit <- my_range[my_range$variable == column_name,"upper_limit"] # valid_range needs to be passed explicitly to this and all controlling functions.
  
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
  
  # Dummy data
  if (FALSE){
    sfdata_unchecked_errors <- sfdata_unchecked[seq(1,76, by = 10),]
    sfdata_unchecked_errors$original_df_row <- seq(1,76, by = 10)
    sfdata_unchecked_errors$bad_var_name <- "layer_1_concentration"
    
    error_df <- sfdata_unchecked_errors
    original_df <- sfdata_unchecked
    default_placeholder <- "NA"
  }
  # End dummy data
  
  
  original_df_name <- deparse(substitute(original_df)) # this might not do what I want inside a function
  bad_variable_vector <- error_df$flag
  bad_row_vector <- row.names(error_df)
  
  writeClipboard(paste(original_df_name,"[",bad_row_vector,",]$",bad_variable_vector," <- ", default_placeholder, sep = "", collapse = "\n"))
  
}

read_sfdata_metadata <- function(){
  ring_ids <- read.csv("metadata/ring_ids.csv"
                       ,stringsAsFactors = FALSE
                       ,colClasses = 'character'
  )
  
  projects <- read.csv("metadata/projects.csv"
                       ,stringsAsFactors = FALSE
                       ,colClasses = 'character'
  )
  start_dates <- read.csv("metadata/start_dates.csv"
                          ,stringsAsFactors = FALSE
                          ,colClasses = 'character'
  )
  end_dates <- read.csv("metadata/end_dates.csv"
                        ,stringsAsFactors = FALSE
                        ,colClasses = 'character'
  )
  fumigation_type <- read.csv("metadata/fumigation_type.csv"
                              ,stringsAsFactors = FALSE
                              ,colClasses = 'character'
  )
  
}

add_sfdata_metadata <- function(my_data){
  if(FALSE){
    my_data = sfdata_fill_gaps
  }
  # End dummy data
  sfdatat1 <- merge(my_data, ring_ids, by = c("ring_id","year"),all = TRUE) # JAM Need to add year when filling the gaps, otherwise this breaks.
  sfdatat2 <- merge(sfdatat1, projects, by = c("ring_number","year"),all = TRUE)
  sfdatat3 <- merge(sfdatat2, start_dates, by = c("project","year"),all = TRUE)
  sfdatat4 <- merge(sfdatat3, fumigation_type, by = c("ring_number","year"),all = TRUE)
  sfdatat5 <- merge(sfdatat4, end_dates, by = c("fumigation_type","year"),all = TRUE)
  return(sfdatat5)
}

check_sfdata_dates <- function(sfdata){
  View(aggregate(dt ~ file_source, data = sfdata, FUN = unique))
}

convert_sfdata_variable_types <- function(my_sfdata){
  
  # TO-DO check if conversion is valid
  if(FALSE){
    my_sfdata <- sfdata
  }
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

fill_gaps <- function(my_sfdata){
  #This function requires that the datetime be truncated to minutes. i.e. drop the seconds.
  if(FALSE){
    my_sfdata <- sfdata_in_valid_date_range
  }
  

  min(my_sfdata$datetime_trunc)
  max(my_sfdata$datetime_trunc)
  
  my_mins = seq(from = min(my_sfdata$datetime_trunc),  to = max(my_sfdata$datetime_trunc), by = 'min')

  all_mins <- data.frame(datetime_trunc = my_mins, ring_id = rep(unique(my_sfdata$ring_id), each = length(my_mins)))
  all_mins$year <- 2019
  sfdataday <- merge(my_sfdata, all_mins, by = c("ring_id",'datetime_trunc','year'), all.y= TRUE)

  #return(original data frame with each minute as an empty row)
}

date_sub <- function(start_row_number,end_row_number,my_sfdata){
  if(FALSE){
    start_row_number <-  409676
    end_row_number <-  409795
    my_sfdata <-  sfdata
  }
  correct_date <-  my_sfdata[end_row_number+1,]$dt
  for(i in start_row_number:end_row_number){
    my_sfdata[i,]$dt <- correct_date
  }
  return(my_sfdata)
  
}

out_of_range_sub <- function(flag,my_rows,my_sfdata){
 if(FALSE){
   my_row = out_of_range
   my_sfdata = sfdata_without_wrong_date
 } 
  my_sf_data_with_flag = merge(unconvertible_rows_with_NA,sfdata_without_wrong_date,all.y = TRUE)
}

subset_by_date <- function(start_date,end_date,my_sfdata){
  if(FALSE){
    start_date <-  "2019-06-11,00:00:00"
    end_date <-  "2019-09-25,23:59:59"
    my_sfdata <-  sfdata_type_converted
  }
  start_date <-  as.POSIXct(start_date)
  end_date <-  as.POSIXct(end_date)
  
  valid_sfdata <-  my_sfdata[which(my_sfdata$datetime >= start_date),]
  my_sfdata <-  valid_sfdata[which(valid_sfdata$datetime <= end_date),]
  
  return(my_sfdata)
  
  
}


fix_out_of_range <- function(my_csv,my_sfdata,ambient_ring_id){
  if(FALSE){
    my_csv <- out_of_range_conentration
    my_sfdata <- sfdata_fill_gaps
    ambient_ring_id <- 16
  }
  
  my_csv <- na.omit(my_csv)
  ambient_subset <- my_sfdata[which(my_sfdata$ring_id == ambient_ring_id),]
  ambient_subset$dt <-  as.Date(ambient_subset$dt,format="%m/%d/%Y") 
  ambient_subset$time <- strptime(ambient_subset$time, "%H:%M")

  my_csv$datetime <- as.POSIXct(paste(my_csv$dt, my_csv$time)
                                ,tz = 'GMT'
                                ,format = "%m/%d/%Y %H:%M:%S")
  
  my_csv$dt <- as.Date(my_csv$dt,format="%m/%d/%Y")
  my_csv$time <- strptime(my_csv$time, "%H:%M")
  my_csv <- subset_by_date("2019-06-11,00:00:00","2019-09-25,23:59:59",my_csv)

  ## Deal with ambinet value
  
  
  my_csv_char_replacement <- my_csv[!is.na(my_csv$replacement_value == "0 or ambient"),]
  my_csv_need_ambient <-  my_csv_char_replacement[my_csv_char_replacement$replacement_value == "0 or ambient",]

  
  for(i in 1:nrow(my_csv_need_ambient)){
    temp_dt <- my_csv_need_ambient[i,]$dt
    temp_time <- my_csv_need_ambient[i,]$time
    
    ambient_subset_i <-  ambient_subset[which(ambient_subset$dt == temp_dt),]
    ambient_subset_i <- ambient_subset_i[which(ambient_subset_i$time == temp_dt),]
   
    my_sfdata[which(my_sfdata$dt == temp_dt & my_sfdata$time == temp_time),]$layer_1_concentration <- ambient_subset_i$layer_1_concentration
  }
  
  ##Deal with other out of range
  my_csv_other <-  my_csv[my_csv$replacement_value != "0 or ambient",]
  for(i in 1:nrow(my_csv_other)){
    print(my_csv[i,]$X)
    temp_row_index <- my_csv[i,]$X
    my_sfdata[temp_row_index,]$layer_1_concentration <- my_csv[i,]$replacement_value
  }
  
  return(my_sfdata)
}


create_groupby_csv <- function(my_rows){
  # 2019-11-22 JAM. This should not have hard-coded columns or file names.
  if(FALSE){
    my_rows <- out_of_range_rows
  }
  require(data.table)
  out_of_range_rows_table <- data.table(my_rows)
  
  out_of_range_groupby_result <-  out_of_range_rows_table[, .N, by = .(Range_flag)] 
  out_of_range_wind_direction <-  out_of_range_rows[which(out_of_range_rows$Range_flag == "wind_direction"),]
  out_of_range_layer_1_con <- out_of_range_rows[which(out_of_range_rows$Range_flag == "layer_1_concentration"),]
  out_of_range_wind_direction <- data.frame(out_of_range_wind_direction)
  out_of_range_layer_1_con <- data.frame(out_of_range_layer_1_con)
  write.csv(out_of_range_wind_direction,"out_of_range/out_of_range_wind_direction.csv")
}






# my_sfdata <- my_sfdata[!is.na(my_sfdata$datetime) & my_sfdata$datetime >= my_sfdata$start_date,

# average_sf_data
# 
# investigate_statistical_properties
# 
# calc_sf_data_time_within_target
# 
# make_face_stats

# TO-DO add plotting functions

