source("analysis_files/soyface_data_processing_functions.R")

#############################Load data################################
load("processed_r_data/out_of_range_rows.Rdata")
load("processed_r_data/sfdata.Rdata")

valid_range <- read.csv("metadata/valid_ranges.csv"
                        ,stringsAsFactors = FALSE
                        ,colClasses = c('character'))

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
######################################################################
## sfdata <- raw_sfdata_avg_to_dataframe("\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\") ## Runing time 3.330841 mins

# Evaluate
sfdata_without_empty <- sfdata[rowSums(is.na(sfdata)) != ncol(sfdata),] ##Delet empty row
# /Evaluate
# Fix
sfdata_without_wrong_date <- date_sub(409676,409795,sfdata_without_empty)
save(sfdata_without_wrong_date, file = "processed_r_data/sfdata_without_wrong_date.Rdata")
# /Fix
# Evaluate
unconvertible_rows <- check_sfdata_types(sfdata_without_wrong_date,valid_range) ## Serch for unconvertible datapoints (Runing time: 18.89308 secs)
# /Evaluate
# Fix
unconvertible_rows_with_NA <- na_sub(unconvertible_rows,sfdata_without_wrong_date)
# /Fix

# Evaluate
out_of_range_rows <- check_sfdata_range(sfdata_without_wrong_date,valid_range) ## Search for out of range datapoints  (Runing time: 52.14598 secs)
# save(out_of_range_rows, file = "processed_r_data/out_of_range_rows.Rdata")
# /Evaluate
#Fix
sfdata_without_wrong_date$layer_2_concentration <-  0
sfdata_without_wrong_date$layer_2_setpoint <-  0
# /Fix

check <- out_of_range_rows[which(out_of_range_rows$Range_flag == "wind_direction"),]
check2 <-  out_of_range_rows[which(out_of_range_rows$Range_flag == "leaf_wetness"),]

##sfdata_with_NA <- merge(unconvertible_rows_with_NA,sfdata_without_wrong_date,all.y = TRUE)
##sfdata_with_NA$flag <-  NULL

sfdata_type_converted <- convert_sfdata_variable_types(sfdata_without_wrong_date)

sfdata_in_valid_date_range <- subset_by_date("2019-06-11,00:00:00","2019-09-25,23:59:59",sfdata_type_converted)

sfdata_fill_gaps <- find_gaps(sfdata_in_valid_date_range)

sfdata_with_metadata <- add_sfdata_metadata(sfdata) ## Merge with metadata (Runing time: 25.51346 secs)

