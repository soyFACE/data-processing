## sandbox of correction process

source("analysis_files/soyface_data_processing_functions.R")

valid_range <- read.csv("metadata/valid_ranges.csv"
                        ,stringsAsFactors = FALSE
                        ,colClasses = c('character'))

sfdata_unchecked <- raw_sfdata_avg_to_dataframe("\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\")

sfdata_unchecked <- sfdata_unchecked[!is.na(sfdata_unchecked$"reporting_type"),]

nonconvertible_types <- check_sfdata_types(sfdata_unchecked) # need to remove NAs?

View(sfdata_unchecked[row.names(nonconvertible_types),])

make_error_template(nonconvertible_types, sfdata_unchecked, "NA")

out_of_range <- check_ranges(sf_data_unchecked, "wind_speed")
