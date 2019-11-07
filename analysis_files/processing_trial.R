source("analysis_files/soyface_data_processing_functions.R")

#############################Load data################################
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

sfdata <- raw_sfdata_avg_to_dataframe("\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\")

sfdata <- sfdata[rowSums(is.na(sfdata)) != ncol(sfdata),] ##Delet empty row

unconvertible_rows <- check_sfdata_types(sfdata,valid_range) ## Serch for unconvertible datapoints 

out_of_range_rows <- check_sfdata_range(sfdata,valid_range) ## Search for out of range datapoints

sfdata_with_metadata <- add_sfdata_metadata(sfdata)



