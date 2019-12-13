source("analysis_files/soyface_data_processing_functions.R")

#############################Load data################################
######################################################################
load("processed_r_data/sfdata.Rdata")
load("processed_r_data/sfdata_fill_gaps.Rdata")

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

#sfdata <- raw_sfdata_avg_to_dataframe("\\\\commons2.life.illinois.edu\\soyface_fumigation_data\\2019\\") ## Runing time 3.330841 mins
#

sfdata_1_raw <- sfdata
rm(sfdata)

sfdata_2_without_empty <- sfdata_1_raw[rowSums(is.na(sfdata_1_raw)) != ncol(sfdata_1_raw),] ##Delet empty row
sfdata_3_without_wrong_date <- date_sub(522055,522174,sfdata_2_without_empty)

# Evaluate
unconvertible_rows <- check_sfdata_types(sfdata_3_without_wrong_date,valid_range) ## Serch for unconvertible datapoints (Runing time: 18.89308 secs)
# /Evaluate

sfdata_4_type_converted <- convert_sfdata_variable_types(sfdata_3_without_wrong_date)
sfdata_5_in_valid_date_range <- subset_by_date("2019-06-11,00:00:00","2019-09-25,23:59:59",sfdata_4_type_converted)

sfdata_6_fill_gaps <- fill_gaps(sfdata_5_in_valid_date_range)
#save(sfdata_fill_gaps, file = "processed_r_data/sfdata_fill_gaps.Rdata")

sfdata_6_fill_gaps$layer_2_setpoint <-  0
out_of_range_rows <- check_sfdata_range(sfdata_6_fill_gaps,valid_range)

out_of_range_rows <- out_of_range_rows[order(out_of_range_rows$Range_flag,out_of_range_rows$ring_id,out_of_range_rows$dt),]

out_of_range_rows$datetime_trunc <- paste("=\"",as.character(out_of_range_rows$datetime_trunc),"\"")

write.csv(out_of_range_rows, file = "out_of_range/out_of_range_all_variables.csv"
          ,row.names = FALSE)
#create_groupby_csv(out_of_range_rows)

out_of_range_fix <- read.csv("out_of_range/out_of_range_all_variables.csv"
                                      ,stringsAsFactors = FALSE
                                      ,colClasses = 'character')

# start = Sys.time() # 2019-12-12 JAM. Use system.time(expression) instead. Also there is a start() function and creating a variable "start" will break it.

system.time(sfdata_7_without_out_of_range <- fix_out_of_range(out_of_range_conentration,sfdata_6_fill_gaps, ambient_ring_id = 16))
# end = Sys.time() # 2019-12-12 JAM There is an end() function.
# end - start

sfdata_8_with_metadata <- add_sfdata_metadata(sfdata_7_without_out_of_range) 
#save(sfdata_with_metadata, file = "processed_r_data/sfdata_with_metadata.Rdata")

rm(sfdata_1_raw,sfdata_2_without_empty,sfdata_3_without_wrong_date,sfdata_4_type_converted,sfdata_5_in_valid_date_range,sfdata_6_fill_gaps,sfdata_7_without_out_of_range)

###################################################################################
sfdata_ambient <- sfdata_8_with_metadata[sfdata_8_with_metadata$ring_number=="Ambient",]
o3dataday <- sfdata_with_metadata[!sfdata_with_metadata$ring_id %in% c(0,1,2,3),]
co2dataday <- sfdata_with_metadata[sfdata_with_metadata$ring_id %in% c(0,1,2,3),]

sfdata_ambient <- sfdata_ambient[order(sfdata_ambient$ring_id,sfdata_ambient$datetime_trunc),]
co2dataday_df <- co2dataday[order(co2dataday$ring_id,co2dataday$datetime_trunc),]
o3dataday_df <- o3dataday[order(o3dataday$ring_id,o3dataday$datetime_trunc),]


pdf("2019_ambient_daily_gaps_showns.pdf", height = 8.5, width = 11)
by(sfdata_ambient, sfdata_ambient$dt, FUN = function(x){
  by(x,x$ring_id, FUN = function(y){
    plot(y$datetime_trunc, y$layer_2_concentration, type = 'b'
         ,ylim = c(0,1000)
         ,main = paste(unique(y$ring_number),unique(y$dt))
         ,cex = .8
    )
    abline(h = 600, lty = 2)
  })
}
)
dev.off()




pdf("2019_ambient_daily.pdf", height = 8.5, width = 11)

by(sfdata_ambient, sfdata_ambient$dt, FUN = function(x){
  
  by(x,x$ring_id, FUN = function(y){
    
    plot(y$datetime, y$layer_2_concentration, type = 'b'
         
         ,ylim = c(0,1000)
         
         ,main = paste(unique(y$ring_number),unique(y$dt))
         
    )
    points(y$datetime, y$wind_speed*100
           
           ,col = 'purple'
           
           ,pch = 10)

    abline(h = 600, col = 'blue', lty = 2)
    
    abline(h = c(480,720), col = 'blue', lty = 4)
    
  })
  
}

)
dev.off()


pdf("2019_Co2_Rings_daily.pdf", height = 8.5, width = 11)

by(co2dataday_df, co2dataday_df$dt, FUN = function(x){
  
  par(mfrow=c(4,1), mar =  c(1, 2, 2, 1) + 0.1)
  
  by(x,x$ring_id, FUN = function(y){
    
    plot(y$datetime, y$layer_1_concentration, type = 'b'
         
         ,ylim = c(0,1000)
         
         ,main = paste(unique(y$ring_number),unique(y$dt))
         
    )
    points(y$datetime, y$wind_speed*100
           
           ,col = 'purple'
           
           ,pch = 10)
    
    lines(y$datetime, y$layer_1_vout*100
          
          ,col = 'red')
    
    abline(h = 600, col = 'blue', lty = 2)
    
    abline(h = c(480,720), col = 'blue', lty = 4)
    
  })
  
}

)
dev.off()



pdf("2019_O3_Rings_daily.pdf", height = 8.5, width = 11)

by(o3dataday_df, o3dataday_df$dt, FUN = function(x){
  
  par(mfrow=c(4,1), mar =  c(1, 2, 2, 1) + 0.1)
  
  by(x,x$ring_id, FUN = function(y){
    
    plot(y$datetime, y$layer_1_concentration, type = 'b'
         
         ,ylim = c(0,1000)
         
         ,main = paste(unique(y$ring_number),unique(y$dt))
         
    )
    points(y$datetime, y$wind_speed*100
           
           ,col = 'purple'
           
           ,pch = 10)
    
    lines(y$datetime, y$layer_1_vout*100
          
          ,col = 'red')
    
    abline(h = 600, col = 'blue', lty = 2)
    
    abline(h = c(480,720), col = 'blue', lty = 4)
    
  })
  
}

)
dev.off()


