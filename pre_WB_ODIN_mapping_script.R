path <- "S:/kachharaa/CONA/ODIN/ODIN_mapping"

setwd(path)
load("S:/kachharaa/CONA/ODINmapping/dust_datatable.RData")

coordinates.ODIN <- read.csv("S:/kachharaa/CONA/ODIN/ODIN_mapping/ODIN co-ordinates 2016.csv")

data_table$date_time <- as.POSIXct(as.character(data_table$date_time), 
                                   format = "%d/%m/%y %H:%M",Sys.timezone())
data_table$date <- as.POSIXct(as.character(data_table$date_time), 
                              format = "%Y-%m-%d",Sys.timezone())
data_table$hour <- hour(data_table$date_time)

#### cutting dates for evening means to go till 2AM. ####

extended.nights <- seq(as.POSIXct("2016-08-11 02:00:00", Sys.timezone()), 
                       as.POSIXct("2016-09-01 02:00:00", Sys.timezone()), by = "day")

data_table$extended.night <- cut(data_table$date_time, breaks = extended.nights, label = FALSE)

## because the data starts after midnight on 12th.
data_table$extended.night <- data_table$extended.night - 1

data_table <- data_table[which(data_table$extended.night >0),]
data_table$daytime <- ifelse(data_table$hour<2 | data_table$hour>=18, "night","day")

ODIN.evening.table <- aggregate(data_table$PM2_5, 
                                by = list(data_table$ODIN,data_table$extended.night, data_table$hour), 
                                FUN = mean, na.rm = T)

#ODIN.evening.table <- ODIN.evening.table[which(ODIN.evening.table$Group.3 == "night"),]
colnames(ODIN.evening.table) <- c("ODIN_ID","day.no","hour","study_mean")

ODIN.evening.table <- ODIN.evening.table[which(ODIN.evening.table$hour >=18 |
                                                 ODIN.evening.table$hour <=2),]

ODIN.evening.table <- ODIN.evening.table[which(ODIN.evening.table$day.no ==4),]

ODIN_final <- ODIN.evening.table
## adding dates to the final table. ####
#all.dates <- cbind.data.frame(day.no= 1:20, Date = unique(data_table$date))
#all.dates <- all.dates[1:19,]
#ODIN_final <- merge(all.dates, ODIN.evening.table, by = "day.no", all = TRUE)
#write.csv(all.dates,"all_dates.csv")
## creating spatial files for each day.
for( i in c( 0,1,2,18,19,20,21,22,23)){
  subset.data <- ODIN_final[which(ODIN_final$hour== i),]
  
  ## adding the coordinates:
  subset.data_wih_coordinates <- merge(coordinates.ODIN, subset.data, by = "ODIN_ID")
  coordinates(subset.data_wih_coordinates) <- cbind(subset.data_wih_coordinates$NZTM_E , 
                                                    subset.data_wih_coordinates$NZTM_N)
  proj4string(subset.data_wih_coordinates) <-  "+init=epsg:2193"
  
  # layerName <- paste0("Daily_ODIN/ODINmeans_dayno_",i)
  
  print(paste("Outputting for date 15/11/2017 HOUR:", subset.data$hour[1]))
  writeOGR(subset.data_wih_coordinates,
           paste0("hourly_ODIN15082017night/ODINmeans_1508HOUR_", i, ".shp"),
           layer = "ODIN_means",
           driver="ESRI Shapefile",
           overwrite_layer = T)
}
