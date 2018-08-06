library(data.table)
library(zoo)
library(xts)
library(spatstat)
library(leaflet)
library(rgdal)
library(sp)
library(reshape)
library(reshape2)
library(ggplot2)

path <- "S:/kachharaa/CONA/ODIN/ODIN_mapping/"
setwd(path)

load("S:/kachharaa/CONA/ODIN/ODIN_mapping/wind_datatable.RData")
colnames(windtable) <- c("id","ws","wd","datetime")

windtable$siteID <- rep(c('CWS1','CWS2','CWS3'), times = 28800)
## convert the winds to math-acceptable format:
windtable$wd.math <- 270 - windtable$wd
windtable$wd.math <- ifelse(windtable$wd.math<0, windtable$wd.math+360,windtable$wd.math)

## calculate i and j vectors
windtable$u <- windtable$ws*cos(windtable$wd.math*0.01745329)
windtable$v <- windtable$ws*sin(windtable$wd.math*0.01745329)

## check if it worked by calculating the length of the vector and comparing with the wind speed:
windtable$ws.vectormath <- sqrt((windtable$u^2) + (windtable$v^2))
plot(windtable$ws~windtable$ws.vectormath)

windtable$datetime <- as.POSIXct(as.character(windtable$datetime), 
                                format = "%Y-%m-%d %H:%M:%S",
                                Sys.timezone())
windtable$date <- as.POSIXct(as.character(windtable$datetime), 
                                format = "%Y-%m-%d",
                                Sys.timezone())
windtable$hour <- hour(windtable$datetime)
windtable$extended.night <- cut(windtable$datetime, breaks = extended.nights, label = FALSE)
## because the data starts after midnight on 12th.
windtable$extended.night <- windtable$extended.night - 1

windtable <- windtable[which(windtable$extended.night == 4),]
windtable$daytime <- ifelse(windtable$hour<2 | windtable$hour>=18, "night","day")

windtable <- windtable[which(windtable$daytime == "night"),]

## calculate spatial vector mean for each timestamp
vector.means.u <- aggregate(windtable$u, by = list(windtable$hour), FUN = mean)
vector.means.v <- aggregate(windtable$v, by = list(windtable$hour), FUN = mean)
vector.means <- merge(vector.means.u,vector.means.v, by = "Group.1")
colnames(vector.means) <- c("hour",'u','v')


# ## extended nights and evening means:
# #### cutting dates for evening means to go till 2AM. ####
# vector.means$datetime <- as.POSIXct(as.character(vector.means$datetime), 
#                                     format = "%Y-%m-%d %H:%M:%S",
#                                     Sys.timezone())
# vector.means$date <- as.POSIXct(as.character(vector.means$datetime), 
#                                 format = "%Y-%m-%d",
#                                 Sys.timezone())
# vector.means$hour <- hour(vector.means$datetime)
# extended.nights <- seq(as.POSIXct("2016-08-11 02:00:00", Sys.timezone()), 
#                        as.POSIXct("2016-09-01 02:00:00", Sys.timezone()), by = "day")
# 
# vector.means$extended.night <- cut(vector.means$datetime, breaks = extended.nights, label = FALSE)
# 
# ## because the data starts after midnight on 12th.
# vector.means$extended.night <- vector.means$extended.night - 1
# 
# vector.means <- vector.means[which(vector.means$extended.night >0),]
# vector.means$daytime <- ifelse(vector.means$hour<2 | vector.means$hour>=18, "night","day")
# wind.evening.u <- aggregate(vector.means$u, 
#                                 by = list(vector.means$extended.night, vector.means$daytime), 
#                                 FUN = mean, na.rm = T)
# wind.evening.v <- aggregate(vector.means$v, 
#                             by = list(vector.means$extended.night, vector.means$daytime), 
#                             FUN = mean, na.rm = T)
# 
# wind.evening.table <- merge(wind.evening.u,wind.evening.v, by = c("Group.1", "Group.2"))
# colnames(wind.evening.table) <- c("day.no","daytime","u","v")

#wind.evening.table <- wind.evening.table[which(wind.evening.table$daytime == "night"),]

wind.evening.table <- vector.means
#write.csv(wind.evening.table,"./windmeans.csv")
##import ODIN_shift table
shift.R <- read.csv("hourly_ODIN15082017night/hourly_maximumRsq_shifts.csv")
wind.odin <- merge(shift.R,wind.evening.table, by = "hour", all = T)

wind.odin$shift.length <- sqrt((wind.odin$shifting.east^2) + (wind.odin$shifting.south^2))*0.001

wind.odin$shift.angle <- atan(-wind.odin$shifting.south/wind.odin$shifting.east)*57.29578
wind.odin$wind.angle <- atan(wind.odin$v/wind.odin$u)*57.29578

wind.odin$wind.angle.corr <- ifelse(wind.odin$wind.angle <0, 90-wind.odin$wind.angle,
                                    270-wind.odin$wind.angle)

wind.odin$shift.angle.corr <- ifelse(wind.odin$shift.angle <0, 90-wind.odin$shift.angle,
                                    270-wind.odin$shift.angle)

wind.odin$wind.length <- sqrt((wind.odin$u^2) + (wind.odin$v^2))
wind.odin <- wind.odin[1:19,]

wind.odin1 <- wind.odin[which(wind.odin$u >0 &wind.odin$v<0),]


ggplot(wind.odin1) +
  geom_point(aes(shift.angle.corr,wind.angle.corr)) +
  geom_smooth(aes(shift.angle.corr,wind.angle.corr),method = "lm", se = F)+
  scale_x_continuous(limit = c(0,360), 
                     breaks = c(0,90,180,270,360)) +
  scale_y_continuous(limit = c(0,360), 
                     breaks = c(0,90,180,270,360)) +
  theme_bw()



x <- lm(formula = wind.angle.corr~shift.angle.corr, data = wind.odin1)
summary(x)
