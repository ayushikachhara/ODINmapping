library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)


setwd("S:/kachharaa/CONA/ODIN/ODIN_mapping/hourly_ODIN15082017night/")


fit.file <- read.csv("fit.vector_15August2016_sig350.csv")

#fit.file$Date <- as.Date(as.character(fit.file$Date), format = "%d/%m/%Y")

fit.file_max <- aggregate(fit.file$R.squared, 
                          by = list(fit.file$hour), 
                          FUN = max, na.rm = T) 

fit_daily_max <- tapply(fit.file$R.squared, fit.file$hour, max)
fit_max_index <- which(fit.file$R.squared %in% fit_daily_max)

write.csv(fit_daily_maxtable, "hourly_maximumRsq_shifts.csv")



fit_noshift <- fit.table


fit_daily_maxtable <- fit.file[fit_max_index,]

colnames(fit.file_max) <- c("day.no","R.squared")

fit.file_max <- merge(fit.file_max,fit.file, by = c("day.no", "R.squared"), all = F)

fit.file_max <- fit.file_max[which(fit.file_max$shifting.east !=0),]

ggplot(fit.file) +
  geom_boxplot(aes(Date,R.squared, color = night_classification, group = Date)) + theme_bw()+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
  ggtitle("Night Classification - BOX PLOT")

ggplot(fit.file) +
  geom_point(aes(eve_u,R.squared))+
  geom_smooth(aes(eve_u,R.squared, color = night_classification), method = "lm") + theme_bw()+
  ggtitle("Night Classification - BOX PLOT")



ggplot(fit.file_max) +
  geom_point(aes(night_PM2_5, R.squared,color = night_classification)) + 
  geom_smooth(aes(night_PM2_5,R.squared),se = F, method = "lm") +
  theme_bw() + ggtitle("max_R-squared vs. meanPM2.5(nighttime)") +
  ylab("R-squared daily maximum")

  
