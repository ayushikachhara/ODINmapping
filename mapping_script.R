
############### LIBRARIES,constants and imports ###########################

library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(dismo)
library(XML)
library(rgdal)
library(rgeos)
library(spatstat)
library(leaflet)
library(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)

latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"
shifting.constants <-c(-1000,-900,-800,-700,-600,-500,-400,-300,-200,-100,
                       0,100,200,300,400,500,600,700,800,900,1000)
sigma.constant <- 350

## reading the files in
#ODIN_monthlymeans_true.copy <- readOGR("S:/kachharaa/CONA/ODIN/ODIN_mapping/ODIN_monthlymeans.shp")
WB_centroids <- readOGR("S:/kachharaa/CONA/ODIN/ODIN_mapping/heating_census_MB_centroids.shp")
all.dates <- read.csv("S:/kachharaa/CONA/ODIN/ODIN_mapping/Daily_ODIN/all_dates.csv")

################# subsetting the census centroids only for the Rangiora region ###############
WB_centroids <- WB_centroids[which(WB_centroids$UA2013_NAM == "Rangiora"),]
WB_centroids <- WB_centroids[,46]  ## taking only WB_density.


#### checks on the wood burner density file to get the projection extents ######
WB_centroids.table <- as.data.frame(WB_centroids)
head(WB_centroids.table)
summary(WB_centroids.table$coords.x1)

#### create an object of class ppp as required by spatstat package functions #######
WB_centroids.ppp <- ppp(WB_centroids.table[,2], WB_centroids.table[,3], c(1564000,1570000),
                        c(5201000,5208000))
plot(WB_centroids.ppp)
marks(WB_centroids.ppp) <- WB_centroids.table[,1]

############ colour scles for the plots ################
norm_palette <- colorRampPalette(c("white","green","yellow","orange","red"))


### kernel density function #######
d0 <- density(WB_centroids.ppp,sigma = sigma.constant, weight = WB_centroids.ppp$marks, positive = TRUE)
plot(d0, col = norm_palette(10))

#### creating a raster of the obtained spatial kernel: ####
dr0 <- raster(d0)
proj4string(dr0) <- CRS("+init=epsg:2193")

# ODIN_monthlymeans <- ODIN_monthlymeans_true.copy
# 
# ODIN_monthlymeans <- spTransform(ODIN_monthlymeans, CRS(NZTM_CRS))
# ODIN_monthlymeans_latlon <- spTransform(ODIN_monthlymeans, CRS(latlon_CRS))
# pal <- colorQuantile("YlOrBr",ODIN_monthlymeans$study_mean, n = 5)
# 
# leaflet() %>% addRasterImage(dr0, colors = norm_palette(10)) %>% 
#   addCircleMarkers(data = ODIN_monthlymeans_latlon,
#                    group = 'B',
#                    color = "black",
#                    weight = 2,
#                    fillColor = ~pal(study_mean),
#                    radius = 9,
#                    label = ~paste("ODIN",as.character(ODIN_ID)),
#                    stroke = TRUE,
#                    fillOpacity = 1)

#### ODIN data import and manipulation:
path <- "S:/kachharaa/CONA/ODIN/ODIN_mapping/hourly_ODIN15082017night/"

setwd(path)
all_days <- list.files(path = path, pattern = "*.shp")

fit.vector.final <- cbind.data.frame(R.squared = NA,
                                     Pvalue = NA,
                                     shifting.east = NA,
                                     shifting.south = NA,
                                     hour = NA)

#ODIN_means.truecopy <- readOGR(all_days[1])
#### transforming the coordinates for measurement sites ################## 
for (day in all_days){
  print(day)
  ## read in
  ODIN_means.truecopy <-  readOGR(day)
  ODIN_means <- ODIN_means.truecopy
  ODIN_means <- spTransform(ODIN_means, CRS(NZTM_CRS))
  pal <- colorQuantile("YlOrBr",ODIN_means$study_mean, n = 5)
  
  shifting.south.constant <- -100
  
  
  for(shifting.east in shifting.constants) {
    
    print(paste("Shifting  easthwards by", shifting.east, "metres"))
    ODIN_means@coords[,1] <- ODIN_means@coords[,1] - shifting.east
    
    ##### re-defining the projection ONLY REQUIRED IF USING LEAFLET FOR VISUALS ######
    
    # ODIN_means_latlon <- spTransform(ODIN_means, CRS(latlon_CRS))
    
    ############## final output tables #############
    WB.density.extracted <- cbind.data.frame(ODIN_ID = ODIN_means$ODIN_ID,
                                             WB.density.extracted = extract(dr0,ODIN_means))
    final.output <- as.data.table(merge(ODIN_means,
                                        WB.density.extracted,by = "ODIN_ID", all = T))
    
    ############# resulting fit calculations and plots.
    fit <- lm(study_mean~WB.density.extracted, data = final.output)
    fit.vector <- cbind(R.squared = summary(fit)$adj.r.squared,
                        Pvalue = signif(summary(fit)$coef[2,4], 4),
                        shifting.east = shifting.east, 
                        shifting.south = shifting.south.constant,
                        hour = final.output$hour[1])
    
    fit.vector.final <- rbind(fit.vector.final,fit.vector)
    ODIN_means <- ODIN_means.truecopy
    ODIN_means <- spTransform(ODIN_means, CRS(NZTM_CRS))
    ODIN_means@coords[,2] <- ODIN_means@coords[,2] + shifting.south.constant
  }
  
  print(paste(day, "done."))
}

fit.vector.final <- fit.vector.final[-1,]
#fit.vector.final1 <- merge(fit.vector.final,all.dates, by = "day.no")
fit.vector.final1 <- fit.vector.final
 
fit.vector.final1$hour_f<-  factor(as.character(fit.vector.final1$hour), 
                                 levels=c('18','19','20','21','22','23','0','1','2'))

ggplot(data = fit.vector.final1) +
  geom_point(aes(x = shifting.east, y = R.squared), size = 2) +theme_bw()

ggplot(data = fit.vector.final1) +
  geom_point(aes(x = shifting.south, y = R.squared), size = 2) +theme_bw()

#### plotting the final.output
plot1 <- ggplot(data = fit.vector.final1) + theme_bw()+
  geom_tile(aes(x =shifting.east ,y = shifting.south,fill = R.squared), color = "black") +
  scale_fill_gradient(low = "beige", high = "brown3", guide="colorbar")+
  scale_x_continuous(limits = c(-1000,1000), breaks = shifting.constants,
                     labels = as.character(shifting.constants), expand = c(0,0)) +
  scale_y_continuous(breaks = unique(fit.vector.final$shifting.south),
                     trans = "reverse") +
  ggtitle("15 August 2016 - Evening PM2.5 mean correlation with Woodburner Density (Sigma = 350)") +
  xlab("Shifting smoke in the East-West Direction (meters)") + 
  ylab("Shifting smoke in the North-South Direction (meters)") +
  theme(plot.title = element_text(size = 16,hjust = 0.5),
        axis.text = element_text(size = 5,hjust = 0.5),
        axis.title = element_text(size = 12,hjust = 0.5))


plot1 <- plot1 +facet_wrap( ~ hour_f, ncol=4)

print(plot1)
#write.csv(fit.vector.final1, "S:/kachharaa/CONA/ODIN/ODIN_mapping/fit.vector_15August2016_sig350.csv")

##############################################
#####for every 100m shift towards south calculate R.squared at 10 shifts to the east at 100m steps.####

# fit.vector.final <- cbind.data.frame(R.squared = NA,
#                                      Pvalue = NA,
#                                      shifting.east = NA,
#                                      shifting.south = NA)
# 
# 
# shifting.south.constant <- 100
# 
# 
# for(shifting.east in shifting.constants) {
#   print(paste("Shifting  easthwards by", shifting.east, "metres"))
#   ODIN_monthlymeans@coords[,1] <- ODIN_monthlymeans@coords[,1] - shifting.east
# 
#   ##### re-defining the projection ONLY REQUIRED IF USING LEAFLET FOR VISUALS ######
# 
#   # ODIN_monthlymeans_latlon <- spTransform(ODIN_monthlymeans, CRS(latlon_CRS))
# 
#   ############## final output tables #############
#   WB.density.extracted <- cbind.data.frame(ODIN_ID = ODIN_monthlymeans$ODIN_ID,
#                                            WB.density.extracted = extract(dr0,ODIN_monthlymeans))
#   final.output <- as.data.table(merge(ODIN_monthlymeans,WB.density.extracted,by = "ODIN_ID", all = T))
# 
#   ############# resulting fit calculations and plots.
#   fit <- lm(study_mean~WB.density.extracted, data = final.output)
#   fit.vector <- cbind(R.squared = summary(fit)$adj.r.squared,
#                       Pvalue = signif(summary(fit)$coef[2,4], 4),
#                       shifting.east = shifting.east,
#                       shifting.south = shifting.south.constant)
#   fit.vector.final <- rbind(fit.vector.final,fit.vector)
#   ODIN_monthlymeans <- ODIN_means_2108
#   ODIN_monthlymeans <- spTransform(ODIN_monthlymeans, CRS(NZTM_CRS))
#   ODIN_monthlymeans@coords[,2] <- ODIN_monthlymeans@coords[,2] + shifting.south.constant
#   }
# 
# fit.vector.final <- fit.vector.final[-1,]
# fit.vector.final2108 <- fit.vector.final
