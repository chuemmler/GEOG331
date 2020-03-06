#Activity 6 "the big one"
#Charlie Huemmler

install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
str(g1966)
plot(g1966, col = "black", axes = T)
