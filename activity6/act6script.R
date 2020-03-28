#Activity 6 "the big one"
#Charlie Huemmler

#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)


#reading in data to labtop
#C:\Users\Charlie\Documents\GitHub\GEOG331\activity6\

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//GNPglaciers//GNPglaciers_1966.shp")
g1998 <- readOGR("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//GNPglaciers//GNPglaciers_1998.shp")
g2005 <- readOGR("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//GNPglaciers//GNPglaciers_2005.shp")
g2015 <- readOGR("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//GNPglaciers//GNPglaciers_2015.shp")

#str(g1966)
#plot(g1966, col = "black", axes = T)

g2015@polygons[[1]]

#question 1
g1966@proj4string

spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME == g2015@data$GLACNAME
g1966@data$GLACNAME == g2005@data$GLACNAME
g1966@data$GLACNAME == g1998@data$GLACNAME

#only 2015 is different, so thats the only year we need to fix
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))



#raster time
redL <- raster("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//glacier_09_05_14//l08_red.tif")
greenL <- raster("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//glacier_09_05_14//l08_green.tif")
blueL <- raster("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//glacier_09_05_14//l08_blue.tif")

#check coordinate system
redL@crs

#g1966@proj4string == redL@crs


#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#zoom in on an area using the ext function
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)


#NDVI data
#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("C:/Users/Charlie/Documents/GitHub/GEOG331/activity6//a06//NDVI//NDVI_",ndviYear[i],".tif"))
  
}

#higher NDVI represtents more vegitation on the ground
plot(NDVIraster[[1]])

str(NDVIraster[[1]])

NDVIraster[[1]]@crs
#question 2

#question 3
par(mfrow = c(1,2))
#par(mai=c(1,1,1,1))
plot(NDVIraster[[1]], axes=TRUE)
plot(g1966, axes=TRUE)


# this doesnt work!!!
# par(mai=c(1,1,1,1))
# plot(g1966, axes=TRUE)
# plot(NDVIraster[[1]], axes=TRUE, add = TRUE)


#question 4
#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#plot making
par(mai=c(1,1,1,1),mfrow = c(1,1),yaxt = "n",xaxt = "n")
plot(NDVIraster[[13]], main = "Levels of Vegetation compared to 2015 Glaciers", sub = "Greener indicaties more vegetation, Glaciers in black")
plot(g2015p, add = TRUE, col = NA, border = "black")

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

par(mai=c(1,1,1,1),mfrow = c(1,1),yaxt = "s",xaxt = "s")
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   

#question 5
for(i in 1:length(g2015)){
  g2015$diff[i]<-(area(g1966)[[i]]-area(g2015)[[i]])/area(g1966)[[i]]
}


spplot(g2015, "diff",main = "Percent Decrease of Glacier Size, 1966 to 2015", col = NA)


#question6
max(g2015$diff)

plotRGB(rgbL, ext=c(273200,275200,5426500,5428000), stretch="lin", axes=TRUE, main = "Boulder Glacier", sub = "84.72% Area Loss since 1966")
plot(g1966[5,], col=rgb(0, 41, 102, maxColorValue = 255), border=NA, add=TRUE)
plot(g1998[5,], col=rgb(0, 82, 204, maxColorValue = 255), border=NA, add=TRUE)
plot(g2005[5,], col=rgb(51, 133, 255, maxColorValue = 255), border=NA, add=TRUE)
plot(g2015[5,], col=rgb(153, 194, 255, maxColorValue = 255), border=NA, add=TRUE)
legend("right", 
       legend = c("1966","1998","2005","2015"),
       col = c(rgb(0, 41, 102, maxColorValue = 255), rgb(0, 82, 204, maxColorValue = 255),rgb(51, 133, 255, maxColorValue = 255), rgb(153, 194, 255, maxColorValue = 255)),
       pch = 15)


#question 7
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

par(mai=c(1,1,1,1),mfrow = c(1,1),yaxt = "s",xaxt = "s")
plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)


#question 8
#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units
#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)
#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#question 9
meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

g2015p$NDVImeanch <- meanChange[2:40,2]

spplot(g2015p, "NDVImeanch",  col = NA, main = "Change in mean NDVI of Area Surrounding Glacier, by Glacier")

