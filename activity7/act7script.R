#Activity 7
#install.packages(c("caret","randomForest"))

#install.packages("mapview","nnet")
install.packages("ggspatial")
install.packages("rasterVis")


library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(ggspatial)
library(ggplot2)
library(mapview)
library(caret)
library(randomForest)
library(nnet)
library(rasterVis)
library(broom)
library(dplyr)



#load in data
#set up directory for oneida data folder
dirR <- "C:/Users/Charlie/Documents/Colgate University/The Senior Year/EDS/act7data/oneida"

#for q5 graph
nylakes <- readOGR(dsn = dirR,
                   layer = "AreaHydrography")

#read in Sentinel data

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB5 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B05_20m.tif"))
rdatB6 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B06_20m.tif"))
rdatB7 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B07_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))
rdatB11 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B11_20m.tif"))
rdatB12 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B12_20m.tif"))
clouds <- raster(paste0(dirR,"/sentinel/MSK_CLDPRB_20m.tif"))

#read in validation data
#here verbose=FALSE hiddes
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
built <- readOGR(paste0(dirR,"/Oneida/built.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)


#take a closer look at the data
#stack red green blue
rgbS <- stack(rdatB4,rdatB3, rdatB2)
#stack all raster data
allbands <- stack(rdatB2,rdatB3,rdatB4,rdatB5,rdatB6,rdatB7, rdatB8,rdatB11, rdatB12,clouds)

#view raster, maximum digigtal is around 20000 so set scale to that
plotRGB(rgbS, scale=20000, stretch = "lin")

#use mapview package
#view rgb and set up a contrast stretch, exclude clouds with high values
# viewRGB(rgbS,r=1,g=2,b=3,maxpixels =  2297430, #view all pixels don' lower resolution
#         quantiles = c(0.00,0.995), #quantilesfor stretch. Cuts off high reflectance from clouds
#         homebutton=FALSE,
#         viewer.suppress=FALSE)#view in Rstudio 
# 
# 
# #use mapview package
# #view rgb and set up a contrast stretch, exclude clouds with high values
# #and view all landcover types
# viewRGB(rgbS,r=1,g=2,b=3,maxpixels =  2297430,quantiles = c(0.00,0.995), homebutton=FALSE,
#         viewer.suppress=FALSE)+
#   mapview(algae, color="grey25",col.regions="palegreen")+
#   mapview(agri, color="grey25",col.regions="violet")+
#   mapview(built, color="grey25",col.regions="darkgoldenrod3")+
#   mapview(forest, color="grey25",col.regions="tan4")+
#   mapview(water, color="grey25",col.regions="royalblue")+
#   mapview(wetlands, color="grey25",col.regions="orangered2")


#check out cloud cover
plot(allbands[[10]])

#set clouds to NA
allbandsCloud <- list()
for(i in 1:9){
  allbandsCloud[[i]] <- setValues(allbands[[i]],
                                  ifelse(getValues(allbands[[10]])>60,NA,getValues(allbands[[i]])))
  
}
allbandsCloudf <- stack(allbandsCloud[[1]],allbandsCloud[[2]],allbandsCloud[[3]],allbandsCloud[[4]],allbandsCloud[[5]],
                        allbandsCloud[[6]],allbandsCloud[[7]],allbandsCloud[[8]],allbandsCloud[[9]])

#view all layers
plot(allbandsCloudf)

plotRGB(allbandsCloudf,r=4, g=3, b=2,  
        scale=10000, 
        stretch="lin", 
        margins=TRUE,
        colNA="grey50")


#set seed so samples always the same
set.seed(13) #go gate


#randomly select 
algSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
algData <- rep("train",120)
#randomly replace half of the data to be validating data
algData[algSamp] <- "valid"


waterSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
waterData <- rep("train",120)
#randomly replace half of the data to be validating data
waterData[waterSamp] <- "valid"


agriSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
agriData <- rep("train",120)
#randomly replace half of the data to be validating data
agriData[agriSamp] <- "valid"


builtSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
builtData <- rep("train",120)
#randomly replace half of the data to be validating data
builtData[builtSamp] <- "valid"

forestSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
forestData <- rep("train",120)
#randomly replace half of the data to be validating data
forestData[forestSamp] <- "valid"


wetlandsSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
wetlandsData <- rep("train",120)
#randomly replace half of the data to be validating data
wetlandsData[wetlandsSamp] <- "valid"



#set up dataframe for coords and landclass
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

#set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
                           sampleType=c(algData,waterData,agriData,builtData,forestData, wetlandsData),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],built@coords[,1],forest@coords[,1],wetlands@coords[,1] ),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],built@coords[,2],forest@coords[,2],wetlands@coords[,2] ))


#extract raster data at each point
# I did a ?extract... its a pretty cool function. I ll keep it in mind for future use.
#using point coordinates
rasterEx <- data.frame(extract(allbandsCloudf,landExtract[,3:4]))
#give names of bands
colnames(rasterEx) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

#combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)
#preview
head(dataAll)

#seperate to training an validation data
trainD <- dataAll[dataAll$sampleType == "train",]
validD <- dataAll[dataAll$sampleType == "valid",]

trainD <- na.omit(trainD)
validD <- na.omit(validD)
#begin modelling
#Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:3) # number of variables available for splitting at each tree node

# Train the random forest model to the Sentinel-2 data
#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainD[,c(5:13)], #digital number data
                         y = as.factor(trainD$landcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#ERROR???


#check output
rf_model 



# Change name in raster stack to match training data
names(allbandsCloudf) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")
# Apply the random forest model to the Sentinel-2 data
rf_prediction <- raster::predict(allbandsCloudf, model=rf_model)
#view predictions
plot(rf_prediction)

#landcover class names
landclass

#set up categorical colors
landclass$cols <-c("#a6d854","#8da0cb","#66c2a5",
                   "#fc8d62","#ffffb3","#ffd92f")
#make plot and hide legend
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n")  

#get validation data from raster by extracting 
#cell values at the cell coordinates
rf_Eval <- extract(rf_prediction, validD[,3:4])

#make the confusion matrix
rf_errorM <- confusionMatrix(as.factor(rf_Eval),as.factor(validD$landcID))
#add landcover names
colnames(rf_errorM$table) <- landclass$landcover
rownames(rf_errorM$table) <- landclass$landcover
#view the matrix
rf_errorM$table

#look at the overall accuracy
rf_errorM$overall

#Neural Network time
#set up grid
nnet.grid <- expand.grid(size = seq(from = 16, to = 28, by = 2), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.1, to = 0.6, by = 0.1)) # regularization parameter to avoid over-fitting 

nnet_model <- caret::train(x = trainD[,c(5:13)], y = as.factor(trainD$landcID),
                           method = "nnet", metric="Accuracy", trainControl = tc, tuneGrid = nnet.grid,
                           trace=T)

nnet_model

# Apply the neural network model to the Sentinel-2 data
nnet_prediction <- raster::predict(allbandsCloudf, model=nnet_model)

#make plot and hide legend
plot(nnet_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE)
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n")

#extract predictions
nn_Eval = extract(nnet_prediction, validD[,3:4])
#confusion matrix
nn_errorM = confusionMatrix(as.factor(nn_Eval),as.factor(validD$landcID))
colnames(nn_errorM$table) <- landclass$landcover
rownames(nn_errorM$table) <- landclass$landcover
data.frame(nn_errorM$table)

par(mfrow=c(2,1), mai=c(0,0,0,0))
#random forest
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE)
#legend
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n")
#add title
mtext("Random Forest", side=3,cex=2, line=-5)

#neural network
plot(nnet_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
#add legend
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols, bty="n")   
#add title
mtext("Neural network", side=3,cex=2, line=-5)

#cell count neural net
nnet_freq <- freq(nnet_prediction)

#cell count random forest
rf_freq <- freq(rf_prediction)

#question 4
algdiff <- (nnet_freq[1,2] - rf_freq[1,2]) * 400

lakesizediff <- (nnet_freq[1,2] + nnet_freq[2,2] - rf_freq[1,2]-rf_freq[2,2]) * 400


#question 5 




nylakes$id <- row.names(nylakes)
nylakes_tidy <- tidy(nylakes)
nylakes_tidy <- left_join(nylakes_tidy, nylakes@data)

diffraster <- rf_prediction - nnet_prediction
rastbin <- function(rast){
  ifelse(rast == 0, 1,0)
}

ggplot()+
  geom_polygon(data = nylakes_tidy[which(nylakes_tidy$NAME == "Oneida Lake"),], aes(x = long, y = lat, group = group),
               fill = NA, color = "black")

diffraster <- calc(diffraster, fun = rastbin)

par(mfrow=c(1,1), mai=c(0,0,0,0))

gplot(diffraster) +
  geom_raster( aes(x = x, y = y, fill = as.factor(value)))+
  coord_fixed()+
  scale_fill_discrete(
                    name="",
                    breaks=c(1,0),
                    labels=c("Same", "Different"))+
  labs(title = "How often do the Neural Network and Random Forest agree?", 
       caption = paste(paste("They agreed for ", round(cellStats(diffraster, mean),3)*100,"% of the points with seed set to 13. #gogate", sep = "")))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  geom_polygon(data = nylakes_tidy[which(nylakes_tidy$NAME == "Oneida Lake"),], aes(x = long, y = lat, group = group),
               fill = NA, color = "black")
  

