#activity 5 code
#2/24

#load packages
library(lubridate)
library(tidyverse)
library(ggplot2)

#read in streamflow data
#datclean <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",na.strings = c("Eqp"))

#pull data from labtop
#datH -> datdirty

datclean <- read.csv("C:/Users/Charlie/Documents/GitHub/GEOG331/activity5/a05/stream_flow_data.csv", na.strings = c("Eqp"))
head(datclean)    

#read in precipitation data
#hourly precipitation is in mm
#datRain <- read.csv("y:\\Data\\activities\\a05\\2049867.csv") 

#labtop pull
#datP -> datRain
datRain<- read.csv("C:/Users/Charlie/Documents/GitHub/GEOG331/activity5/a05/2049867.csv")
head(datRain)

#only use most reliable measurements
#datD-> datclean
datclean <- datclean[datclean$discharge.flag == "A",]

#question2

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datclean$date, "%m/%d/%Y")
#get day of year
datclean$doy <- yday(datesD)
#calculate year
datclean$year <- year(datesD)
#define time
timesD <- hm(datclean$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datRain$DATE)
#get day of year
datRain$doy <- yday(dateP)
#get year 
datRain$year <- year(dateP)

#another date column in datRain

datclean$D2 <- date(datesD)
datRain$D2 <- date(dateP)



#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datclean$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datclean$decDay <- datclean$doy + (datclean$hour/24) - 1
#calculate a decimal year, but account for leap year
datclean$decYear <- ifelse(leap_year(datclean$year),datclean$year + (datclean$decDay/366),
                       datclean$year + (datclean$decDay/365))
#calculate times for datRain                       
datRain$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datRain$decDay <- datRain$doy + (datRain$hour/24) - 1
#calculate a decimal year, but account for leap year
datRain$decYear <- ifelse(leap_year(datRain$year),datRain$year + (datRain$decDay/366),
                       datRain$year + (datRain$decDay/365))       


#question3
#plot discharge
plot(datclean$decYear, datclean$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))


#question4

plot(datclean$decYear, datclean$discharge, type="l", xlab="Year - exptest", ylab=paste("Discharge ft3 ","sec-1"))
plot(datclean$decYear, datclean$discharge, type="l", xlab="Year - exptest", ylab=expression("V"^"3"))


#question5
#basic formatting
aveF <- aggregate(datclean$discharge, by=list(datclean$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datclean$discharge, by=list(datclean$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,175),
     xaxs="i", yaxs ="i",
     xaxt = "n",
     main = "Average Flow Rate by Day of Year",
     sub = "Data from USGS Site #4240100")  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA)
legend("topleft", c("Average Flow Rate 2007-2019","2017 Flow Rate","1 standard deviation from average"), #legend items
       lwd=c(2,2,NA),#lines
       col=c("black", rgb(1,0,0,.7),rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,NA,15),#symbols
       bty="n")#no legend border
lines(datclean$doy[which(datclean$year == 2017)], datclean$discharge[which(datclean$year == 2017)],
     type = "l",
     col = rgb(1, 0, 0,.7))
axis(side = 1, at=c(1,32,60,91,121,152,182,213,244,274,305,335),
     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
     las = 2)

#question 7


#date vec
datevec <- data.frame(seq(ymd('2007-1-1'), ymd('2013-12-31'), by = "day"))
datevec$allobs <- rep(NA, nrow(datevec))

colnames(datevec)= c("D2","allobs")

j <- 1
for(i in 1:nrow(datevec)){    #for each date... i 
  count<-0
  
  while(datevec$D2[i] == datRain$D2[j] & j < 16151){  #matches up day with Rain Data
    j <- j + 1     #iterates through hourly observations in Rain Data
    count <- count + 1     #counts how many hours of data are in each day
    #print(paste(j, count, datRain$D2[j])) troubleshooting
  }
  #print(paste(j, count, datRain$D2[j])) troubleshooting
  ifelse(count == 24,datevec$allobs[i] <- TRUE, datevec$allobs[i] <- FALSE)  #if amount of hours
}

table(datevec$allobs) #check to see it worked. not many days with all observations!!

#combine dataframes for graphing

q7plotdf <- left_join(distinct(datclean), distinct(datevec), by = "D2")
q7plotdf <- q7plotdf[!is.na(q7plotdf$allobs),]

#now we can get plotting

dev.new(width=8,height=8)

ggplot(data = q7plotdf, aes(y = discharge, x = decYear))+
  geom_line()+
  theme_bw()+
  geom_point(data = q7plotdf[q7plotdf$allobs == TRUE,], aes(x = decYear, y = -10, color = allobs))+
  labs(title = "Discharge over Time", y = expression(paste("Discharge ft"^"3 ","sec"^"-1")), x = "Year", subtitle = "Red dot indicates all precipitation observations are recorded for that day")+
  theme(legend.position = "none")

#graph past q7
#subsest discharge and precipitation within range of interest
hydroD <- datclean[datclean$doy >= 248 & datclean$doy < 250 & datclean$year == 2011,]
hydroP <- datRain[datRain$doy >= 248 & datRain$doy < 250 & datRain$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#question 8

hydroD2 <- datclean[datclean$doy >= 5 & datclean$doy < 10 & datclean$year == 2011,]
hydroP2 <- datRain[datRain$doy >= 5 & datRain$doy < 10 & datRain$year == 2011,]

min(hydroD2$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD2$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh-yl)/(pm-pl)) * hydroP2$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     main= "Hydrograph of Harbor Brook, Syracuse NY",
     sub= "Blue bars indicate level of precipitation")
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(yl,hydroP2$pscale[i],hydroP2$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#specify year as a factor
datclean$yearPlot <- as.factor(datclean$year)
#make a boxplot
ggplot(data= datclean, aes(yearPlot,discharge)) + 
  geom_boxplot()


#make a violin plot
ggplot(data= datclean, aes(yearPlot,discharge)) + 
  geom_violin()

#question 9
season<- function(doy){
  if(doy > 78 & doy <= 171){
    return("Spring")
  }
  if(doy > 171 & doy <= 265){
    return("Summer")
  }
  if(doy > 265 & doy <= 355){
    return("Fall")
  }
  if(doy > 355 | doy <= 78){
    return("Winter")
  }
}

datclean$season <- sapply(X = datclean$doy, FUN = season)
datclean$season <- factor(x = datclean$season, levels= c("Spring","Summer","Fall","Winter"))
#lets get plotting
library(gridExtra)

p1<-ggplot(data = datclean[datclean$year == 2016,], aes(x = season, y = discharge))+
  geom_violin(fill = "blue")+
  theme_bw()+
  scale_y_continuous(limits = c())+
  labs(y= "Discharge", x = "2016", title = "Discharge Rates by Season and Year", subtitle = expression(paste("Measured in ft"^"3 ","sec"^"-1")))+
  theme(axis.text.x = element_blank())
  

p2<- ggplot(data = datclean[datclean$year == 2017,], aes(x = season, y = discharge))+
  geom_violin(fill = "blue")+
  theme_bw()+
  scale_y_continuous(limits = c(0,30))+
  labs(y= "Discharge", x = "2017", caption = "Data collected from Harbor Brook, Syracuse NY")
  


grid.arrange(p1, p2)


