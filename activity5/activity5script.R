#activity 5 code
#2/24

#load in lubridate
library(lubridate)

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

noleap <- seq(1:365)
leap <- seq(1:366)
years <- seq(2007,2013)

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

