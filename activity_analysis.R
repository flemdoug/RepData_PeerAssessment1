library(dplyr)
library(lubridate)

dataDir <- "./data"
activityFile <- paste(dataDir,"/repdata_data_activity.zip", sep="");

if (!file.exists(dataDir)) {
  dir.create(file.path(dataDir))
}

if (!file.exists(activityFile)) {
  activityFile <- paste(dataDir,"/repdata_data_activity.zip", sep="");
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", activityFile)
}

data <- read.csv(unz(activityFile,"activity.csv"))
data <- mutate(data, daytype = ifelse(wday(date, label=T) %in% c("Sat","Sun"),'weekend','weekday'))

totStepsPerDay <- data[!is.na(data$steps),] %>%
  group_by(date) %>%
  #summarise_each_(funs(sum), vars = c("date","steps")) 
  summarise( totSteps = sum(steps))

medianStepsPerDay <- median(totStepsPerDay$totSteps)
meanStepsPerDay <- mean(totStepsPerDay$totSteps) 

hist(data$steps)

meanByInterval <- data[!is.na(data$steps),] %>%
  group_by(interval) %>%
  summarise_each_(funs(mean), vars = c("interval","steps"))

with(meanByInterval,plot(interval, steps, type="l", ylab="Steps", xlab = "Interval"))

#handle NA update with mutate or sqldf update?
# mutate(dat, dist = ifelse(speed == 4, dist * 100, dist)
#update NA to mean by weekday, interval 

meanByIntervalDayType <- data[!is.na(data$steps),] %>%
  group_by(interval, daytype) %>%
  summarise_each_(funs(mean), vars = c("interval","steps","daytype"))

meanByIntervalDayType$steps <- meanByIntervalDayType$steps


#d2<- left_join(meanByIntervalDayType, data, by=c("interval","daytype"))
#dataAll <- data
#dataAll[is.na(dataAll$steps),]$steps <- round(d2[is.na(d2$steps.y),]$steps.x,0)
data[is.na(data$steps) & data$interval == 5 & data$daytype == 'weekday',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == 5 & meanByIntervalDayType$daytype == 'weekday',]$steps,0)
data[is.na(data$steps) & data$interval == 130 & data$daytype == 'weekday',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == 130 & meanByIntervalDayType$daytype == 'weekday',]$steps,0)





