library(dplyr)
library(lubridate)

dataDir <- "./data"
activityFile <- paste(dataDir,"/activity.zip", sep="");

if (!file.exists(dataDir)) {
  dir.create(file.path(dataDir))
}

if (!file.exists(activityFile)) {
  activityFile <- paste(dataDir,"/activity.zip", sep="");
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

d2<- left_join(meanByIntervalDayType, data, by=c("interval","daytype"))
data[is.na(data$steps),]$steps <- d2[is.na(d2$steps.y),]$steps.x







