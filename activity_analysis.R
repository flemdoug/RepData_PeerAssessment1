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

meanByInterval[meanByInterval$steps == max(meanByInterval$steps),]

weekend <- data[wday(data$date, label=T) %in% c("Sat","Sun"),]
weekday <- data[!wday(data$date, label=T) %in% c("Sat","Sun"),]