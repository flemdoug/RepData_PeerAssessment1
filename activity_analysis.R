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

meanByIntervalDayType <- data[!is.na(data$steps),] %>%
  group_by(interval, daytype) %>%
  summarise_each_(funs(mean), vars = c("interval","steps","daytype"))

meanByIntervalDayType$steps <- meanByIntervalDayType$steps

for ( i in unique(data$interval)) {
  data[is.na(data$steps) & data$interval == i & data$daytype == 'weekday',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == i & meanByIntervalDayType$daytype == 'weekday',]$steps,0)
}

for ( i in unique(data$interval)) {
  data[is.na(data$steps) & data$interval == i & data$daytype == 'weekend',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == i & meanByIntervalDayType$daytype == 'weekend',]$steps,0)
}





