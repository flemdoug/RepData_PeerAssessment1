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
data <- mutate(data, dow = wday(date, label=T))

totStepsPerDay <- data[!is.na(data$steps),] %>%
  group_by(date) %>%
  #summarise_each_(funs(sum), vars = c("date","steps")) 
  summarise( totSteps = sum(steps))

medianStepsPerDay <- median(totStepsPerDay$totSteps)
meanStepsPerDay <- mean(totStepsPerDay$totSteps) 

meanByInterval <- data[!is.na(data$steps),] %>%
  group_by(interval) %>%
  summarise_each_(funs(mean), vars = c("interval","steps"))

meanByIntervalDayType <- data[!is.na(data$steps),] %>%
  group_by(interval, daytype) %>%
  summarise_each_(funs(mean), vars = c("interval","steps","daytype"))

meanByIntervalDOW <- data[!is.na(data$steps),] %>%
  group_by(interval, dow) %>%
  summarise_each_(funs(mean), vars = c("interval","steps","dow"))

dataDOW <- data
dataDateType <- data

for (i in unique(dataDateType$interval)) {
  dataDateType[is.na(dataDateType$steps) & dataDateType$interval == i & dataDateType$daytype == 'weekday',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == i & meanByIntervalDayType$daytype == 'weekday',]$steps,0)
  dataDateType[is.na(dataDateType$steps) & dataDateType$interval == i & dataDateType$daytype == 'weekend',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == i & meanByIntervalDayType$daytype == 'weekend',]$steps,0)
}


for (i in unique(dataDOW$interval)) {
  for (j in c("Mon","Tue","Wed","Thurs","Fri","Sat","Sun")) {
    dataDOW[is.na(dataDOW$steps) & dataDOW$interval == i & dataDOW$dow == j,]$steps <- round(meanByIntervalDOW[meanByIntervalDOW$interval == i & meanByIntervalDOW$dow == j, ]$steps,0)
  }
}

data$StepsIntDayType <- dataDateType$steps
data$StepsIntDOW <- dataDOW$steps
rm(dataDateType, dataDOW)

hist(data$steps)
with(meanByInterval,plot(interval, steps, type="l", ylab="Steps", xlab = "Interval"))

hist(data$StepsIntDayType)

