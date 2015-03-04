library(dplyr)

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

mean1 <- data[!is.na(data$steps),] %>%
  group_by(date) %>%
  summarise_each_(funs(mean), vars = c("date","steps"))

totStepsPerDay <- data[!is.na(data$steps),] %>%
  group_by(date) %>%
  summarise_each_(funs(sum), vars = c("date","steps"))

medianStepsPerDay <- median(totStepsPerDay$steps)

hist(data$steps)

meanByInterval <- data[!is.na(data$steps),] %>%
  group_by(interval) %>%
  summarise_each_(funs(mean), vars = c("interval","steps"))

plot(meanByInterval, meanByInterval$interval, meanByInterval$steps, type="l", na.rm=T)

meanByInterval[meanByInterval$steps == max(meanByInterval$steps),]