library(dplyr)
library(lubridate)
library(ggplot2)

dataDir <- "./data"
activityFile <- paste(dataDir,"/repdata_data_activity.zip", sep="");

if (!file.exists(dataDir)) {
  dir.create(file.path(dataDir))
}

if (!file.exists(activityFile)) {
  activityFile <- paste(dataDir,"/repdata_data_activity.zip", sep="");
  if (.Platform$OS.type == "windows") {
    setInternet2(TRUE)
  }
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", activityFile)
}

data <- read.csv(unz(activityFile,"activity.csv"))
data <- mutate(data, daytype = ifelse(wday(date, label=T) %in% c("Sat","Sun"),'Weekend','Weekday'))
data <- mutate(data, dow = wday(date, label=T))

totStepsPerDay <- data[!is.na(data$steps),] %>%
  group_by(date) %>% 
  summarise( totSteps = sum(steps))

medianStepsPerDay <- median(totStepsPerDay$totSteps)
meanStepsPerDay <- mean(totStepsPerDay$totSteps) 

meanByInterval <- data[!is.na(data$steps),] %>%
  group_by(interval) %>%
  summarise_each_(funs(mean), vars = c("interval","steps"))

hist(data$steps)
pHistSteps <- ggplot(data=totStepsPerDay, aes(x=totSteps)) + 
                geom_histogram()
with(meanByInterval,plot(interval, steps, type="l", ylab="Steps", xlab = "Interval"))

meanByIntervalDOW <- data[!is.na(data$steps),] %>%
  group_by(interval, dow) %>%
  summarise_each_(funs(mean), vars = c("interval","steps","dow"))

mid <- meanByIntervalDOW
mid$steps <- as.integer(round(mid$steps,0))

data$StepsIntDOW <- as.numeric(apply(data,1, function(x) if( is.na(x[1])) round(mid[mid$interval == as.numeric(x[3]) & mid$dow ==  x[5],]$steps,0) else x[1][[1]][[1]]))

totStepsPerDayIntDOW <- data %>%
  group_by(date) %>% 
  summarise( totSteps = sum(StepsIntDOW))

pHistStepsNA <- ggplot(data=totStepsPerDayIntDOW, aes(x=totSteps)) + 
  geom_histogram()

medianStepsPerDayIntDOW <- median(totStepsPerDayIntDOW$totSteps)
meanStepsPerDayIntDOW <- mean(totStepsPerDayIntDOW$totSteps)
nNA <- nrow(data[is.na(data$steps),])


pActivityRAW <- ggplot(data=data, aes(x=interval, y=steps)) + geom_line() + facet_grid(daytype ~ .)
pActivityRAW + ggtitle("Weekend vs Weekday Activity \n (NA not removed)")

pActivityDOW <- ggplot(data=data, aes(x=interval, y=StepsIntDOW)) + geom_line() + facet_grid(daytype ~ .)
pActivityDOW + ggtitle("Weekend vs Weekday Activity \n (NA replaced with Mean of steps by Interval, Day of Week)")

par(mfrow=c(1,2))
print(pActivityRAW)
print(pActivityRAW)