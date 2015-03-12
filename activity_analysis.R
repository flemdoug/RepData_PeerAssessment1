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
with(meanByInterval,plot(interval, steps, type="l", ylab="Steps", xlab = "Interval"))

meanByIntervalDayType <- data[!is.na(data$steps),] %>%
  group_by(interval, daytype) %>%
  summarise_each_(funs(mean), vars = c("interval","steps","daytype"))

meanByIntervalDOW <- data[!is.na(data$steps),] %>%
  group_by(interval, dow) %>%
  summarise_each_(funs(mean), vars = c("interval","steps","dow"))

dataDOW <- data
dataDateType <- data

mid <- meanByIntervalDOW
mid$steps <- as.integer(round(mid$steps,0))


data$StepsIntDOW <- apply(data,1, function(x) if( is.na(x[1])) round(mid[mid$interval == as.numeric(x[3]) & mid$dow ==  x[5],]$steps,0) else x[1][[1]][[1]])
#for (i in unique(dataDateType$interval)) {
#  dataDateType[is.na(dataDateType$steps) & dataDateType$interval == i & dataDateType$daytype == 'weekday',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == i & meanByIntervalDayType$daytype == 'weekday',]$steps,0)
#  dataDateType[is.na(dataDateType$steps) & dataDateType$interval == i & dataDateType$daytype == 'weekend',]$steps <- round(meanByIntervalDayType[meanByIntervalDayType$interval == i & meanByIntervalDayType$daytype == 'weekend',]$steps,0)
#}


#for (i in unique(dataDOW$interval)) {#
#  for (j in c("Mon","Tue","Wed","Thurs","Fri","Sat","Sun")) {
#    dataDOW[is.na(dataDOW$steps) & dataDOW$interval == i & dataDOW$dow == j,]$steps <- round(meanByIntervalDOW[meanByIntervalDOW$interval == i & meanByIntervalDOW$dow == j, ]$steps,0)
#  }
#}

#data$StepsIntDayType <- dataDateType$steps
#data$StepsIntDOW <- dataDOW$steps
#rm(dataDateType, dataDOW)

#hist(data$StepsIntDayType)

totStepsPerDayIntDOW <- data %>%
  group_by(date) %>% 
  summarise( totSteps = sum(StepsIntDOW))

medianStepsPerDayIntDOW <- median(totStepsPerDayIntDOW$totSteps)
meanStepsPerDayIntDOW <- mean(totStepsPerDayIntDOW$totSteps)
nNA <- nrow(data[is.na(data$steps),])


pActivity <- qplot(interval, steps, data=data, facets=daytype ~ ., geom="line", main="Weekend vs Weekday Activity (Includes NA)")
pActivityDOW <- qplot(interval, StepsIntDOW, data=data, facets=daytype ~ ., geom="line", main="Weekend vs Weekday Activity (NA replaced with Mean of steps by Interval, Day of Week)")
pActivityDayType <- qplot(interval, StepsIntDayType, data=data, facets=daytype ~ ., geom="line", main="Weekend vs Weekday Activity \n (NA replaced with Mean of steps by Interval and either weekend day or weekday day)")

par(mfrow=c(1,2))
print(pActivityDOW)
print(pActivityDayType)

pActivityDOW2 <- ggplot(data=data, aes(x=interval, y=StepsIntDOW)) + geom_line() + facet_grid(daytype ~ .)
pActivityDOW2 + ggtitle("Weekend vs Weekday Activity \n (NA replaced with Mean of steps by Interval, Day of Week)")
