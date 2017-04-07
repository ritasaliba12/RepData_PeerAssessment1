library(knitr)
---
title: "Reproducible Research"
author: "Rita Saliba"
date: "7 April 2017"
output: html_document
keep_md: true
---

## Loading and preprocessing the data

```r
activitydata <- read.csv("activity.csv")
activitydata$date<- as.Date(activitydata$date, format = "%Y-%m-%d") 
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(steps ~ date, data = activitydata, sum, na.rm = TRUE)
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, xlab="Total Number of Steps per day", ylab="Number of Days", main="Total Number of Steps taken each day", breaks = 16)
```

![plot of chunk histrogram1](figure/histrogram1-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps<-mean(steps_per_day$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps<-median(steps_per_day$steps)
median_steps
```

```
## [1] 10765
```
##What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averageStepsbyInterval<-aggregate(steps~interval, activitydata, mean, na.rm= TRUE)
with(averageStepsbyInterval, plot(interval, steps, type = "l"))
```

![plot of chunk timeseries](figure/timeseries-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageStepsbyInterval[which.max(averageStepsbyInterval[,2]),1]
```

```
## [1] 835
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
NA_count <- is.na(activitydata$steps)
```
There are 2304 missing values in the dataset.

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
fillmissingvalues <-mean(averageStepsbyInterval$steps)
```
Raplace missing values with 37.3825996

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity1<-activitydata
activity1[NA_count,1]<-fillmissingvalues
head(activity1)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsByDay1<-aggregate(steps~date, activity1, sum)
hist(totalStepsByDay1$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")
```

![plot of chunk totalStepsByDay1](figure/totalStepsByDay1-1.png)

```r
totalStepsByDay1<-aggregate(steps~date, activity1, sum)

mean_afterNAfill<-mean(totalStepsByDay1$steps)
mean_afterNAfill
```

```
## [1] 10766.19
```

```r
median_afterNAfill<-median(totalStepsByDay1$steps)
median_afterNAfill
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity1$date<- as.Date(activity1$date, format = "%Y-%m-%d")
#  indicating whether a given date is a weekday or weekend day.

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

activity1$day <- sapply(activity1$date, FUN=weekday.or.weekend)
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(ggplot2)

averages <- aggregate(steps ~ interval + day, data=activity1, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("Interval") + ylab("Number of steps") 
```

![plot of chunk panel](figure/panel-1.png)
