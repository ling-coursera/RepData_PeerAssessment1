# Reproducible Research: Peer Assessment 1

This is a report analyzing the data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
First, unzip, load and preproces the activity monitoring data.

```r
unzip("./activity.zip")
act <- read.csv("./activity.csv")

## convert interval identifiers to POSIXct times
act$intervaltime <- as.POSIXct(strptime(sprintf("%04d", act$interval), "%H%M"))
```

## What is mean total number of steps taken per day?
Calculate the total number of steps taken each day.  Here, we ignore the missing values in the dataset.

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
daily <- group_by(act, date)
dailySum <- summarize(daily, dailySteps = sum(steps, na.rm=TRUE))
```

Here is the histogram of the total number of steps taken each day.

```r
hist(dailySum$dailySteps, xlab="Number of steps", main="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/dailyplot-1.png) 

Here are the mean and median total number of steps taken per day.

```r
mean(dailySum$dailySteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(dailySum$dailySteps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Calculate the average number of steps taken for each 5-minute interval, averaged across all days. Here, we ignore the missing values in the dataset.

```r
intervals <- group_by(act, intervaltime)
intervals <- summarize(intervals, steps=mean(steps, na.rm=TRUE))
```

Find out which 5-minute interval contains the maximum number of steps.

```r
ind <- which.max(intervals$step)
mInt <- intervals$intervaltime[ind]
mSteps <- intervals$steps[ind]
```

Here is the time series plot of the 5-minute interval and the average number of steps taken.

```r
plot(intervals$intervaltime, intervals$steps, type="l", 
     xlab="5-min intervals", ylab="Number of steps", main="Average daily activity pattern")

## Mark the interval with the maximum number of steps
points(mInt, mSteps, pch=19)
text(mInt, mSteps+5, "Max")
```

![](PA1_template_files/figure-html/intervalsplot-1.png) 

The interval 08:35 contains the maximum number of steps, 206.2.


## Imputing missing values
Calculate the total number of missing values in the dataset.

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

We will fill in a missing value in the dataset with the mean for that 5-minute interval, which we calculated earlier.  A new dataset is created that is equal to the original dataset but with the missing data filled in.

```r
act1 <- merge(act, intervals, by.x="intervaltime", by.y="intervaltime")
act1 <- transmute(act1, steps=ifelse(is.na(steps.x), steps.y, steps.x), 
                  date=date, intervaltime=intervaltime)
```

Calculate the total number of steps taken each day for the new dataset.

```r
daily <- group_by(act1, date)
dailySum <- summarize(daily, dailySteps = sum(steps, na.rm=FALSE))
```

Here is the histogram of the total number of steps taken each day for the new dataset.

```r
hist(dailySum$dailySteps, xlab="Number of steps", main="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/dailyplot1-1.png) 

Here are the mean and median total number of steps taken per day for the new dataset.

```r
mean(dailySum$dailySteps, na.rm=FALSE)
```

```
## [1] 10766.19
```

```r
median(dailySum$dailySteps, na.rm=FALSE)
```

```
## [1] 10766.19
```

Compare to the orginal dataset, the new dataset with missing data filled in results in fewer days with low numbers of steps taken. The mean and median are higher with the new dataset.  It is apparent that the missing values are concentrated in certain days, instead of spreading across most days.

## Are there differences in activity patterns between weekdays and weekends?
Calculate average number of steps taken for each 5-minute interval, averaged across all weekday days or weekend days.

```r
act1$weekday = factor(ifelse(weekdays(as.Date(act1$date)) %in% c("Saturday","Sunday"), 
                      "weekend", "weekday"))

intervals1 <- group_by(act1, intervaltime, weekday)
intervals1 <- summarize(intervals1, steps=mean(steps, na.rm=FALSE))
```

Here is the panel plot containing a time series plot of the 5-minute interval and the average number of steps taken.

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.3
```

```r
xyplot(intervals1$steps ~ intervals1$intervaltime | intervals1$weekday,
       scales=list(x=list(format="%H:%M")), type="l", layout=c(1,2),
       xlab="5-min Intervals", ylab="Number of steps")
```

![](PA1_template_files/figure-html/weekdaysplot-1.png) 
