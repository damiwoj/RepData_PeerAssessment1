---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Read the data from *activity.csv* file using prespecified column data types. 

```r
data = read.csv('activity.csv', colClasses = c('numeric','Date','numeric'))
```

## What is mean total number of steps taken per day?

Let's start with a histogram of total number of steps taken each day; it shows the
distribution of number of steps.

```r
stepsPerDay = tapply(data$steps, data$date, sum, na.rm=T)
hist(stepsPerDay, xlab = 'Total number of steps', main='')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Let's compute basic statistics of total number of steps per day.

```r
stepsPerDay_mean = mean(stepsPerDay)
stepsPerDay_median = median(stepsPerDay)
```
They are as follows:  

- mean: 9354.23  
- median: 10395.00  

## What is the average daily activity pattern?

Let's plot an average daily pattern of steps taken.

```r
stepsDailyPattern = tapply(data$steps, data$interval, mean, na.rm=T)
plot(stepsDailyPattern, type = 'l', xlab = '5-minute intervals', 
     ylab = 'Average number of steps taken', main='')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

The maximum number of steps, on average across all the days in the dataset, is taken during 5-minute interval whose identifier is as follows:

```r
names(which.max(stepsDailyPattern))
```

```
## [1] "835"
```

## Imputing missing values

There are a number of day/intervals with missing values; total number of rows with missing values is as follows:

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

To fill in all the missing values, I replace them with the mean of that 5-minute interval. I assume that the order of the 5-miute interval identifies is the same for each day and it is ordered by time (as the original data are).

```r
dataNew = data
numberOfDays = length(unique(data$date))
dataNew$steps[is.na(dataNew$step)] = rep(stepsDailyPattern, numberOfDays)[is.na(dataNew$step)]
```

Let's redo a histogram of total number of steps taken each day using new data

```r
stepsPerDayNew = tapply(dataNew$steps, dataNew$date, sum)
hist(stepsPerDayNew, xlab = 'Total number of steps', main='')
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

and re-compute basic statistics (mean and media) of total number of steps per day:

```r
mean(stepsPerDayNew)
```

```
## [1] 10766.19
```

```r
median(stepsPerDayNew)
```

```
## [1] 10766.19
```

As it can be seen, the mean and median changed as compared to the original data. The missing values decreased the values of these basic statistics, as these missing values gave 'zero' to the total sum and thus decreased the total number of steps per day.

## Are there differences in activity patterns between weekdays and weekends?

Let's start with creating a new factor variable with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data$weekDayFactor = factor(weekdays(data$date) %in% c('Saturday','Sunday'),
                    levels = c(TRUE, FALSE),
                    labels = c('weekend', 'weekday'))
```

Let's plot an average daily pattern of steps taken for weekdays and weekends separately.


```r
stepsDailyPatternWeekday = sapply(c('weekday', 'weekend'), function(day)
    tapply(data$steps[data$weekDayFactor == day],
           data$interval[data$weekDayFactor == day], 
           mean, na.rm=T)
    )
par(mfrow = c(2,1))
plot(stepsDailyPatternWeekday[,1], type = 'l', xlab = '5-minute intervals', 
     ylab = 'Average number of steps taken', main='weekday')
plot(stepsDailyPatternWeekday[,2], type = 'l', xlab = '5-minute intervals', 
     ylab = 'Average number of steps taken', main='weekend')
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 



