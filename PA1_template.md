# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

This script and the dataset file `"activity.zip"` are located in the GitHub repository and therefore
you should use the folder contains these files as your working directory. If you want this script to work
in another folder, copy the dataset `"activity.zip"` to that folder and change this folder in the following `path`


```r
path <- "F:/UHM/Coursera/Data Science/5_Reproducible Research/CourseProject1" ## this path should be change accordingly to your working directory
setwd(path) ## set working directory
```

* unzip the dataset file


```r
zipFile <- "activity.zip"
unzip(zipFile)
```

* read the csv data


```r
data <- read.csv(file = "activity.csv", sep =",", header = TRUE)
```

```r
Sys.setlocale(category = "LC_TIME", locale = "US") ## set local date, change local if required
data$date <- as.Date(data$date) ## convert to date class type
```

* process data


```r
data.sumbydate <- aggregate(data$steps, by=list(data$date), FUN=sum) ## aggreagate total step each day
names(data.sumbydate) <- c("date", "sum")
```

## What is mean total number of steps taken per day?

* histogram of the total number of steps taken each day


```r
with(data.sumbydate, hist(sum, breaks = 10,
                          main = " Histogram of the total number of steps taken each day",
                          xlab = "Total number of steps taken each day"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

* mean total number of steps


```r
mean(data.sumbydate$sum, na.rm = TRUE)
```

```
## [1] 10766
```

* median total number of steps


```r
median(data.sumbydate$sum, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

* a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.interval.data <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm = TRUE) ## aggregate steps by interval, averaging across days
names(steps.interval.data) <- c("interval", "mean")
with(steps.interval.data, plot(interval, mean, type = "l",
                               main = " The number of steps taken per 5-minute interal, averaged across all days",
                               ylab = "averaged step"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# Interval 835 contains the maximum number of steps on average of 206.1698
steps.interval.data[which.max(steps.interval.data$mean),]
```

```
##     interval  mean
## 104      835 206.2
```

## Imputing missing values


```r
good <- !is.na(data$steps)
sum(!good) ## report the total number of misisng values in the dataset
```

```
## [1] 2304
```

```r
gooddata <- data[good,]
```


```r
gooddata.sumbydate <- aggregate(gooddata$steps, by=list(gooddata$date), FUN=sum)
names(gooddata.sumbydate) <- c("date", "sum")
```


```r
with(gooddata.sumbydate, hist(sum, breaks = 10,
                              main = " Histogram of the total number of steps taken each day",
                              xlab = "Total number of steps taken each day"))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


```r
mean(gooddata.sumbydate$sum, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(gooddata.sumbydate$sum, na.rm = TRUE)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

* create a new factor variable named `weekday` contains two values ("weekday" and "weekend")


```r
days <- weekdays(gooddata$date, abbreviate = TRUE) ## get weekdays (character-type like Mon for Monday)
gooddata$days <- days ## add new column to dataset
func <- function(x){ 
        if (x == "Sat" | x == "Sun") ("weekend") ## weekend?
        else ("weekday") ## weekday?
}
weekday <- lapply(gooddata$days, func)
gooddata$weekday <- paste(weekday)
gooddata <- transform(gooddata, weekday = factor(weekday)) ## transform weekday variable into factor variable
```

* a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
#install.packages("lattice")  ## install this package if required
library(lattice)
gooddata <- transform(gooddata, weekday = factor(weekday))
agg.gooddata <- aggregate(steps ~ interval + weekday, data = gooddata, FUN=mean)
xyplot(steps ~ interval | weekday, data = agg.gooddata, type = "l", layout = c(1,2))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 
