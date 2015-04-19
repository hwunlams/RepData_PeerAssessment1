# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip(zipfile="activity.zip")
Activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

First the total number of steps taken per day is calculted across all dates and plotted.

```r
Totalstpperdate <- aggregate(steps ~ date, Activity, sum)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
qplot(Totalstpperdate$steps, binwindth = 1000, xlab = "Total numbers of steps taken each day")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

And then the mean and median values of these total numbers are calculted.

```r
summary(Totalstpperdate$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```
The mean of total number of steps taken per day is 10770 and the median is 10760. They are very close to each other.

## What is the average daily activity pattern?

The average number of steps taken per each 5 minutes interval is calculated.

```r
averagestpperinterval <- aggregate(x=Activity$steps, by = list(interval = Activity$interval), mean, na.rm=TRUE)
colnames(averagestpperinterval) = c("interval", "steps")
```

The plot for the average number of steps per interval is:

```r
ggplot(data=averagestpperinterval, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Across all dates, the 5-minute interval contas the maximum number of steps is the 835th interval with 206.1698 steps.

```r
averagestpperinterval[which.max(averagestpperinterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
missing <- is.na(Activity$steps)
# How many missing
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```
2304 intervals are missing values of steps.
All the missing values will be filled with mean value for all respective 5-minute intervals.

```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averagestpperinterval[averagestpperinterval$interval==interval, "steps"])
    return(filled)
}
filleddata <- Activity
filleddata$steps <- mapply(fill.value, filleddata$steps, filleddata$interval)
```
With the new data, the total number of steps taken per day is calculated and plotted.

```r
Totalstpperdatefilled <- aggregate(steps ~ date, filleddata, sum)
library(ggplot2)
qplot(Totalstpperdatefilled$steps, binwindth = 1000, xlab = "Total numbers of steps taken each day")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

And then the mean and median values of these total numbers are calculted.

```r
summary(Totalstpperdatefilled$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
The mean and median are the same now due to adding the missing values for some intervals. Mean went higher and median stayed the same comparing the filled dataset with the original dataset.

## Are there differences in activity patterns between weekdays and weekends?
First we determine whether it is a weekday or not for all intervals.

```r
weekdayindicator <- function(date){
  day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filleddata$date <- as.Date(filleddata$date)
filleddata$weekdayind <- sapply(filleddata$date, FUN=weekdayindicator)
```
Then the mean total numer of steps taken per interval are calculated and plotted respectively for weekdays and weekends.

```r
averagestpperintervalweekdayind <- aggregate(steps ~ interval + weekdayind, data=filleddata, mean)
ggplot(averagestpperintervalweekdayind, aes(interval, steps)) + geom_line() + facet_grid(weekdayind ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
