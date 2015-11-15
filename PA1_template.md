# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis

```r
    unzip("activity.zip")
    activity <- read.csv("activity.csv")
    activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
    library(plyr)
    library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
    library(ggplot2)
    library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
## 
## The following object is masked from 'package:plyr':
## 
##     here
```

```r
    library(gridExtra)
    daySums <- summarize(group_by(activity, date), steps = sum(steps))
    qplot(date, steps, data = daySums, geom = "bar", stat = "identity")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
    mean(daySums$steps)
```

```
## [1] NA
```

```r
    median(daySums$steps)
```

```
## [1] NA
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
    intervalMeans <- summarize(group_by(activity, interval), mean = mean(steps, na.rm = TRUE))
    qplot(interval, mean, data = intervalMeans, geom = "line", stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
    which.max(intervalMeans$mean)
```

```
## [1] 104
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
    dim(activity)[1] - sum(complete.cases(activity))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

###*Lets impute mean interval values*

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
    noNaActivity <- activity
    impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    noNaActivity <- ddply(noNaActivity, ~ interval, transform, steps = impute.mean(steps))
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
    daySums2 <- summarize(group_by(noNaActivity, date), steps = sum(steps))
    qplot(date, steps, data = daySums2, geom = "bar", stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
    mean(daySums2$steps)
```

```
## [1] 10766.19
```

```r
    median(daySums2$steps)
```

```
## [1] 10766.19
```

```r
    mean(daySums$steps, na.rm = TRUE) - mean(daySums2$steps)
```

```
## [1] 0
```

```r
    median(daySums$steps, na.rm = TRUE) - median(daySums2$steps)
```

```
## [1] -1.188679
```

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
    noNaActivity <- mutate(noNaActivity, dayType = ifelse(wday(noNaActivity$date, label = TRUE) %in% c("Sun", "Sat"), "Weekend", "Weekday"))
    noNaActivity$dayType <- as.factor(noNaActivity$dayType)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
    weekdayActivity <- filter(noNaActivity, dayType == "Weekday")
    weekendActivity <- filter(noNaActivity, dayType == "Weekend")
    
    weekdayIntervalMeans <- summarize(group_by(weekdayActivity, interval), mean = mean(steps))
    weekendIntervalMeans <- summarize(group_by(weekendActivity, interval), mean = mean(steps))
    
    p1 <- qplot(interval, mean, data = weekdayIntervalMeans, geom = "line", stat = "identity", main = "weekdays")
    p2 <- qplot(interval, mean, data = weekendIntervalMeans, geom = "line", stat = "identity", main = "weekends")
    grid.arrange(p1, p2, nrow = 2, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
