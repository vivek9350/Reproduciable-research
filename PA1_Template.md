---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Here is my submission for the reproducible research peer assessment 1


```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.0.4
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.4
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv", na.strings="NA")
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

```r
steps.sum <- aggregate(x = activity["steps"],
                     FUN = sum,
                     by = list(Group.date = activity$date))
hist(steps.sum$steps,main="Total Number of Steps per Day Histogram",xlab="Total Number of Steps per Day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->



```r
mean <- mean(steps.sum$steps, na.rm=TRUE)
```

The mean number of steps is 1.0766189\times 10^{4}.

```r
median <-median(steps.sum$steps, na.rm=TRUE)
```
The median number of steps is 10765.

## What is the average daily activity pattern?


```r
steps.interval <- aggregate(x = activity["steps"],
                     FUN = mean,
                     by = list(Group.interval = activity$interval),
                     na.rm=TRUE)
plot(steps.interval$Group.interval, steps.interval$steps, type = "l", xlab="Interval", ylab="Average Steps")
abline(v=steps.interval$Group.interval[which.max(steps.interval$steps)], col="red")
```

![](PA1_Template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
interval<-steps.interval$Group.interval[which.max(steps.interval$steps)]
```

The 5-minute interval with the maximum number of steps is 835.

## Imputing missing values


```r
numberna <- sum(is.na(activity))
```

The total number of NA is 2304.


```r
new <- activity
for (i in steps.interval$Group.interval) {
  new[new$interval == i & is.na(new$steps), ]$steps <- steps.interval$steps[steps.interval$Group.interval == i]
}
sum(is.na(new))
```

```
## [1] 0
```



```r
newsteps.sum <- aggregate(x = new["steps"],
                     FUN = sum,
                     by = list(Group.date = new$date))
```



```r
hist(newsteps.sum$steps,main="Total Number of Steps per Day Histogram w/o NA",xlab="Total Number of Steps per Day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
newmean <- mean(newsteps.sum$steps, na.rm=TRUE)
```

The mean number of steps is 1.0766189\times 10^{4} when calculated without missing values.

```r
newmedian <-median(newsteps.sum$steps, na.rm=TRUE)
```
The median number of steps is 1.0766189\times 10^{4} when calculated without missing values.

Imputing missing data increases the total number of steps but does not impact the mean or the median. 

## Are there differences in activity patterns between weekdays and weekends?


```r
new$week <- ifelse(weekdays(new$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
```


```r
new.interval <- aggregate(x = new["steps"],
                     FUN = mean,
                     by = list(Group.interval = new$interval,Group.week=new$week),
                    )
new.interval$Group.week<-factor(new.interval$Group.week)
ggplot(data = new.interval)+
  aes(x=Group.interval,y= steps, group = Group.week, col = Group.week)+
  geom_line()+
  facet_grid(Group.week~.)+
  labs(title= "Average Steps Per Interval by Days",x="Interval", y="Average Steps") 
```

![](PA1_Template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
