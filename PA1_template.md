---
title: "PA1_template"
output: 
  html_document:
    keep_md: true
---





## Loading and preprocessing the data

```r
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activitymon.zip",method="curl")
unzip(zipfile="./data/activitymon.zip",exdir="./data")
activitymon<-read.csv("./data/activity.csv")
activitymon$date<-as.Date(activitymon$date,format="%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
#Calculate the total number of steps taken each day
totalsteps<-aggregate(steps~date,activitymon,sum)

#Histogram of the total number of steps taken each day
hist(totalsteps$steps, main = "Frequency of Total Number of Steps Taken Per Day", xlab = "Total Steps Taken Per Day",ylab="Frequency", breaks = seq(0,25000, by=2500), col="aquamarine")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Mean total number of steps taken per day
mean(totalsteps$steps)
```

```
## [1] 10766.19
```

```r
#Median of total number of steps taken per day
median(totalsteps$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) 
averagesteps<-aggregate(steps~interval,activitymon,mean)
plot(steps~interval,data=averagesteps,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
averagesteps[which.max(averagesteps$steps),]$interval
```

```
## [1] 835
```


## Imputing missing values

```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activitymon))
```

```
## [1] 2304
```

```r
#Devise a strategy for filling in all of the missing values in the dataset. Design a function that fill in all the missing values in the dataset with the mean per interval.
getMeanStepsPerInterval<-function(interval){
    averagesteps[averagesteps$interval==interval,]$steps
}

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
activitymonNoNA<- activitymon
for(i in 1:nrow(activitymonNoNA)){
    if(is.na(activitymonNoNA[i,]$steps)){
        activitymonNoNA[i,]$steps <- getMeanStepsPerInterval(activitymonNoNA[i,]$interval)
    }
}

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Calculate the total number of steps taken each day without NA
totalstepsnoNA<-aggregate(steps~date,activitymonNoNA,sum)

#Histogram of the total number of steps taken each day
hist(totalstepsnoNA$steps, main = "Frequency of Total Number of Steps Taken Per Day Without NA", xlab = "Total Steps Taken Per Day Without NA",ylab="Frequency", breaks = seq(0,25000, by=2500), col="cyan")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Mean total number of steps taken per day without NA
mean(totalstepsnoNA$steps)
```

```
## [1] 10766.19
```

```r
#Median of total number of steps taken per day without NA
median(totalstepsnoNA$steps)
```

```
## [1] 10766.19
```

```r
#The mean didn’t change after the replacements of NAs, the median changed about 0.1% of the original value.
```


## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activitymonNoNA$type <- factor(ifelse (weekdays(activitymonNoNA$date) %in% c("Saturday","Sunday"),"Weekend", "Weekday"))

#Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
library(lattice)

stepspatternbytype <- aggregate(steps ~ interval + type, activitymonNoNA, mean, na.action = na.omit)

xyplot(steps ~ interval | type, 
       data = stepspatternbytype, 
       type = "l", 
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
