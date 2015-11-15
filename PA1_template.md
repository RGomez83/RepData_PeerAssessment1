# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}

data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
TotalStepsDay <- aggregate(data$steps, list(data$date), sum, na.rm=TRUE)

names(TotalStepsDay) <- c("date", "total")

head(TotalStepsDay)
```

```
##         date total
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(TotalStepsDay$total)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(TotalStepsDay$total)
```

```
## [1] 9354.23
```

```r
median(TotalStepsDay$total)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
meanStepsInterval <- aggregate(data$steps, list(data$interval), mean, na.rm=TRUE)
  
names(meanStepsInterval) <- c("interval", "mean")

plot(meanStepsInterval$interval,meanStepsInterval$mean,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
meanStepsInterval$interval[which.max(meanStepsInterval$mean)]
```

```
## [1] 835
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. I use the mean for that 5-minute interval.


```r
for (i in 1:length(data$steps)){
    if (is.na(data$steps[i])){
      
      int_pos <- data[i,3]
      
      data$steps[i] <- meanStepsInterval$mean[meanStepsInterval$interval == int_pos]
      
    }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
head(data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
TotalStepsDay <- aggregate(data$steps, list(data$date), sum, na.rm=TRUE)

names(TotalStepsDay) <- c("date", "total")

hist(TotalStepsDay$total)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Now the mean and median are:


```r
mean(TotalStepsDay$total)
```

```
## [1] 10766.19
```

```r
median(TotalStepsDay$total)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
##Add a column of weekdays
data <- cbind(data,weekdays(as.Date(data$date)))

names(data) <-  c("steps", "date", "interval", "weekday")

##Create a vector of day types
day_type <- NULL

for(i in 1:length(data$weekday)){
  
  if(data$weekday[i] == "Saturday" || data$weekday[i] == "Sunday"){
    
    day_type[i] <- c("weekend")
    
  }
  else day_type[i] <- c("weekday")
  
}
##Add a column of day types
data <- cbind(data,day_type)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_interval<- aggregate(steps ~ interval + day_type, data, mean)

library(lattice)

xyplot(steps_interval$steps ~ steps_interval$interval|steps_interval$day_type,layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 





