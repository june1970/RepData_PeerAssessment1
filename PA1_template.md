---
Title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and clean data

1.Show any code that is needed to

Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis


```r
getwd()
```

```
## [1] "C:/Users/Administrator/Desktop/Coursera data science specification/Course 4 Exploratory data analysis/Project wk2/RepData_PeerAssessment1"
```

```r
 setwd('C:/Users/Administrator/Desktop/Coursera data science specification/Course 4 Exploratory data analysis/Project wk2/RepData_PeerAssessment1')
```

2.Loading the data


```r
Act<- read.csv("activity.csv")
```


Checking the data


```r
dim(Act)
```

```
## [1] 17568     3
```
3.Clean data without NA value

```r
cleanAct <- Act[ with (Act, { !(is.na(steps)) } ), ]

dim(cleanAct)
```

```
## [1] 15264     3
```

```r
str(cleanAct)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(cleanAct)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```
## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day


```r
stepsday <- with(Act, tapply(steps, as.factor(Act$date), sum, na.rm = T))
stepsday
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0        126      11352      12116      13294      15420      11015 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##          0      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##      10139      15084      13452      10056      11829      10395       8821 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##      13460       8918       8355       2492       6778      10119      11458 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##       5018       9819      15414          0      10600      10571          0 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##      10439       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336          0         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##       8841       4472      12787      20427      21194      14478      11834 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##      11162      13646      10183       7047          0
```

2.Make a histogram of the total number of steps taken each day

```r
hist(stepsday, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day

```r
summary(stepsday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```


## What is the average daily activity pattern?

1.Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ave_steps <- with(cleanAct, tapply(steps, cleanAct$interval, mean))
int<- levels(as.factor(cleanAct$interval))
plot(int, ave_steps, type = "l", main = "Time series plot of average daily activity", xlab = "interval", ylab = "Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
table <- data.frame(ave_steps, int)
table[table$ave_steps==max(table$ave_steps),][2]
```

```
##     int
## 835 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
missing_Act <- Act[is.na(Act$steps),]
dim(missing_Act)
```

```
## [1] 2304    3
```

```r
length(missing_Act$steps)
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. 
Strategy: using mean values to replace NA value


```r
ave_steps <- with(cleanAct, tapply(steps, cleanAct$interval, mean))
missing_Act$steps <- ave_steps
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newAct <- rbind(cleanAct, missing_Act)
newAct <- newAct[order(newAct$date), ]
dim(newAct)
```

```
## [1] 17568     3
```

```r
head(newAct)
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

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newstepsday <- with(newAct, tapply(steps, as.factor(newAct$date), sum))
hist(newstepsday, main = "Histogram of total steps taken per day", xlab = "Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


```r
summary(newstepsday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```


```r
summary(stepsday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
newAct$days <- weekdays(as.Date(newAct$date))
weekend<- grep("Saturday|Sunday", newAct$days, ignore.case = T)
weekendAct<-newAct[weekend, ]
weekendAct$weekday <- "weekend"
weekdayAct <- subset(newAct,newAct$days!=weekend)
```

```
## Warning in newAct$days != weekend: longer object length is not a multiple of
## shorter object length
```

```r
weekdayAct$weekday <- "weekday"
```

```r
head(weekdayAct)
```

```
##       steps       date interval   days weekday
## 1 1.7169811 2012-10-01        0 Monday weekday
## 2 0.3396226 2012-10-01        5 Monday weekday
## 3 0.1320755 2012-10-01       10 Monday weekday
## 4 0.1509434 2012-10-01       15 Monday weekday
## 5 0.0754717 2012-10-01       20 Monday weekday
## 6 2.0943396 2012-10-01       25 Monday weekday
```

```r
head(weekendAct)
```

```
##      steps       date interval     days weekday
## 1441     0 2012-10-06        0 Saturday weekend
## 1442     0 2012-10-06        5 Saturday weekend
## 1443     0 2012-10-06       10 Saturday weekend
## 1444     0 2012-10-06       15 Saturday weekend
## 1445     0 2012-10-06       20 Saturday weekend
## 1446     0 2012-10-06       25 Saturday weekend
```

```r
newAct2 <- rbind(weekdayAct, weekendAct)
head(newAct2)
```

```
##       steps       date interval   days weekday
## 1 1.7169811 2012-10-01        0 Monday weekday
## 2 0.3396226 2012-10-01        5 Monday weekday
## 3 0.1320755 2012-10-01       10 Monday weekday
## 4 0.1509434 2012-10-01       15 Monday weekday
## 5 0.0754717 2012-10-01       20 Monday weekday
## 6 2.0943396 2012-10-01       25 Monday weekday
```

2.Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
meansteps <- aggregate(steps~ interval+weekday, newAct2, mean)
g <- qplot(interval, steps, data = meansteps, facets = weekday~.)
g + geom_line(size = 1) + ylab("Mean steps") + ggtitle("Average steps taken, \n averaged across weekday days or weekend days ")
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
