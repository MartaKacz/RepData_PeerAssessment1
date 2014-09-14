# Reproducible Research: Peer Assessment 1
Marta Kaczmarz  
Sunday, September 14, 2014  


This is a report (R mark document) that answers the questions from Peer Assessment 1. 


## Loading and preprocessing the data

To load the dataset stored in a comma-separated-value (CSV) file you can use ```read.csv()``` function:

```r
act.data <- read.csv('activity.csv', sep = ",")
```
Dataset has 17,568 observations and 3 variables. I also use function ```head()``` to see the first parts of a data frame or function:

```r
head(act.data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
In my opinion everything looks correct so data do not need transformation.

## What is mean total number of steps taken per day?

To answer to the question: *What is mean total number of steps taken per day?* first I prepared the data to make a histogram of the total number of steps taken each day:

```r
tot.no.steps <- tapply(act.data$steps,act.data$date, sum , na.rm = TRUE)
```
and then plot a histogram:

```r
hist(tot.no.steps, main = "Hisogram of the total number of steps", 
      xlab = "number of steps taken each day", col = "blue")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

Then I calculated the mean and median total number of steps taken per day:

```r
mean(tot.no.steps, na.rm = "TRUE")
```

```
## [1] 9354
```

```r
median(tot.no.steps, na.rm = "TRUE")
```

```
## [1] 10395
```

## What is the average daily activity pattern?

I made a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days with R package ```ggplot2```:

```r
avg.steps <- tapply(act.data$steps,act.data$interval, mean , na.rm = TRUE)
avg.steps <- data.frame(interval = as.numeric(names(avg.steps)),mean.steps = as.vector(avg.steps))

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.0.2
```

```r
ggplot(avg.steps,aes(interval ,mean.steps)) + geom_line()+
  xlab('5-minute interval') +
  ylab('avg. number of steps')
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 
  
Next
I tried to answer to the qestion *Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*:

```r
avg.steps[which.max(avg.steps$mean.steps),]
```

```
##     interval mean.steps
## 104      835      206.2
```
So the answer is 835.

## Imputing missing values

The total number of missing values in the dataset:

```r
sapply(act.data, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

The other options are: ```apply()``` or ```colSums()``` functions:

```r
apply(is.na(act.data),2,sum)
```

```
##    steps     date interval 
##     2304        0        0
```

```r
colSums(is.na(act.data))
```

```
##    steps     date interval 
##     2304        0        0
```

I used a ceiling of the mean for that 5-minute interval to filling in all of the missing values in the dataset:

```r
merged.data <- merge(act.data, avg.steps[,],  by.x = "interval", by.y = "interval", sort = FALSE)
new.data <- merged.data[order(merged.data[,3], merged.data[,2],merged.data[,1]), ]
```

The next step was to create a new dataset that is equal to the original dataset but with the missing data filled in:

```r
new.data$steps[is.na(new.data[,2])] <- ceiling(new.data$mean.steps[is.na(new.data[,2])])

head(new.data)
```

```
##     interval steps       date mean.steps
## 1          0     2 2012-10-01    1.71698
## 63         5     1 2012-10-01    0.33962
## 128       10     1 2012-10-01    0.13208
## 205       15     1 2012-10-01    0.15094
## 264       20     1 2012-10-01    0.07547
## 327       25     3 2012-10-01    2.09434
```

I made again a histogram of the total number of steps taken each day and cCalculated the mean and median total number of steps taken per day for filling data:


```r
tot.no.steps.new <- tapply(new.data$steps, new.data$date, sum , na.rm = TRUE)

hist(tot.no.steps.new, main = "Hisogram of the total number of steps", 
      xlab = "number of steps taken each day", col = "blue")
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 

Mean and median total number of steps taken per day:

```r
mean(tot.no.steps.new, na.rm = "TRUE")
```

```
## [1] 10785
```

```r
median(tot.no.steps.new, na.rm = "TRUE")
```

```
## [1] 10909
```

These values are differ from the estimates from the first part of the assignment, to make better compare:

```r
data.frame(Na.filling = c("NO", "YES"),
            mean = c(mean(tot.no.steps, na.rm = "TRUE"),mean(tot.no.steps.new, na.rm = "TRUE")),
            median = c(median(tot.no.steps, na.rm = "TRUE"),median(tot.no.steps.new, na.rm = "TRUE")) 
           )
```

```
##   Na.filling  mean median
## 1         NO  9354  10395
## 2        YES 10785  10909
```
Imputing missing data on the estimates of the total daily number of steps cause increasing of mean and median this value.


## Are there differences in activity patterns between weekdays and weekends?

I created a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:

```r
new.data$weekdays <- weekdays(as.Date(new.data$date))
new.data$weekdays[new.data$weekdays %in% c("niedziela", "sobota")]<- "weekend"
new.data$weekdays[!new.data$weekdays %in% c("weekend")] <- "weekday"

head(new.data)
```

```
##     interval steps       date mean.steps weekdays
## 1          0     2 2012-10-01    1.71698  weekday
## 63         5     1 2012-10-01    0.33962  weekday
## 128       10     1 2012-10-01    0.13208  weekday
## 205       15     1 2012-10-01    0.15094  weekday
## 264       20     1 2012-10-01    0.07547  weekday
## 327       25     3 2012-10-01    2.09434  weekday
```

```r
table(new.data$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

To make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days, first I prepered a data separately for "weekday" and "weekend":


```r
avg.steps.weekday <- tapply(new.data$steps[new.data$weekdays == "weekday"],new.data$interval[new.data$weekdays == "weekday"], mean)
avg.steps.weekday <- data.frame(interval = as.numeric(names(avg.steps.weekday)),mean.steps = as.vector(avg.steps.weekday))

avg.steps.weekend <- tapply(new.data$steps[new.data$weekdays != "weekday"],new.data$interval[new.data$weekdays != "weekday"], mean)
avg.steps.weekend <- data.frame(interval = as.numeric(names(avg.steps.weekend)),mean.steps = as.vector(avg.steps.weekend))
```

Finaly plot:


```r
par(mfrow = c(2,1))
plot(avg.steps.weekday, type = "l", main = "weekday", ylab = "Number of steps")
plot(avg.steps.weekend, type = "l", main = "weekend",ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-17](./PA1_template_files/figure-html/unnamed-chunk-17.png) 
