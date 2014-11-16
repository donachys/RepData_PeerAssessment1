# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
data <- read.csv("activity.csv", sep=",", stringsAsFactors=F)
data$date <- as.Date(data$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
step <- aggregate(formula=steps ~ date, data=data, FUN=sum)
plot(step, type="h")
```

![](./PA1_template_files/figure-html/hist-1.png) 



```r
mean(step$steps, na.rm=T)
```

```
## [1] 10766.19
```


```r
median(step$steps, na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?  

```r
step2 <- aggregate(formula=steps ~ interval, data=data, FUN=mean)  
plot(step2, type="l")  
```

![](./PA1_template_files/figure-html/line-1.png) 

```r
maxAvgStepInterval = step2$interval[step2$steps==max(step2$steps)]
maxAvgStepInterval
```

```
## [1] 835
```

## Imputing missing values


```r
nrow(data[is.na(data$steps)==T,])
```

```
## [1] 2304
```

```r
data2 <- read.csv("activity.csv", sep=",", stringsAsFactors=F)
data2$date <- as.Date(data$date, format="%Y-%m-%d")

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
data2 <- ddply(data2, ~ interval, transform, steps = impute.mean(steps))
```

```r
step2 <- aggregate(formula=steps ~ date, data=data2, FUN=sum)
plot(step, type="h")
```

![](./PA1_template_files/figure-html/hist2-1.png) 

```r
mean(step2$steps, na.rm=T)
```

```
## [1] 10766.19
```


```r
median(step2$steps, na.rm=T)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
isWeekday <- function(x){ if( x == "Saturday" || x == "Sunday") "weekday" else "weekend"}
daytype <- sapply(weekdays(data2$date), isWeekday)
data3 <- cbind(data2, daytype)
```

```r
step3 <- aggregate(formula=steps ~ interval + daytype, data=data3, FUN=mean)
step4 <- step3[step3$daytype=="weekday",]
step5 <- step3[step3$daytype=="weekend",]

par(mfrow=c(2,1))
plot(step4$interval, step4$steps, type="l", xlab="interval", ylab="steps", sub="weekdays")
plot(step5$interval, step5$steps, type="l", xlab="interval", ylab="steps", sub="weekends")
```

![](./PA1_template_files/figure-html/diffs2-1.png) 

