---
title: "Reproducible Research - Week 2 Assignment"
author: "Ashley Campisano"
date: "6/23/2019"
output: 
  html_document: 
    keep_md: yes
---



###Reproducible Data - Week 2 Assignment


```r
#load packages that may be needed during the assignment
library(ggplot2)
```

```
## Registered S3 methods overwritten by 'ggplot2':
##   method         from 
##   [.quosures     rlang
##   c.quosures     rlang
##   print.quosures rlang
```

```r
library(dplyr)
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
library(tidyr)
library(knitr)
library(lattice)
```

```r
#load the data
activitydat <- read.csv("/Users/ashleycampisano/Downloads/activity.csv")
opts_chunk$set(echo=TRUE)
```

```r
#explore the data
head(activitydat)
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

```r
tail(activitydat)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
summary(activitydat)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(activitydat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#Based on initial exploration of the data, it appears that there is a fair amount of NA's (2304).
```

```r
#create subset of data without NAs
activitydat_nona <- na.omit(activitydat)
```

```r
##What is the mean total number of steps taken per day?

#1.Calculate the total number of steps taken per day
dailyact <- group_by(activitydat_nona, date)
dailyact <- summarize(dailyact, steps=sum(steps))

head(dailyact)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <fct>      <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
summary(dailyact)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
#2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
ggplot(dailyact, aes(steps)) + 
  geom_histogram(fill = "blue") +
  labs(title = "Mean Total Number of Daily Steps", x = "Number of Steps")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](figs/fig-unnamed-chunk-6-1.png)<!-- -->

```r
#3.Calculate and report the mean and median of the total number of steps taken per day

summary(dailyact)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
#The mean of total number of steps taken per day is 10,776 steps and the median is 10,765 steps taken per day.

##What is the average daily pattern?

#1.Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

avgdailypat <- aggregate(steps ~ interval, data = activitydat_nona, FUN = mean)
plot(avgdailypat, type = "l", main = "Average Daily Pattern Time Series")
```

![](figs/fig-unnamed-chunk-7-1.png)<!-- -->

```r
#2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

avgdailypat[avgdailypat$steps==max(avgdailypat$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
#Time interval 835 contains the max number of steps (206.1698).
```

```r
##Imputing Missing Values

#1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

summary(activitydat)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
#There are 2304 total missing values in the data set.
```

```r
#2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#The strategy I chose to fill in all of the missing values in the dataset is to set any NA values to the mean for that particular 5 minute interval.

#Merge activity interval data with original activity data

names(avgdailypat)[2] <- "Mean_Steps"
actmerge <- merge(activitydat, avgdailypat)
```

```r
#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

#Replace NAs with interval mean

actmerge$steps[is.na(actmerge$steps)] <- actmerge$Mean_Steps[is.na(actmerge$steps)]
```

```r
#4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Create new dataset w/total number of steps taken each day

actmergedaily <- group_by(actmerge, date)
actmergedaily <- summarize(actmergedaily, steps=sum(steps))

qplot(steps, data = actmergedaily)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](figs/fig-unnamed-chunk-12-1.png)<!-- -->

```r
#Mean and Median Total Number

mean(actmergedaily$steps)
```

```
## [1] 10766.19
```

```r
median(actmergedaily$steps)
```

```
## [1] 10766.19
```

```r
#The mean does not appear to be affected by the missing data imputation. The median is slightly smaller.
```

```r
##Are there differences in activity patterns between weekdays and weekends?

#1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

#Convert date variable to date class and use "weekday" function to assign the day of the week to each date. Create indicator variable to show whether day is weekday or weekend.

actmerge$dayofweek <- weekdays(as.Date(actmerge$date))
actmerge$weekend <-as.factor(actmerge$dayofweek=="Saturday"|actmerge$dayofweek=="Sunday")
levels(actmerge$weekend) <- c("Weekday", "Weekend")
```

```r
#2.Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

#Create subset for weekdays and weekends

act_weekday <- actmerge[actmerge$weekend=="Weekday",]
act_weekend <- actmerge[actmerge$weekend=="Weekend",]

#Find mean steps across each 5 minute interval

actint_weekday <- group_by(act_weekday, interval)
actint_weekday <- summarize(actint_weekday, steps=mean(steps))
actint_weekday$weekend <- "Weekday"
actint_weekend <- group_by(act_weekend, interval)
actint_weekend <- summarize(actint_weekend, steps=mean(steps))
actint_weekend$weekend <- "Weekend"

#Bind Weekday and Weekend Subsets together

actint <- rbind(actint_weekday, actint_weekend)
actint$weekend <- as.factor(actint$weekend)
ggplot(actint, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)
```

![](figs/fig-unnamed-chunk-14-1.png)<!-- -->

