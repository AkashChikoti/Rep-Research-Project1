---
title: "Reproducible Research Project 1"
author: "Akash Gupta Chikoti"
date: "8/8/2020"
output: html_document
---

# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

# Data for analysis

The data for this assignment can be downloaded from the course web site:

- **Dataset** : [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

- **steps** : Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
- **date** : The date on which the measurement was taken in YYYY-MM-DD format
- **interval** : Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# Loading and processing the data

### Loading dependencies

Before loading the data, load the libraries that are going to be used for the analysis. While `dplyr` is used for data manipulation, `lattice` is used to generate panel plots.


```r
library(dplyr)
library(lattice)
```

### Loading data

##### Steps taken
1. Download the data from the given URL (embedded in the Dataset name) to the working directory
2. Data is unzipped to extract the CSV file
3. Extracted CSV file is read into R environment


```r
setwd("C:/Users/Abhinav/Desktop/Data Science/Reproducible Research/Week2 Project")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "repdata_data_activity.zip")
unzip("repdata_data_activity.zip")
activity <- read.csv("activity.csv", na.strings = NA)
```

### Processing data

Converting the **date** column from character to date format


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

# Problem statements

### 1. What is mean total number of steps taken per day?

Note : Missing values in the number of steps are ignored for this analysis

##### Distribution of total number of steps taken per day
1. Total number of steps taken per day is calculated using the given data
2. Check the distribution of the total number of steps using histogram


```r
daily_steps <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps))

hist(daily_steps$total_steps, xlab = "# Steps", ylab = "Frequency", 
     main = "Total number of steps taken per day")
```

![plot of chunk total_steps_day](figure/total_steps_day-1.png)

##### Checking central tendencies (mean and median)


```r
mean_daily_steps <- as.integer(mean(daily_steps$total_steps))
median_daily_steps <- as.integer(median(daily_steps$total_steps))
c(mean = mean_daily_steps, median = median_daily_steps)
```

```
##   mean median 
##  10766  10765
```

The average number of steps taken each day was 10766 steps.
The median number of steps taken each day was 10765 steps.

### 2. What is the average daily activity pattern?

Note : Missing values in the number of steps are ignored for this analysis

##### Average daily activity
1. Number of steps taken in an interval averaged across all days is calculated
2. Trend of the activity in a day is plotted in a time series plot


```r
interval_steps <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(avg_steps = mean(steps))

plot(interval_steps$interval, interval_steps$avg_steps, type = "l", 
     xlab = "Interval of the day", ylab = "Average number of steps", 
     main = "Average number of steps in an interval over two months")
```

![plot of chunk avg_steps_interval](figure/avg_steps_interval-1.png)

##### 5-minute interval with the maximum number of steps


```r
maxSteps <- interval_steps[which.max(interval_steps$avg_steps), 1]
as.integer(maxSteps)
```

```
## [1] 835
```

The 5-minute interval which had the maximum number of steps was the 835 interval.

### 3. Imputing missing values

##### Total number of missing values


```r
missingVals <- sum(is.na(activity$steps))
missingVals
```

```
## [1] 2304
```

The total number of rows with missing values of steps is 2304.

##### Imputing strategy
1. Split the activity data based on missing values in the columns **steps**.
2. Use the calculated data of average steps taken in an interval across all days to impute missing values for the specific interval with missing data.


```r
# Splitting missing and non missing data
activity_na <- activity[is.na(activity$steps),]
activity_non_na <- activity[!is.na(activity$steps),]

# Merging average steps per interval data to missing steps data
activity_na_new <- merge(activity_na, interval_steps, by.x = "interval", by.y = "interval", all.x = TRUE)

# Processing merged data
activity_na_new <- select(activity_na_new,avg_steps,date,interval)
names(activity_na_new) <- names(activity_non_na)
activity_imputed <- rbind(activity_na_new,activity_non_na)
activity_imputed <- arrange(activity_imputed,date,interval)
activity_imputed$steps <- as.integer(activity_imputed$steps)
```

##### Comparing distribution of total number of steps taken per day (imputed vs non missing)


```r
# Computing average steps taken per day for imputed data
daily_steps2 <- activity_imputed %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# Overlapping histograms for comparison
hist(daily_steps2$total_steps, xlab = "# Steps", ylab = "Frequency", 
     main = "Total number of steps taken per day", col = "Black")
hist(daily_steps$total_steps, xlab = "# Steps", ylab = "Frequency", 
     main = "Total number of steps taken per day", col = "Grey", add = TRUE)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey"))
```

![plot of chunk compare](figure/compare-1.png)

##### Comparing central tendencies


```r
mean_daily_steps_n <- as.integer(mean(daily_steps2$total_steps))
median_daily_steps_n <- as.integer(median(daily_steps2$total_steps))
c(mean = mean_daily_steps_n, median = median_daily_steps_n)
```

```
##   mean median 
##  10749  10641
```

The new mean of the imputed data is 10749 steps compared to the old mean of 10766 steps.

The new median of the imputed data is 10641 steps compared to the old median of 10765 steps.

### 4. Are there differences in activity patterns between weekdays and weekends?

##### Steps taken
1. New factor variable **dayCategory** is created which stores weekday/weekend information using date
2. Number of steps taken in an interval averaged by weekday/weekend is calculated
3. Trend of the activity in a weekday/weekend is plotted in a time series plot


```r
activity_imputed$dayCategory <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

interval_steps2 <- activity_imputed %>%
    filter(!is.na(steps)) %>%
    group_by(dayCategory,interval) %>%
    summarize(avg_steps = mean(steps))

xyplot(avg_steps ~ interval | dayCategory, data = interval_steps2, type = "l", layout = c(1, 2),
       main = "Average steps per interval by type of day",
       xlab = "Interval", ylab = "Average number of steps")
```

![plot of chunk dayTypeTrend](figure/dayTypeTrend-1.png)

Yes, the step activity trends are different based on whether the day occurs on a weekend or not. We see start in the activity earlier in a weekday than weekend. Furthermore, maximum activity is also in the weekend during the time corresponding to morning commute.
