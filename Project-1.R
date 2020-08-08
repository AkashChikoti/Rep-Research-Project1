# Loading dependencies
library(dplyr)
library(lattice)

# Loading data
setwd("C:/Users/Abhinav/Desktop/Data Science/Reproducible Research/Week2 Project")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "repdata_data_activity.zip")
unzip("repdata_data_activity.zip")
activity <- read.csv("activity.csv", na.strings = NA)

# Processing data
activity$date <- as.Date(activity$date, "%Y-%m-%d")

# 1. What is mean total number of steps taken per day?
# Calculation of total number of steps taken per day
daily_steps <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps))

# Distribution of total number of steps taken per day
hist(daily_steps$total_steps, xlab = "# Steps", ylab = "Frequency", 
     main = "Total number of steps taken per day")

# Checking central tendencies (mean and median)
mean_daily_steps <- as.integer(mean(daily_steps$total_steps))
median_daily_steps <- as.integer(median(daily_steps$total_steps))
c(mean = mean_daily_steps, median = median_daily_steps)

# 2. What is the average daily activity pattern?
# Calculation of average daily activity
interval_steps <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(avg_steps = mean(steps))

# Trend of average daily activity
plot(interval_steps$interval, interval_steps$avg_steps, type = "l", 
     xlab = "Interval of the day", ylab = "Average number of steps", 
     main = "Average number of steps in an interval over two months")

# 5-minute interval with the maximum number of steps
maxSteps <- interval_steps[which.max(interval_steps$avg_steps), 1]
as.integer(maxSteps)

# 3. Imputing missing values
# Total number of missing values
missingVals <- sum(is.na(activity$steps))
missingVals

# Imputing strategy
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

# Comparing distribution of total number of steps taken per day (imputed vs non missing)
# Computing average steps taken per day for imputed data
daily_steps2 <- activity_imputed %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps))

# Overlapping histograms for comparison
hist(daily_steps2$total_steps, xlab = "# Steps", ylab = "Frequency", 
     main = "Total number of steps taken per day", col = "Black")
hist(daily_steps$total_steps, xlab = "# Steps", ylab = "Frequency", 
     main = "Total number of steps taken per day", col = "Grey", add = TRUE)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey"))

# Comparing central tendencies
mean_daily_steps_n <- as.integer(mean(daily_steps$total_steps))
median_daily_steps_n <- as.integer(median(daily_steps$total_steps))
c(mean = mean_daily_steps_n, median = median_daily_steps_n)

# 4. Are there differences in activity patterns between weekdays and weekends?
# dayCategory is created which stores weekday/weekend
activity_imputed$dayCategory <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Number of steps taken in an interval averaged by weekday/weekend
interval_steps2 <- activity_imputed %>%
    filter(!is.na(steps)) %>%
    group_by(dayCategory,interval) %>%
    summarize(avg_steps = mean(steps))

# Trend of average daily activity split by dayCategory
xyplot(avg_steps ~ interval | dayCategory, data = interval_steps2, type = "l", layout = c(1, 2),
       main = "Average steps per interval by type of day",
       xlab = "Interval", ylab = "Average number of steps")