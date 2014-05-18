library(stats)

data = read.csv("./rrProject1/activity.csv", header = TRUE) ## loads data --> takes a while
data$date = as.Date(data$date, "%Y-%m-%d")

stepsbyday = aggregate(steps ~ date, data = data, sum)


###Make a histogram of the total number of steps taken each day
hist(stepsbyday$steps)

### Calculate and report the mean and median total number of steps taken per day
mean(stepsbyday$steps, na.rm = TRUE) ## 10766.19
median(stepsbyday$steps, na.rm = TRUE) ## 10765

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
### and the average number of steps taken, averaged across all days (y-axis)
plot(stepsbyday$interval, data$steps, type = "l")

### Which 5-minute interval, on average across all the days in the dataset,
### contains the maximum number of steps?


