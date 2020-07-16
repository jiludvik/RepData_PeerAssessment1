library(ggplot2)
library(dplyr)

##
## 1. Loading and preprocessing the data
##
# Load the data (i.e. read.csv() read.csv())
# Process/transform the data (if necessary) into a format suitable for your analysis
activity <- read.csv(file="activity.csv", 
                     colClasses=c("steps"="integer", "date"="Date", "interval"="integer"))

##                     
## 2. What is mean total number of steps taken per day?
##
# Calculate the total number of steps taken per day
steps_daily_total <- activity %>% group_by(date) %>% summarise (steps= sum(steps))
#  Make a histogram of the total number of steps taken each day
ggplot(steps_daily_total, aes(x=steps)) + 
        geom_histogram(bins=10, fill="grey50", color="white") + 
        labs (x="Daily Steps", y="Frequency", title = "Average Frequency")
dev.copy(png, file="p1_totalsteps_original.png", height = 480, width = 480, units = "px")
dev.off()
# Calculate and report the mean and median of the total number of steps taken per day
mean_daily_steps <- round(mean(steps_daily_total$steps, na.rm=TRUE))
median_daily_steps <- round(median(steps_daily_total$steps, na.rm=TRUE))
mean_daily_steps
median_daily_steps


##
## 3. What is the average daily activity pattern?
##
# Time series plot of the 5-minute interval and the average number of steps taken
steps_interval_avg <- 
        activity %>% 
        group_by(interval) %>% 
        summarise (steps=mean(steps, na.rm=TRUE)) %>%
        ungroup() %>%
        arrange(interval) 
ggplot(steps_interval_avg, aes(interval, steps)) + 
        geom_line() + 
        labs (x="Interval", y="Number of steps", title = "Average Number Of Steps By Interval")
dev.copy(png, file="p2_averagesteps.png", height = 480, width = 480, units = "px")
dev.off()
# Which 5-minute interval, on average across all the days in the dataset, contains 
# the maximum number of steps?
peak_interval <- as.integer(steps_interval_avg [which.max(steps_interval_avg$steps),"interval"])
peak_steps <- as.integer(steps_interval_avg [which.max(steps_interval_avg$steps),"steps"])
peak_interval_time <- sprintf("%.02d:%.02d", peak_interval %/% 60, round(peak_interval %% 60))
peak_interval
peak_steps
peak_interval_time

##
## 4. Imputing missing values
##
# Calculate and report the total number of missing values in the dataset
missing_values_rows <- sapply(activity, function(x) sum(is.na(x)))
missing_values_rows

# Create a dataset that is equal to the original dataset but with the missing data filled in.
activity_imputed <- activity %>% arrange(date, interval) 
activity_imputed$steps <- 
        coalesce(activity$steps, 
                 rep(steps_interval_avg$steps, 
                 times=length(activity$steps)/length(steps_interval_avg$steps)))
# Make a histogram of the total number of steps taken each day using the imputed data 
steps_imputed_daily_total <- 
        activity_imputed %>% 
        group_by(date) %>% 
        summarise (steps= sum(steps))
ggplot(steps_imputed_daily_total, aes(x=steps)) + 
        geom_histogram(bins=10, fill="grey50", color="white") + 
        labs (x="Daily Steps  (incl. Imputed Values", y="Frequency", title = "Activity Frequency")
dev.copy(png, file="p3_totalsteps_imputed.png", height = 480, width = 480, units = "px")
dev.off()

# Calculate and report the mean and median total number of steps taken per day.
mean_daily_steps_imputed <- round(mean(steps_imputed_daily_total$steps, na.rm=TRUE))
median_daily_steps_imputed <- round(median(steps_imputed_daily_total$steps, na.rm=TRUE))
mean_daily_steps_imputed
median_daily_steps_imputed

# Do these values differ from the estimates from the first part of the assignment? 
mean_daily_steps_imputed-mean_daily_steps
median_daily_steps_imputed-median_daily_steps

# What is the impact of imputing missing data on the estimates of the total daily number of steps?
steps_daily_total_dif <- 
        steps_daily_total %>%               # copy the steps_daily_total data set
        mutate(steps_original0=steps) %>%   # create a copy of the steps column called steps_original0
        rename(steps_original=steps) %>%    # rename steps column to steps_original
        tidyr::replace_na(list(steps_original0=0)) %>% # replace NAs in steps_original0 with zeros
        mutate (steps_imputed = steps_imputed_daily_total$steps, # create a copy of the column steps_imputed
                steps_dif = steps_imputed-steps_original0) %>% #calculated difference between the two columns
        filter(steps_dif!=0) %>%             # filter out columns with zero difference
        select (-steps_original0)            # remove steps_original0
steps_daily_total_dif

##
## 5. Are there differences in activity patterns between weekdays and weekends?
##        
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity_imputed$daytype <- factor(
        (weekdays(activity_imputed$date) %in% c("Saturday", "Sunday")), 
        labels=c("Weekday", "Weekend"))
# Make a panel plot containing a time series plot  of the 5-minute interval and 
# the average number of steps taken, averaged across all weekday days or weekend days
steps_imputed_interval_avg <- 
        activity_imputed %>% 
        group_by(daytype, interval) %>% 
        summarise (steps= round(mean(steps)))

ggplot(steps_imputed_interval_avg, aes(interval, steps)) + 
        geom_line()  + 
        facet_wrap(var(daytype), nrow=2) + 
        labs (x="Interval", y="Number of steps")
dev.copy(png, file="p4_averagesteps_interval_bydaytype.png", height = 480, width = 480, units = "px")
dev.off()