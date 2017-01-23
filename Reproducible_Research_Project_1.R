# Loading and preprocessing the data

# Read in data from GitHub
# create "fileUrl" from which the data will come
fileUrl <- "https://raw.githubusercontent.com/Mooney721/RepData_PeerAssessment1/master/activity.csv"

# download data based on file URL and save in a destination file "./activity"
download.file(fileUrl, destfile = "./activity")

# read in activity data through .csv extension
activity <- read.csv("activity")


# "What is the mean total number of steps taken per day?" 

# Calculate the total number of steps taken per day
totalStepsDay <- aggregate(steps ~ date, data = activity, FUN = sum)

# Make a histogram of the total number of steps taken in each day
hist(totalStepsDay$steps, main = "Total Number of Steps Taken Each Day", 
     xlab = "Total Number of Steps",
     ylab = "Number of Days")

# Calculate and report the mean and median of the total number of steps taken per day
meanTotalStepsDay <- mean(totalStepsDay$steps, na.rm = TRUE)
print(meanTotalStepsDay)

medianTotalStepsDay <- median(totalStepsDay$steps, na.rm = TRUE)
print(medianTotalStepsDay)


# What is the average daily activity pattern?

# Making a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
# First, declare library(lattice) to create plot
library(lattice)

# Next, aggregate the mean number of steps by interval
meanStepsInterval <- aggregate(steps ~ interval, data = activity, na.rm = TRUE, FUN = mean)

# Drawing the time series plot
xyplot(steps ~ interval,
       data = meanStepsInterval,
       type = "l",
       main = "Mean Number of Steps per Day by Interval")

# Calculating the 5-minute interval containing, on average, the maximum number of steps
fiveMinInt <- meanStepsInterval$interval[meanStepsInterval$steps == max(meanStepsInterval$steps)]
print(fiveMinInt)

# Imputing missing values

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
missingSteps <- length(which(is.na(activity$steps == TRUE)))
print(missingSteps)

# Using MICE to impute missing values

# First, declare library(mice) for imputing missing values
library(mice)

# Second, use the MICE package to impute the missing values and create a new data set based on the original data set, but with imputed (no NA) values
set.seed(1)
activityMICE <- mice(activity)
activityNoNA <- complete(activityMICE)

# Next, calculate the total number of steps taken per day on implemented data
totalStepsDayNoNA <- aggregate(steps ~ date, data = activityNoNA, FUN = sum)

# Make a histogram of the total number of steps taken in each day with implemented data
hist(totalStepsDayNoNA$steps, main = "Total Number of Steps Taken Each Day", 
     xlab = "Total Number of Steps",
     ylab = "Number of Days")

# Calculate and report the mean and median of the total number of steps taken per day on implemented data
meanTotalStepsDayNoNA <- mean(totalStepsDayNoNA$steps)
print(meanTotalStepsDayNoNA)

medianTotalStepsDayNoNA <- median(totalStepsDayNoNA$steps)
print(medianTotalStepsDayNoNA)

# Calculating differences in mean and median of imputed data vs. non-imputed data
meanDiff <- meanTotalStepsDay - meanTotalStepsDayNoNA

medianDiff <- medianTotalStepsDay - medianTotalStepsDayNoNA


# "Are there differences in activity patterns between weekdays and weekends?" To answer this, a new factor variable with two levels - "weekday" and "weekend" was create and a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days were drawn.

# Convert "date" variable to date data type
activityNoNA$date <- as.Date(activityNoNA$date, format = "%Y-%m-%d")

# Determine weekday for date
activityNoNA$weekday <- weekdays(activityNoNA$date)

# Create indicator variable for whether 'weekday' variable was a day of the week or a day of the weekend
for (i in 1:nrow(activityNoNA))
{
  if ((activityNoNA$weekday[i] == "Saturday") || (activityNoNA$weekday[i] == "Sunday"))
  {
    activityNoNA$weekday.indicator[i] <- "weekend"
  }
  else
  {
    activityNoNA$weekday.indicator[i] <- "weekday"
  }
}

# Create time-series plot for the number of steps per day by interval, weekday vs. weekend
xyplot(steps ~ interval | weekday.indicator,
       data = activityNoNA,
       type = "l",
       main = "Mean Number of Steps per Day by Interval",
       sub = "Weekday vs. Weekend")

# Saving plots as jpeg files
# Additionally, save plots from above into 
jpeg('totalStepsDayHistogram.jpg')
hist(totalStepsDay$steps, main = "Total Number of Steps Taken Each Day", 
     xlab = "Total Number of Steps",
     ylab = "Number of Days")
dev.off()

jpeg('meanStepsByInterval.jpg')
xyplot(steps ~ interval,
       data = meanStepsInterval,
       type = "l",
       main = "Mean Number of Steps per Day by Interval")
dev.off()

jpeg('imputedTotalStepsDayHistogram.jpg')
hist(totalStepsDayNoNA$steps, main = "Total Number of Steps Taken Each Day", 
     xlab = "Total Number of Steps",
     ylab = "Number of Days")
dev.off()

jpeg('weekendVsWeekdayTSPlot.jpg')
xyplot(steps ~ interval | weekday.indicator,
       data = activityNoNA,
       type = "l",
       main = "Mean Number of Steps per Day by Interval",
       sub = "Weekday vs. Weekend")
dev.off()
