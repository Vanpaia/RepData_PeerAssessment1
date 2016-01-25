# Mean total number of steps taken per day
# Average daily activity pattern
# Imput missing values
# Differences in activity patterns between weekdays and weekends


# Load all needed libraries

if (!require("ggplot2")) {
  install.packages("ggplot2", repos="http://cran.rstudio.com/") 
  library("ggplot2")
}
if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}
if (!require("xtable")) {
  install.packages("xtable", repos="http://cran.rstudio.com/") 
  library("xtable")
}

# Check if the data is present, download if needed

if (dir.exists("repdata_data_activity")) {
  cat("data found
      ")
} else if (file.exists("repdata_data_activity.zip")) {
  unzip("repdata_data_activity.zip")
  cat("data extracted
      ")
} else {
  temp_file <- tempfile(tmpdir = getwd())
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp_file)
  unzip(temp_file)
  unlink(temp_file)
  cat("data downloaded
      ")
}

# load the data

x <- data.table(read.csv("activity.csv"))

# calculate total, mean, median steps per day and the mean per interval

total <- x[, lapply(.SD, sum), by=x$date]
mean <- x[, lapply(.SD, mean), by=x$date]
median <- x[, lapply(.SD, function(x) as.numeric(median(x))), by=x$date]

overview <- data.table(date=total$x, total=total$steps, mean=mean$steps, median=median$steps)

mean_interval <- x[, lapply(.SD, mean, na.rm = TRUE), by=x$interval]

# plot a histogram for the total number of steps per day

ggplot(data=total, aes(total$steps)) +
  geom_histogram(breaks=seq(0, 20000, by = 1000),
                 col="red",
                 fill="green",
                 alpha = .2) +
  labs(title="Histogram for Steps") +
  labs(x="Steps", y="Count") +
  xlim(c(0,20000)) +
  ylim(c(0,15))

# plot the timeplot of average steps per interval

ggplot(data=mean_interval, aes(mean_interval$x, mean_interval$steps)) +
  geom_line() +
  labs(title="Timeplot for Average Steps per Interval")+
  labs(x="Interval", y="Average Steps")

# maximum number of steps

ordered <- mean_interval[order(steps)]
max <- ordered$x[length(ordered$x)]

# calculate missing data

missing <- sum(is.na(x$steps))

# Fill NA's

missing_values <- which(is.na(x))
replacement_mean <- mean
replacement_mean$steps[which(is.na(replacement_mean$steps))] <- 0
new <- x
for (i in 1:length(missing_values)){
  day<- as.integer(i/288+1)
  new$steps[missing_values[i]] <- replacement_mean$steps[day]
}

# New total, mean and median

new_total <- new[, lapply(.SD, sum), by=new$date]
new_mean <- new[, lapply(.SD, mean), by=new$date]
new_median <- new[, lapply(.SD, function(new) as.numeric(median(new))), by=new$date]

new_overview <- data.table(date=new_total$new, total=new_total$steps, mean=new_mean$steps, median=new_median$steps)

# plot a histogram for the total number of steps per day of the new data

ggplot(data=new_total, aes(new_total$steps)) +
  geom_histogram(breaks=seq(0, 20000, by = 1000),
                 col="red",
                 fill="green",
                 alpha = .2) +
  labs(title="Histogram for Steps") +
  labs(x="Steps", y="Count") +
  xlim(c(0,20000)) +
  ylim(c(0,15))

# Weekday

new$date <- as.Date(new$date, "%Y-%m-%d")

for (i in 1:length(new$date)){
  if (weekdays(new$date[i], abbreviate = FALSE) == "Saturday"){
      new$weekday[i] <- "weekend"
  } else if (weekdays(new$date[i], abbreviate = FALSE) == "Sunday"){
      new$weekday[i] <- "weekend"
  } else {
      new$weekday[i] <- "weekday"
  }
}

#  the average number of steps taken, averaged across all weekday days or weekend days

mean_interval_weekday <- new[, lapply(.SD, mean), by=list(new$interval, new$weekday)]
setnames(mean_interval_weekday, 1:4, c("interval", "weekday", "steps", "date"))

ggplot(data=mean_interval_weekday, aes(mean_interval_weekday$interval, mean_interval_weekday$steps, fill = mean_interval_weekday$weekday)) +
  geom_line(
    col = "blue") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(weekday ~ .) +
  labs(title="Timeplot for Average Steps per Interval")+
  labs(x="Interval", y="Average Steps")