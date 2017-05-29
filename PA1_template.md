# <center>Reproducible Research <br><br> Course Project 1 </center>

# Call libraries

Set default for chunks



Install Libraries and packages

```r
library(dplyr)
library(data.table)
library(ggplot2)
library(lattice)
```

# Load the data

Download zip file

```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
## Unzip the files
```

Load the data

```r
data <- read.csv(unzip(temp), header = TRUE, sep = ",", na.strings = "NA" )
data <- mutate(data, date = as.Date(date))
```

# Calculate the mean of steps taken per day


```r
# Find the number of total steps per day
totalSteps_day <- group_by(data, date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE)/1000)

# Plot the histogram of total steps
ggplot(totalSteps_day, aes(totalSteps_day$total_steps)) + 
  geom_histogram(binwidth = 2, 
                 colour = "dark blue", aes(fill = ..count..)) + 
  scale_x_continuous(name = "Total Number of Steps", 
                     breaks = seq(0, 24, 2), limits = c(-1, 24)) +
  scale_y_continuous(name = "Frequency (thousands)") +
  ggtitle("Frequency histogram of total number of steps per day")
```

![](./images/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate the mean of total steps per day
meanSteps <- mean(totalSteps_day$total_steps)
meanSteps
```

```
## [1] 9.35423
```

```r
# Calculate the median of total steps per day
medSteps <- median(totalSteps_day$total_steps)
medSteps
```

```
## [1] 10.395
```

The mean of steps per day is 9.3542295 and the median is 10.395.

# Average daily activity pattern



```r
# Find the median of steps per interval
meanSteps_interv <- group_by(data, interval) %>%
  summarise(average_steps = mean(steps, na.rm = TRUE))

ggplot(meanSteps_interv, aes(interval, average_steps)) +
  geom_line(col = "dark red") + 
  xlab("Time Intervals (5-minute)") + 
  ylab("Mean number of steps taken (all Days)") + 
  ggtitle("Average Steps Taken at 5 minute Intervals")
```

![](./images/unnamed-chunk-3-1.png)<!-- -->

´The interval with the maximum number of steps is the interval 835

# Imputing missing values

The total number of missing values in the dataset is 2304. We will fill the missing values with the mean for that 5-minute interval:


```r
# Create a new dataset to fill the missing values
data_fill <- data

# Change the missing values for the mean of the 5-minute interval
for(i in which(is.na(data))){
  data_fill$steps[i] <- meanSteps_interv$average_steps [meanSteps_interv$interval == data_fill$interval[i]] 
}

# Check if there is any missing value in the filled dataset
sum(is.na(data_fill))
```

```
## [1] 0
```
We created a new dataset: data_fill with the missing data filled in.


```r
# Find the number of total steps per day
totalSteps_day_fill <- group_by(data_fill, date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE)/1000)

# Plot the histogram of total steps
ggplot(totalSteps_day_fill, aes(totalSteps_day_fill$total_steps)) + 
  geom_histogram(binwidth = 2, 
                 colour = "dark blue", aes(fill = ..count..)) + 
  scale_x_continuous(name = "Total Number of Steps", 
                     breaks = seq(0, 24, 2), limits = c(-1, 24)) +
  scale_y_continuous(name = "Frequency (thousands)") +
  ggtitle("Frequency histogram of total number of steps per day with filled missings")
```

![](./images/unnamed-chunk-5-1.png)<!-- -->

```r
# Calculate the mean of total steps per day
meanSteps <- mean(totalSteps_day_fill$total_steps)
meanSteps
```

```
## [1] 10.76619
```

```r
# Calculate the median of total steps per day
medSteps <- median(totalSteps_day_fill$total_steps)
medSteps
```

```
## [1] 10.76619
```


The mean of steps, with the filled missings, per day is 10.7661887 and the median is 10.7661887. With the missings filled, the mean and median have gone up.


# Weekdays x Weekends

We are looking to see if there are differences in the patterns between weekdays and weekends:


```r
# Lets create the factor variable "week_day" with two levels: "weekday" or "weekend"
data_fill <- mutate(data_fill, week_day = ifelse(
  weekdays(data_fill$date) %in% c("sábado", "domingo"), "weekend", "weekday"))

# Calculate the average number of steps taken by interval and type of week day
meanSteps_weekdays <-
  group_by(data_fill, interval, week_day) %>%
  summarise(average_steps = mean(steps))

# Plot two graphs, one for each type of day with the average steps by interval
xyplot(average_steps ~ interval | week_day, data = meanSteps_weekdays, type = "l", layout = 
         c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](./images/unnamed-chunk-6-1.png)<!-- -->










