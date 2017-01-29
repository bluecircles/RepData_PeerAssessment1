# Reproducible Research - Week 2 - Assignment

## Loading and Processing Data
Below the R script for part1





```r
######################################
## PART 1
## Assuming that the data files are saved in the current working directory
## Read data from CSV file

dt <- read.csv("./activity.csv", header = TRUE)
```

## What is mean total number of steps taken per day?
Below the R script for part2


```r
######################################
## PART 2
## Load the dyplr package
options(repos=c(CRAN="http://cran-mirror.cs.uu.nl/"))
install.packages("dplyr")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(dplyr)

dt_q2 <- select(dt, steps, date)

## Using group by and summarise to count number of steps per day
by_day <- group_by(dt_q2, date)
n_steps <- summarise(by_day, steps = sum(steps, na.rm = TRUE))

## Load the ggplot system to create the histogram
install.packages("ggplot2")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(ggplot2)

ggplot(data = n_steps, aes(n_steps$date)) + 
  geom_histogram(aes(n_steps$steps), fill = "blue") + 
  labs(title = "Histogram - Number of Steps by Day") + 
  labs(x = "Day", y = "Steps")
```

![plot of chunk part2](figure/part2-1.png)

```r
## Calculate mean and median
mean <- mean(n_steps$steps , na.rm = TRUE)
median <- median(n_steps$steps, na.rm = TRUE)
```

The mean is: `r.mean`  
The median is: `r.median`


## What is the average daily activity pattern?
Below the R script for part3


```r
######################################
## PART 3
dt_q3 <- select(dt, steps, interval)

by_interval <- group_by(dt_q3, steps, interval)
avg_steps <- summarise(by_interval, avg = mean(steps, na.rm = TRUE))

ggplot(avg_steps, aes(interval, avg)) +
  geom_line() +
  labs(title = "Average Number of Steps") +
  labs(x = "Intervals", y = "Average Steps")
```

![plot of chunk part3](figure/part3-1.png)

```r
maximum <- filter(avg_steps, avg == max(avg_steps$avg, na.rm = TRUE))
maximun_interval <- maximum$interval
```

The interval with the MAX steps is: `r.maximun_interval`

## Imputing missing values
Below the R script for part4


```r
######################################
## PART 4
dt_q4 <- select(dt, steps)
na <- sum(is.na(dt_q4$steps))

dt_q4.1 <- select(dt, steps, date, interval)

## Median steps per interval
by_day2 <- group_by(dt_q4.1, interval)
steps_per_interval <- summarise(by_day2, median = median(steps, na.rm = TRUE))

## Combine the two data sets
merged <- dt_q4.1 %>% left_join (steps_per_interval, by = "interval")

## Create a variable without NA values and plot
merged$steps_final <- ifelse(is.na(merged$steps), merged$median, merged$steps)

ggplot(data = merged, aes(merged$date)) + 
  geom_histogram(aes(merged$steps_final), fill = "blue") + 
  labs(title = "Histogram - Number of Steps by Interval") + 
  labs(x = "Interval", y = "Steps")    
```

![plot of chunk part4](figure/part4-1.png)

```r
mean_q4 <- mean(merged$steps_final , na.rm =TRUE)
median_q4 <- median(merged$steps_final , na.rm =TRUE)

impact_mean <- mean_q4 - mean
impact_median <- median_q4 - median
```

The impact on mean is: `r.impact_mean`  
The impact on median is: `r.impact_median`


## Are there differences in activity patterns between weekdays and weekends?
Below the R script for part5


```r
######################################
## PART 5
dt_q5 <- select(merged, steps, date, interval, median, steps_final)

dt_q5$wk_day <- weekdays(as.Date(as.character(dt_q5$date)))

## Create a weekday vector and a new column with the weekday/ weekend classification
wkdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
dt_q5$is_wk_day <- ifelse(dt_q5$wk_day %in% wkdays ,"weekday", "weekend") 

## Create the data set
dt_q5.1 <- select(dt_q5, is_wk_day, interval, steps_final)

by_wkday <- group_by(dt_q5.1, is_wk_day, interval)
avg_q5 <- summarise(by_wkday, average = mean(steps_final, na.rm = TRUE))

g <- ggplot(avg_q5, aes(avg_q5$interval, avg_q5$average))
g + geom_line(linetype = 1) + facet_grid(. ~ avg_q5$is_wk_day ) +
  labs(x = "Intervals", y= "Average") +
  labs(title = "Average Steps per Day - Weekday and Weekend")
```

![plot of chunk part5](figure/part5-1.png)

