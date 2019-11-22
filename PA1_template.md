---
title: "Reproducible Research: Peer Assesment 1"
output: 
  html_document:
    keep_md: true
---
*Submitted by: Sanjib Tiwari*  
*Date: 11/22/2019*

# Activity Monitoring Data Analysis

The activity monitoring data was obtained from coursera course web site  

The different variables in the dataset are  

* steps: Number of steps taken in 5 minute interval. Contains missing values

* date: Measurement Date

* interval: The interval at which readings were taken


```r
# loading required libraries
library(tidyverse) # for data manupulation/wrangling
library(ggplot2) # for vizualization
library(lubridate) # for working with dates
```

The following code reads the data and converts the date (Make sure you have set up your working directory correctly)

```r
activity <- read.csv("activity.csv")
activity$date <- ymd(activity$date)
```

Taking a look at the data

```r
glimpse(activity)
```

```
## Observations: 17,568
## Variables: 3
## $ steps    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
## $ date     <date> 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012...
## $ interval <int> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 10...
```

##Part A Analysis: What is mean total number of steps taken per day?


```r
steps_stat_per_day <- activity %>% 
  select(date, steps) %>% 
  group_by(date) %>% 
  summarize(total_steps  = sum(steps, na.rm = TRUE),
            mean_steps   = mean(x = total_steps, na.rm = TRUE),
            median_steps = median(total_steps, na.rm = TRUE)) %>% 
  ungroup(.)
```

The following code displays the mean and median number of steps taken per day

```r
str_glue("The mean number of steps taken per day is {round(mean(steps_stat_per_day$total_steps, na.rm =TRUE),2)}")
```

```
## The mean number of steps taken per day is 9354.23
```

```r
str_glue("The median number of steps taken per day is {round(median(steps_stat_per_day$total_steps, na.rm =TRUE),2)}")
```

```
## The median number of steps taken per day is 10395
```

Histogramm of the total number of steps taken each day


```r
steps_stat_per_day %>% 
  
  ggplot(aes(total_steps)) + 
  
  geom_histogram(binwidth = 5000, color = "white", fill = "black") +
  
  labs( title = "Frequency Distribution of Total Steps",
        subtitle = " Maximum frequency distribution accours ar around 10,000 steps per day",
        x = "Total Steps",
        y = "Frequency") +
  theme_light()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Part B Analysis: What is the average daily activity pattern?

Data manupulation

```r
interval_mean_steps <- activity %>% 
  select(steps, interval) %>% 
  group_by(interval) %>% 
  summarize(mean_steps = mean(steps, na.rm = TRUE)) %>% 
  ungroup(.) 
```

Time Series plot


```r
interval_mean_steps %>% 
  ggplot(aes(interval,mean_steps)) +
  
  geom_line() +
  
  labs(title = "Average Steps per Interval",
       subtitle = "It is clear from the plot that the maximum average number of steps
occurs at around the 800 minute interval",
       x = "Interval",
       y = "Average Number of Steps") +
  
   theme_light()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


## Part C Analysis: Imputing Missing Values

Total number of missing values in the dataset


```r
str_glue(" The number of missing values in the activity data set steps observation 
         is {sum(is.na(activity$steps))}. This is only {round(sum(is.na(activity$steps))/nrow(activity)*(100),0)}% of the total value")
```

```
## The number of missing values in the activity data set steps observation 
## is 2304. This is only 13% of the total value
```

we'll use the mean for each interval to impute the missing values.


```r
activity_no_missing <- activity %>% 
        select(steps, interval) %>% 
        group_by(interval) %>% 
        summarize(mean_steps_interval = mean(steps, na.rm = TRUE)) %>% 
        ungroup(.) 
```

Checking if there are NAs in the mean used to impute


```r
sum(is.na(activity_no_missing$mean_steps_interval))
```

```
## [1] 0
```
Since the sum is zero, we are good.

Imputing missing values with the mean for each interval


```r
activity_no_missing_stat <- activity %>%
        left_join(.,activity_no_missing, by = "interval") %>% 
        mutate(steps = ifelse(is.na(steps), mean_steps_interval, steps)) %>% 
        select(steps, date) %>% 
        group_by(date) %>% 
        summarize(total_steps = sum(steps),
               mean_steps  =mean(steps),
               median_steps = median(steps)) %>% 
        ungroup(.)
```

Histogram of the total number of steps taken each day


```r
activity_no_missing_stat %>% 
  
  ggplot(aes(total_steps)) +
  
  geom_histogram(binwidth = 5000, color = "white", fill = "black") +
  
  labs( title = "Frequency Distribution of Total Steps with Missing Values Replaced by Mean",
        subtitle = "Frequency distribution of total steps shows that\nit is normally distributed with the mean around 10,000 steps
which isn't any different than when the missing values were excluded",
        x = "Total Steps",
        y = "Frequency") +
  theme_light()
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

The mean and median total number of steps taken per day


```r
str_glue("The mean number of steps taken per day after replacing missing values 
         is {round(mean(activity_no_missing_stat$total_steps, na.rm =TRUE),2)}")
```

```
## The mean number of steps taken per day after replacing missing values 
## is 10766.19
```

```r
str_glue("The median number of steps taken per day after replacing missing values
         is {round(median(activity_no_missing_stat$total_steps, na.rm =TRUE),2)}")
```

```
## The median number of steps taken per day after replacing missing values
## is 10766.19
```

This is almost similar to the mean and median for the dataset where we dropped the missing values

## Part D Analysis: Are there differences in activity patterns between weekdays and weekends?

Extracting the day information, setting an indicator for weekdays and weekends. The indicators are then  
turned into a factor variable

```r
days_activity <- activity %>% 
        mutate(day_of_week = weekdays(date)) %>% 
        mutate(day_indicator = case_when(
                day_of_week == "Saturday" ~ "Weekend",
                day_of_week == "Sunday"  ~ "Weekend",
                TRUE ~ "Weekdays")) %>% 
        mutate(day_indicator = as_factor(day_indicator)) %>%
        select(interval,day_indicator, steps) %>% 
        group_by(interval, day_indicator) %>% 
        summarize(mean_steps = mean(steps, na.rm = TRUE)) %>% 
ungroup(.)
```

Panel Plot containing a time series plot of the 5-minute interval and the average number of steps taken  
for weekends and weekdays


```r
days_activity %>% 
        ggplot(aes(interval,mean_steps)) +
        
        geom_line() +
        
        facet_wrap(~day_indicator, ncol = 1, scale = "free_x", "fixed_y") +
        
        labs( title = "Average Steps by Interval- Weekdays and Weekend",
              subtitle = "Weekdays average steps peak around 800 minutes interval
while weekend average steps stay pretty stable",
              x = "Intervals",
              y = "Average Steps") +
        
        theme_light()
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

