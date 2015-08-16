dataset.zipfile <- file.path(".", "activity.zip")
dataset.csvfile <- file.path(".", "activity.csv")

if (!file.exists(dataset.csvfile)) {
    unzip(dataset.zipfile)
}
activity.data <- read.csv(dataset.csvfile)

## Question 1
library(dplyr)
library(scales)

#activity.data.nona <- na.omit(activity.data)

total_steps_by_date <- activity.data %>% 
    group_by(date) %>%
    summarise(Count = n(), Total = sum(steps), 
              Mean = mean(steps, na.rm = TRUE), 
              Median = median(steps, na.rm = TRUE)) %>%
    transform(date = strptime(date, "%Y-%m-%d"))

library(ggplot2)
ph <- ggplot(total_steps_by_date, aes(x=date, y=Total))
ph + geom_histogram(stat="identity") +
    labs(title = "Total Steps By Day", x = "Day", y = "Total Steps")
    scale_x_date(labels = date_format("%W"))

## Question 2
interval_means <- activity.data %>%
    group_by(interval) %>%
    summarise(Mean = round(mean(steps, na.rm = TRUE), 0), Median = median(steps, na.rm = TRUE)) %>%
    select(Interval = interval, Mean, Median)

pm <- ggplot(interval_means, aes(x=Interval, y=Mean))
pm + geom_line()

filter(interval_means, Mean == max(interval_means$Mean))

## Question 3
nrow(filter(activity.data, is.na(steps)))

imputed_activity_data <- select(interval_means, Interval, IntervalMean = Mean) %>%
    inner_join(activity.data, by = c("Interval" = "interval")) %>%
    mutate(steps = ifelse(is.na(steps), IntervalMean, steps)) %>%
    select(Interval, Steps = steps, Date = date)

imputed_total_steps_by_date <- imputed_activity_data %>% 
    group_by(Date) %>%
    summarise(Count = n(), Total = sum(Steps), 
              Mean = mean(Steps, na.rm = TRUE), 
              Median = median(Steps, na.rm = TRUE)) %>%
    transform(Date = strptime(Date, "%Y-%m-%d"))

ph_imputed <- ggplot(imputed_total_steps_by_date, aes(x=Date, y=Total))
ph_imputed + geom_histogram(stat="identity") +
    labs(title = "Total Steps By Day (Imputed Values)", x = "Day", y = "Total Steps")
scale_x_date(labels = date_format("%W"))

## Question 4

activity_data_with_weekday <- imputed_activity_data %>%
    transform(PosixDate = strptime(Date, "%Y-%m-%d")) %>%
    mutate(Weekday = factor(!grepl("Sat|Sun", weekdays(PosixDate, abbreviate = TRUE)), labels = c("weekend", "weekday"))) %>%
    select(Date, Interval, Steps, Weekday)

interval_means_with_weekday <- activity_data_with_weekday %>%
    group_by(Weekday, Interval) %>%
    summarise(Mean = round(mean(Steps, 0))) %>%
    select(Interval, Mean, Weekday)

p_by_weekday <- ggplot(interval_means_with_weekday, aes(x=Interval, y=Mean))
p_by_weekday + geom_line() + facet_grid(. ~ Weekday)

qplot(interval_means_with_weekday, aes(x=Interval, y=Mean), facet_grid(Weekday), geom="line")

