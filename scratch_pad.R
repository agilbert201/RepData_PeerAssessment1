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
