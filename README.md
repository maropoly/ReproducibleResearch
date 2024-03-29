---
title: "Reproducible Research: Peer Assessment 1"
author: "Michael Rith"
date: "July 12, 2016"
output: 
   html_document:
    keep_md: true
---
##This is the Read Me Markdown.  The embeded Markdown with graphs can be seen on the PA1_template_2.md file.  


```{r}
setwd("C:/Users/mrith/Documents/ReproducableResearch")
#Open Data Set
data <- read.csv("activity.csv")
#install lattice package
library(lattice)
library(knitr)
library(markdown)
```
```{r knitr_options , include=FALSE}
knitr::opts_chunk$set(fig.width=12, fig.height=4,
fig.path='Figs/', warning=FALSE,
message=FALSE)
```
Create Histogram with Mean and Median values for number of steps
```{r}
#Sum steps by day, create Histogram, and calculate mean and median.
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```

The Mean is `r format(rmean, round=2)` and the Median is `r format(rmedian, round=2)`

#What is the average daily activity?

```{r}
steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, 
     type="l", xlab="Interval",
     ylab="Number of Steps",
     main="Average # of Steps per Day by Interval")

```
Remove all missing variables and imputed zeros
```{r}
removeNA <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
```
Total Step recount and create a Histogram
```{r}
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps")

#Create Histogram graph to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="orange", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("green", "orange"), lwd=10)
```
Calculate new mean and median for imputed data.

```{r}

rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)

```

Calculate difference between imputed and non-imputed data.
```{r}

mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian

```
Calculate total difference.

```{r}

total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)

```
* The imputed data mean is `r format(rmean.i, round=2)`
* The imputed data median is `r format(rmedian.i, round=2)`
* The difference between the non-imputed mean and imputed mean is `r format(mean_diff, round=2)`
* The difference between the non-imputed median and imputed median is `r format(med_diff, round=2)`
* The difference of `r format(total_diff, round=2)` was the number of more steps in the imputed data.

#Are there differences in activity patterns between weekdays and weekends?
Want to create a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.
```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)


xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, 
       main="Average Steps per Day by Interval",xlab="Interval", 
       ylab="Steps",layout=c(1,2), type="l")

```



Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
#Fin
