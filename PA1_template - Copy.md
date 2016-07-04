---
title: "Reproducible Research Peer Assessment 1"
author: "Tayler Hentges"
date: "July 3, 2016"
output:
  md_document:
    variant: markdown_github
First set working directory to where data is located and read in data.

```{r} 
setwd("C:/Users/ttop2/Desktop/repdata%2Fdata%2Factivity")
data<-read.csv("activity.csv")
summary(data)
```

Notice how there are many NAs in the document. Let's remove those first.
```{r}
datanonas<-data[!is.na(data$steps),]
datanonas$date<-factor(datanonas$date)
summary(datanonas)
```
One question we want to answer is the mean number of steps taken each day.
First we will sum up the steps taken.

```{r}
dailysteps<-as.data.frame(tapply(datanonas$steps, as.factor(datanonas$date), sum))
names(dailysteps)<-c("Steps")
```
In order to visualize the data, a histogram will show the results of how many days have the specific number of steps.
```{r}
library(ggplot2)
qplot(dailysteps$Steps, geom="histogram", ylab="Number of Days", xlab="Number of Steps", binwidth = 500)
png("plot1.png")
dev.off()
```
For a more computational value, see the actual calculated values.
```{r}
print(mean(dailysteps$Steps), row.names = FALSE)
print(median(dailysteps$Steps), row.names = FALSE)
```
The next question is what is the average daily activity? 
To calculate, we will look at the data by interval.
```{r}
stepsbyinterval<-tapply(datanonas$steps, as.factor(datanonas$interval), mean)
stepsbyinterval<-as.data.frame(stepsbyinterval)
stepsbyinterval$interval<-rownames(stepsbyinterval)
plot(stepsbyinterval$interval, stepsbyinterval$mean, type = 'l', ylab = "Mean of Steps", xlab = "Interval")
png("plot2.png")
dev.off()
```


We also would like to know which interval has the highest activity.
```{r}
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMax(stepsbyinterval)

```
Moving on, we want to improve the accuracy of some calculations by imputing the data.
First let's recall how many NAs are in the dataset.
```{r}
sum(is.na(data))
```
For simplicity, we will be changing the NAs to a mean at that point.
```{r}
stepsbyinterval$interval <- as.integer(stepsbyinterval$interval)
imputed <- merge(data, stepsbyinterval, by = "interval")
imputed$steps[is.na(imputed$steps)] <-imputed$stepsbyinterval[is.na(imputed$steps)]
summary(imputed)
```
Next, lets create a histogram of the newly crafted data.
```{r}
imputedsums <- as.data.frame(tapply(imputed$steps, as.factor(imputed$date), sum))
names(imputedsums) <- c("Steps")
qplot(imputedsums$Steps, geom="histogram", ylab="Number of Days", xlab="Number of Steps", binwidth = 500)
png("plot3.png")
dev.off()
```
And lastly let's see how the means differ between the two sets.
```{r}
print(mean(imputedsums$Steps), row.names = FALSE)
print(median(imputedsums$Steps), row.names = FALSE)
```
For our final question, we want to investigate to see if there is a difference in the number of steps between the weekdays and the weekend.
To start, we must determine which days are weekdays/weekends and add this to the new dataset.
```{r}
imputed$dayofweek <- weekdays(as.Date(imputed$date))

wkday<- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

imputed$dtype <- factor(weekdays(as.Date(imputed$date)) %in% wkday, 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
head(imputed)
```
And now our final calculation to determine if there is a difference.
```{r}
averages <- aggregate(steps ~ interval + dtype, data=imputed, mean)
ggplot(averages, aes(interval, steps)) + geom_line(colour="green") + facet_grid(dtype ~ .) +
  xlab("Interval (5 minutes)") + ylab("Average number of Steps")
png("plot4.png")
dev.off()
```