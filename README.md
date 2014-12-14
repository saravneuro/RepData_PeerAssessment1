RepData_PeerAssessment1
=======================
title: "Reproducible Research, Peer Assessment 1- Activity Monitoring Data"
author: "Dr.Saravanan Subramaniam"
date: "Sunday, December 15, 2014"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

#### Loading and Preprocessing the data
```{r}
file.loc<-"C:/Users/Dr.Saravanan/Desktop/R/"
setwd(file.loc)
Data <- "activity.csv"
Raw_Data <- read.table(Data, TRUE, sep = ",")
```
#### Formating the data suitable for the analysis, removing NA's
```{r}
Omit_NA <- na.omit(Raw_Data)
```
#### Summation of Steps per day, removing NA's
```{r}
Steps_Per_Day <- tapply(Omit_NA$steps, Omit_NA$date, sum)
```
```{r}
Days <- names(Steps_Per_Day)
```
```{r}
Steps_Per_Day <- as.vector(Steps_Per_Day)
```
```{r}
names(Steps_Per_Day) <- Days
```
```{r}
which(!is.na(Raw_Data[which(Raw_Data$date == "2012-11-10"),]$steps))
```
```{r}
Steps_Per_Day <- na.omit(Steps_Per_Day)
```
#### Plotting the histogram
```{r}
hist(Steps_Per_Day, 30, col= "red", xlab= "Number of Steps Per Day")
```
```{r}
mean(Steps_Per_Day, na.rm = TRUE)
```
```{r}
median(Steps_Per_Day, na.rm = TRUE)
```
#### What is the average daily activity pattern?

* Calculating the average number of steps for each interval, averaged across all days:
```{r}
Omit_NA$interval <- as.factor(Omit_NA$interval)
```
```{r}
Average_Steps_Day_Interval <- tapply(Omit_NA$steps, Omit_NA$interval, mean)
```
```{r}

plot(names(Average_Steps_Day_Interval), Average_Steps_Day_Interval, type="l", col="red", main="Time Series of Average Number of Steps", xlab="Interval", ylab= "Average Number of Steps")
```
```{r}
max <- Average_Steps_Day_Interval[
  which(max(Average_Steps_Day_Interval)==Average_Steps_Day_Interval)]
```
```{r}
names(max)
```
```{r}
unname(max)
```
#### Imputing the missing values
* In number of days and Intervals the missing values are filled with NA's. The presence of missing days may introduce bias into some calculations or summaries of the data. 
The total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
length(which(is.na(Raw_Data$steps) == TRUE))
```
#### Filling the missing data
```{r}
Median_Steps_Day_Interval <- tapply(Omit_NA$steps, Omit_NA$interval, median)
```
```{r}
interval <- Raw_Data$interval[which(is.na(Raw_Data$steps) == TRUE)]
```
```{r}
CopyRawData <- Raw_Data
```
```{r}
Raw_Data$steps[which(is.na(Raw_Data$steps) == TRUE)] <-  
  sapply(interval, function(x, y){
    y[which(names(y) == x)]
  }, y = Median_Steps_Day_Interval)
```
```{r}
TidyData <- Raw_Data
```
```{r}
Raw_Data <- CopyRawData
```
```{r}
Steps_Per_Day <- tapply(TidyData$steps, TidyData$date, sum)
Days <- names(Steps_Per_Day)
Steps_Per_Day <- as.vector(Steps_Per_Day)
names(Steps_Per_Day) <- Days
```
#### Plotting the histogram
```{r}

hist(Steps_Per_Day, 30, col= "blue", xlab= "Number of steps per Day")
```
```{r}

mean(Steps_Per_Day, na.rm = TRUE)
```
```{r}
median(Steps_Per_Day, na.rm = TRUE)
```
#### Differential activity patterns between weekdays and weekends.
* A new factor variable in the dataset is created with two levels - “weekdays” and “weekends” indicating whether a given date is a weekday or a weekend.
```{r}
TidyData$WeekDays <- weekdays(as.Date(TidyData$date))
TidyData$TypeDay <- sapply(TidyData$WeekDays, function(x,y){
  if(x %in% y) "weekends"
  else "weekdays"
}, y = c("Saturday", "Sunday")) 
```
```{r}

TidyData$TypeDay <- as.factor(TidyData$TypeDay)
TidyData$interval_TypeDay <- mapply(function(x,y){
  paste(x,y,sep = "_")}, x = TidyData$interval, y = TidyData$TypeDay)
```
```{r}

Average_Steps_Day_Interval_Type_Day <- tapply(TidyData$steps, 
                                         as.factor(TidyData$interval_TypeDay), mean)
```
```{r}
Interval_TypeDay <- names(Average_Steps_Day_Interval_Type_Day)
Interval <- as.numeric(gsub("_.*","", Interval_TypeDay))
TypeDay <- gsub(".*_","", Interval_TypeDay)
```
```{r}
df <- data.frame(steps = unname(Average_Steps_Day_Interval_Type_Day), interval = Interval,typeDay = TypeDay)
```
#### Loading (ggplot2) library
```{r}
require(ggplot2)
```
#### Plotting the data
```{r}
p <- ggplot(df, aes(x=interval, y=steps)) + facet_grid(typeDay ~ .)
```
```{r}
p + geom_line(aes(group=typeDay)) + scale_x_discrete(breaks = seq(0,2355, by = 250))
```
```{r}
options(rpubs.upload.method = "internal")
```
