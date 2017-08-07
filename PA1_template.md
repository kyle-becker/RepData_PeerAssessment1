# Reproducible Research: Peer Assessment 1


---
title: "Assignment 1 Reproducible Research"
author: "Kyle Becker"
date: "July 31, 2017"
output: html_document
---



## Loading and preprocessing the data

First of all before we begin any of the analysis we must first download the file, unzip into a csv and read the file into R. The code is shown below with comments for easy following.


```r
#Download file from github source and save as Activity_Data.zip
download.file('https://raw.github.com/rdpeng/RepData_PeerAssessment1/master/activity.zip', "Activity_data.zip")

#Unzip file and save as activity.csv
unzip('Activity_data.zip', "activity.csv")

Data_Raw <- read.csv("activity.csv", stringsAsFactors = FALSE)
Data <- read.csv("activity.csv", stringsAsFactors = FALSE)
```



```r
Data_Raw <- read.csv("C:\\Users\\kbec\\RepData_PeerAssessment1\\activity.csv", stringsAsFactors = FALSE)
Data <- read.csv("C:\\Users\\kbec\\RepData_PeerAssessment1\\activity.csv", stringsAsFactors = FALSE)
```

Before we can do any operations on the data first the date field must be formatted as a date. This is because when the file is imported the date field is originally formatted as a character not a date. Additionally those rows where the steps variable is not populated must be removed so aggregates can be performed on the data set


```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
#Format date column as date
Data$date <- as.Date(Data$date)

#Remove rows where the steps column is not filled in
Data <- Data[is.na(Data$steps)==FALSE,]
```


```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

In order to calculate the steps per day an aggregate function is used. The information is aggregated per date using a Sum function. The columns are then renamed. Once an aggregate data frame is created the results can be displayed through a histogram. The histogram displays the frequency of each step count over the time period, breaking the ranges in increments of 10.


```r
#Use the aggregate function to sum step counts by day
DF_Sum <- aggregate(Data$steps, by=list(Date = Data$date), FUN = sum, na.rm=TRUE)

#Rename Column names to Date and Total_Steps
names(DF_Sum) <- c("Date", "Total_Steps")

#Create Histogram
hist(DF_Sum$Total_Steps,10, xlab = "Step_Number", ylab = "Frequency", main = "Total Steps Per Day")
```


![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

After the histogram is created the mean and median is calculated from the daily step totals. The mean number of steps taken per day is 10766.19 and the median number of steps taken per day is 10765.


```r
#Calculate mean for the daily step totals
mean(DF_Sum$Total_Steps)
```

```
## [1] 10766.19
```

```r
#Calculated median for the daily step totals
median(DF_Sum$Total_Steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

In order to create a time series graph for the average steps taken during an interval the data must be summarized. Using the aggregate function the mean function is applied to the steps taken by the variable interval. Once the data is aggregated it can be graphed as a time series using the plot function. Then the max average steps returned using the R base functions. The average daily activity pattern is shown below. Interval 835 has the maximum number of steps at 206.1698.


```r
#Caculate mean steps taken by interval
DF_Int_Mean <- aggregate(Data$steps, by=list(Data$interval), FUN = mean)
names(DF_Int_Mean) <- c("Interval","Steps_Taken")

#Plot Results on a time series
plot(y = DF_Int_Mean$Steps_Taken, x=DF_Int_Mean$Interval, type="l", xlab="5 Minute Interval",
     main="Interval Pattern", ylab="Average Number of Steps Taken")
```

![](https://raw.githubusercontent.com/kyle-becker/RepData_PeerAssessment1/master/Graphs/Histogram1.jpg)<!-- -->

```r
#Finding interval with maximum number of steps
DF_Int_Mean[DF_Int_Mean$Steps_Taken==max(DF_Int_Mean$Steps_Taken),]
```

```
##     Interval Steps_Taken
## 104      835    206.1698
```

## Imputing missing values
The missing values are calculated using r base functions shown below.


```r
Missing_Values <- Data_Raw[is.na(Data_Raw$steps)==TRUE,]
nrow(Missing_Values)
```

```
## [1] 2304
```

In order to fill in the missing values I chose to use the caret library which is for machine learning but has several options for filling in missing values. First the library is attached. Following this, the preprocess function is used to create an equation based on k nearest neighbor algorith to populate missing values. Since this algorithm using variance to find an appropriate value to fill in by default the data is centered (mean subtracted) scaled (divided by the standard deviation). The algorithm finds the record that is closest to the original record which is missing a value and populates the missing value. Then using the predict function this formula is applied to the dataframe in order to populate any missing data. Lastly since the data by default is centered and scaled these operations are reversed so that it can be properly graph.


```r
#Filling in missing values
#Attach caret Machine Learning Library
library(caret)

#Use K nearest neighbors to predict NA values
Prep <- preProcess(Data_Raw, method = c("knnImpute"))

#Predict values for NA values
Data_Filled_In <- predict(Prep,Data_Raw)

#Remove scaling by multiplying by the standard deviation and adding back the mean
Data_Filled_In$steps <- Data_Filled_In$steps * sd(Data_Raw$steps, na.rm=TRUE)
Data_Filled_In$steps <- Data_Filled_In$steps + mean(Data_Raw$steps, na.rm=TRUE)

Data_Filled_In$interval <- Data_Filled_In$interval * sd(Data_Raw$interval, na.rm=TRUE)
Data_Filled_In$interval <- Data_Filled_In$interval + mean(Data_Raw$interval, na.rm=TRUE)
```

Once the data values are filled in they are then aggregated using the sum function by each date to get the total steps taken by each date in the data set.




```r
#Aggregate Totals by Day
DF_Sum_Filled_In <- aggregate(Data_Filled_In$steps, by=list(Date = Data_Filled_In$date), FUN = sum, na.rm=TRUE)
names(DF_Sum_Filled_In) <- c("Date", "Total_Steps") 
```

After the data set is aggregated the R window is then reformatted to display 2 graphs side by side using par command. Each graph is displayedand then the median and mean is calculated for each dataset to determine whether there is a difference. As we can see there is no difference between the two means but there is between the medians.


```r
#Aggregate Totals by Day
DF_Sum_Filled_In <- aggregate(Data_Filled_In$steps, by=list(Date = Data_Filled_In$date), FUN = sum, na.rm=TRUE)
names(DF_Sum_Filled_In) <- c("Date", "Total_Steps") 

#Reset window to display two graphs
par(mfrow=c(1,2))

#Original Histogram with missing values removed
hist(DF_Sum$Total_Steps,10, xlab = "Step_Number", ylab = "Frequency", main = "Total Steps Per Day", ylim = c(0,25))

#New Histogram with missing values imputed
hist(DF_Sum_Filled_In$Total_Steps,10, xlab = "Step_Number", ylab = "Frequency", main = "Total Steps Per Day", ylim = c(0,25))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
#Calculate the mean and median for the new data sets
mean(DF_Sum$Total_Steps)
```

```
## [1] 10766.19
```

```r
mean(DF_Sum_Filled_In$Total_Steps)
```

```
## [1] 10766.85
```

```r
median(DF_Sum$Total_Steps)
```

```
## [1] 10765
```

```r
median(DF_Sum_Filled_In$Total_Steps)
```

```
## [1] 10771.2
```

## Are there differences in activity patterns between weekdays and weekends?


First before the data is split up into weekends and weekdays the date must be formated to be a date not a character. Following this a new column is added called weekday where the day of the week for each date is populated. Once that has been accomplished an if statement is used to create a column called Weekend_Or_Not to populate whether a day is a weekend or weekday.


```r
#format date column as date
Data_Filled_In$date <- as.Date(Data_Filled_In$date)


#create a new column called date
Data_Filled_In$Weekday <- weekdays(Data_Filled_In$date)

#use an if statement to populate weekend or weekday
Data_Filled_In$Weekend_Or_Not <- ifelse(Data_Filled_In$Weekday==c("Saturday","Sunday"),"Weekend", "Weekday")

str(Data_Filled_In)
```

```
## 'data.frame':	17568 obs. of  6 variables:
##  $ X             : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ steps         : num  0 0 0 0 0.8 ...
##  $ date          : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval      : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ Weekday       : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ Weekend_Or_Not: chr  "Weekday" "Weekday" "Weekday" "Weekday" ...
```



 Using the subset function separate the data into 2 sets using the Weekend_Or_Not column. Once the data has been separated create aggregates calculating the average by interval. Rename the columns of each dataset. Plot the graphs to compare each graph on the same scale (from 0 to 300).


```r
#Subset data set into weekend and weekday
Data_Set_Weekend <- subset(Data_Filled_In, Data_Filled_In$Weekend_Or_Not=="Weekend")
Data_Set_Weekday <- subset(Data_Filled_In, Data_Filled_In$Weekend_Or_Not=="Weekday")

#Aggregate data for Weekend and Weekday datasets
DF_SUM_Weekend <- aggregate(Data_Set_Weekend$steps, by=list(Interval = Data_Set_Weekend$interval), FUN = mean, na.rm=TRUE)
DF_SUM_Weekday <- aggregate(Data_Set_Weekday$steps, by=list(Interval = Data_Set_Weekday$interval), FUN = mean, na.rm=TRUE)

#Rename Columns
names(DF_SUM_Weekday) <- c("Interval","Step_Number")
names(DF_SUM_Weekend) <- c("Interval","Step_Number")
```

```r
# Set display panel to display graphs side by side
par(mfrow=c(1,2))

#Plot Data Sets to compare
plot(y = DF_SUM_Weekday$Step_Number, x = DF_SUM_Weekday$Interval, type = "l", xlab = "5 Min Interval", 
     main = "Activity on Weekdays", ylab = "Average steps", ylim=c(0,300))
plot(y = DF_SUM_Weekend$Step_Number, x = DF_SUM_Weekend$Interval, type = "l", xlab = "5 Min Interval", 
     main = "Activity on Weekends", ylab = "Average steps",ylim=c(0,300))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

Looking at the graphs we see that on average weekends have much more steps than weekdays during 5 minute intervals.
