R Reproducible Research Assignment 1
========================================================

This Markdown file is my submission for the assignment.

## Loading and preprocessing the data

The file is downloaded from the URL given in the assignment. 


```r
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        ,temp)
data <- read.csv(unz(temp,"activity.csv"))
unlink(temp)
```

Next, a version of the file is made without the NA values in "steps."


```r
good_data <- subset(data,data$steps!="NA")
```
## What is mean total number of steps taken per day?

First we make the histogram. Some extra arguments were added for presentation purposes.


```r
# Use tapply to make a Vector of total steps per day
# The "date" column is a factor class already
totals <- tapply(good_data$steps,good_data$date,FUN=sum)
hist(totals,col = "blue", main = "Histogram of Total Steps per Day", xlab = "Number of Steps", breaks=8)
```

![plot of chunk Histogram](figure/Histogram.png) 

Next, the mean and median per day.


```r
library(xtable)
mean <- mean(totals, na.rm=TRUE)
median <- median(totals, na.rm=TRUE)
table <- data.frame(mean = mean, median = median)
xt <- xtable(table)
print(xt, type="html",include.rownames=FALSE)
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Thu Jun 12 01:49:32 2014 -->
<TABLE border=1>
<TR> <TH> mean </TH> <TH> median </TH>  </TR>
  <TR> <TD align="right"> 10766.19 </TD> <TD align="right"> 10765 </TD> </TR>
   </TABLE>

## What is the average daily activity pattern?

Average is another word for mean and good_data has no NA values, so getting the average for each interval is just a tapply call with mean as the function.


```r
average_per_interval <- tapply(good_data$steps,good_data$interval,FUN=mean)
```

For the graph, I want to convert the interval from numbers between 0 and 2355 to times between 00:00 and 23:55.


```r
# Create x-axis labels.
times <- seq(from = 0000, to = 2400, by = 100) #make digits
times <- formatC(times, width = 4, format = "d", flag = "0") #make into xxxx
times <- strftime(strptime(as.character(times),"%H%M"), "%H:%M") #make into xx:xx

##plot graph

plot(average_per_interval, type="l", xaxt="n", xlab="Time (24hr clock)", 
     ylab="Average Number of Steps")
axis(1, at=seq(from = 0, to = 288, by = 12), labels=times, pos=-8.5, las=0)
```

![plot of chunk Average per interval graph](figure/Average per interval graph.png) 

Return the Interval with the highest average number of steps.


```r
#find max and location of max
max.val <- max(average_per_interval)
max.loc <- which.max(average_per_interval)

#Find the interval where max occurs and convert to form "hh:mm - hh:mm""
intervals <- levels(factor(good_data$interval))
max.interval <- intervals[max.loc]
max.interval <- c(as.numeric(max.interval), as.numeric(max.interval) + 5)
max.interval <- formatC(max.interval, width = 4, format = "d", flag = "0")
max.interval <- strftime(strptime(as.character(max.interval),"%H%M"), "%H:%M")
max.interval <- paste(max.interval[1], "-", max.interval[2])

#Make Table
max.table <- data.frame(interval = max.interval, steps = max.val)
xt <- xtable(max.table )
print(xt, type="html",include.rownames=FALSE)
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Thu Jun 12 01:49:32 2014 -->
<TABLE border=1>
<TR> <TH> interval </TH> <TH> steps </TH>  </TR>
  <TR> <TD> 08:35 - 08:40 </TD> <TD align="right"> 206.17 </TD> </TR>
   </TABLE>

## Inputing missing values

As we created a new dataset without NA values previously, we can find the number of missing entries by finding the difference between number of rows in each set.


```r
nrow(data)-nrow(good_data)
```

```
## [1] 2304
```

For filling in the missing data, we're going to put in the mean values for that interval.


```r
#create new dataframe for fixed data
fixed.data <- data

#for loop will run over each row and replace the NA value with the mean for that interval
for (row in 1:nrow(fixed.data)){
        if (is.na(fixed.data$steps[row])){
                interval.set <- fixed.data$interval[row] #find interval
                steps.new <- subset(average_per_interval, 
                                    rownames(average_per_interval)==interval.set)
                fixed.data$steps[row] <- steps.new
        }
        
}
```

Now we will make a new histogram.


```r
totals2 <- tapply(fixed.data$steps,fixed.data$date,FUN=sum)
hist(totals2,col = "blue", main = "Fixed data", xlab = "Number of Steps", breaks=8)
```

![plot of chunk histogram 2](figure/histogram 2.png) 

Note that the Frequency is now higher for each bar and the jump is bigger as we get closer to the mean. This is because we are adding more values to the plot. Since the values we are adding are the mean per interval, the figures are skewed towards the overall mean.

Now we will find the new mean and medians.


```r
mean2 <- mean(totals2, na.rm=TRUE)
median2 <- median(totals2, na.rm=TRUE)
table <- data.frame(mean = c(mean, mean2, mean-mean2), median = c(median, median2, median-median2), row.names=c("Old","New","Change"))
xt <- xtable(table)
print(xt, type="html")
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Thu Jun 12 01:49:32 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> mean </TH> <TH> median </TH>  </TR>
  <TR> <TD align="right"> Old </TD> <TD align="right"> 10766.19 </TD> <TD align="right"> 10765.00 </TD> </TR>
  <TR> <TD align="right"> New </TD> <TD align="right"> 10766.19 </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> Change </TD> <TD align="right"> 0.00 </TD> <TD align="right"> -1.19 </TD> </TR>
   </TABLE>
