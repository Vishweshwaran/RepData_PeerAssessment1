#1.Load File - Code for reading in the dataset and/or processing the data
unzip("activity.zip")
data <- read.csv("activity.csv", header = T, sep = ",")

#Calulate no of steps per day
sum_of_steps <- tapply(data$steps, data$date, sum, na.rm=T)

#2. Plot sum of steps per day - Histogram of the total number of steps taken each day
hist(sum_of_steps,breaks = 10, col=5,xlab = "sum of steps per day", main = "histogram of steps per day")

#3. Find mean and median - Mean and median number of steps taken each day
mean_sum_of_steps <- mean(sum_of_steps)
median_sum_of_steps <- median(sum_of_steps)

#Print mean and median
print(c("mean of the total number of steps taken per daymean is:",mean_sum_of_steps, "median of the total number of steps taken per day is:",median_sum_of_steps))

#Calulate mean no of steps for interval
min_int <- tapply(data$steps, data$interval, mean, na.rm=T)

#4. Plot Interval Vs Average no of steps - Time series plot of the average number of steps taken
plot(min_int ~ unique(data$interval), col=2,type="l", xlab = "5 mins interval",ylab="Average no of steps")

#5. Find interval with max no of steps - The 5-minute interval that, on average, contains the maximum number of steps
max_no_of_steps_int<-min_int[which.max(min_int)]

#Print interval with max no of steps and Average no of steps
print(c("5 minute interval with max no of steps: ","No of steps:",max_no_of_steps_int))

#6. Find missing values-Code to describe and show a strategy for imputing missing data
step_missing_values<-sum(is.na(data$steps))
date_missing_values<-sum(is.na(data$date))
interval_missing_values<-sum(is.na(data$interval))

#Print Summary of missing values
print(c("step missing values:",step_missing_values,"date missing values:",date_missing_values,"interval missing values:",interval_missing_values))

#Impute NA values
data1 <- data
for (i in 1:nrow(data)){
  if(is.na(data$steps[i])){
    data1$steps[i]<- min_int[[as.character(data[i, "interval"])]]
  }
}
#7. Histogram of the total number of steps taken each day after missing values are imputed
sum_of_steps1 <- tapply(data1$steps, data1$date, sum, na.rm=T)
hist(sum_of_steps1,breaks = 10, col=5,xlab = "sum of steps per day", main = "histogram of steps per day")

#Find mean and median
mean_sum_of_steps1 <- mean(sum_of_steps1)
median_sum_of_steps1 <- median(sum_of_steps1)

#Print mean and median
print(c("mean of the total number of steps taken per daymean is:",mean_sum_of_steps1, "median of the total number of steps taken per day is:",median_sum_of_steps1))

#Observation
#We can observe that both mean and median of imputed data are higher than the mean and median of raw data, suprisingly mean and median for the imputed data is same. 

#Convert date from factor to date 
data1$date<-as.Date(data1$date)

#Creating a factor variable "day "to store the day of the week:
data1$day <- as.factor(weekdays(data1$date))

#Create logical variable "weekday" (weekday=TRUE, weekend = FALE) :
data1$weekday <- ifelse(!(data1$day %in% c("Saturday","Sunday")), TRUE, FALSE) 


#Average number of steps for weekdays
weekdays_data <- data1[data1$weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)


#Average number of steps for weekends
weekends_data <- data1[!data1$weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

#Add columns names
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
#Adding a column to indecate the day
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

#Merging the two day types together
week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)
#Converting the day variabke to a factor
week_data$day <- as.factor(week_data$day)

#8. Plotting the data-Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, col=3,layout = c(1,2), type ="l", ylab="Number of Steps")