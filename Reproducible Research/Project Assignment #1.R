#######################################
##  Reproducible Research
##  Project Assignment #1
#######################################

setwd("/Users/Amilcar/Desktop/DataScience/datasciencecoursera/Reproducible Research")


### load the dataset

act<- read.csv("activity.csv")
head(act)

### Change the variables to a suitable class for analysis
act$steps<- as.numeric(act$steps)
act$date<- as.Date(act$date, format="%Y-%m-%d")
act$interval<- as.factor(act$interval)


#########################
#  Data Analysis
#########################

# make a histogram of the total number of steps taken each day

total.steps<- tapply(act$steps, act$date, sum)
library(ggplot2)
qplot(total.steps, main="Total number of Steps each day", ylab="Frequency", xlab="Total number of steps each day")

# mean and median
summary(total.steps)

###### Calculate the average steps taken by interval across all days
mean.interval<- tapply(act$steps, act$interval, mean, na.rm=TRUE)
interval.new<-unique(act$interval) 
average.interval<- data.frame(mean.interval, interval.new)

## the intervals have to be continues for ggplot to use it as xlab
average.interval$interval.new<-as.numeric(as.character(average.interval$interval.new))                                                                                                
qplot(interval.new, mean.interval, data=average.interval, geom="line", main="Average number of steps taken by 5 minute interval", xlab="5-minute interval", ylab="Average number of steps taken")

## Which interval has the maximum average number of steps

summary(average.interval$mean.interval)
sub<- subset(average.interval, average.interval$mean.interval>=206)
sub


##### Imputing missing data

summary(act$steps)
miss<- complete.cases(act)
act.miss<-data.frame(act, miss)

## Imputation method: use the average of steps or the interval across all days
for (i in 1:length(act$steps))  {
        if (act.miss[i,4]=="FALSE"){
            
            for (j in 1:length(average.interval$mean.interval)){
                if (average.interval[j,2] == act.miss[i,3]){
                  
                  act.miss[i,1]<-average.interval[j,1]
                }else{
                
                }
              
            }
          
        }else{
          
        }
}
### to verify if there any missing values (we are suppose to have none)
test<-complete.cases(act.miss)
table(test)

### Make a histogram with the imputed data
total.steps.miss<- tapply(act.miss$steps, act.miss$date, sum)
qplot(total.steps.miss, main="Total number of Steps each day", ylab="Frequency", xlab="Total number of steps each day")
 
#### 
summary(total.steps.miss)
summary(total.steps)

##### What is the effect of imputing data 
boxplot(total.steps, total.steps.miss, main="Distribution of Total number of steps (Non Imputed data vs Imputed", names=c("Non Imputed","Imputed"), col=c("red","lightblue"))


### Are there differences in activity patterns between weekdays and weekends?
library(timeDate)
weekday<- isWeekday(act.miss$date)
act.miss<-data.frame(act.miss,weekday)
act.miss$weekday<- factor(act.miss$weekday, level=c("TRUE","FALSE"), labels=c("Weekday", "Weekend"))
# Get the mean steps by interval by weekday or weekend
week.data<-aggregate(act.miss$steps, list(act.miss$interval, act.miss$weekday), mean)
# Before plotting we need to convert the intervals in a conitnous variable
week.data$Group.1<- as.numeric(as.character(week.data$Group.1))
ggplot(week.data, aes(Group.1, x)) + labs(title="Average Total number of steps taken each day by Weekday or Weekend", x="5-minute Intervals", y="Mean total number of steps") + geom_line() + facet_wrap(~Group.2)
























