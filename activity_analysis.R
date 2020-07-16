stepsPerDay<- function(){
        library(dplyr)
        activity_data <- read.csv("activity.csv", header=T, sep=',', na.strings="?", 
                                   nrows=17568, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
        
        activity_data$steps <- as.numeric(activity_data$steps)
        activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")
        #str(activity_data)
        
        activity_steps_day <- aggregate(steps ~ date, data = activity_data, FUN = sum, na.rm = TRUE)
        hist(activity_steps_day$steps, main="Steps Per Day", 
             xlab="Number of Steps per day", ylab="Frequency", col="blue")
        
        print(paste("Mean of Number of Steps taken per day",mean(activity_steps_day$steps,na.rm = TRUE)))
        print(paste("Median of Number of Steps taken per day",median(activity_steps_day$steps,na.rm = TRUE)))
}
AverageStepsPerInterval <-function(){
        library(dplyr)
        activity_data <- read.csv("activity.csv", header=T, sep=',', na.strings="?", 
                                  nrows=17568, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
        
        activity_data$steps <- as.numeric(activity_data$steps)
        activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")
        activity_steps_interval <- aggregate(steps ~ interval, data = activity_data, FUN = mean, na.rm = TRUE)
        with(activity_steps_interval, {
                plot(activity_steps_interval$steps~activity_steps_interval$interval, type="l",
                     ylab="Average Steps", xlab="Interval")
        })
        max_interval <- which(activity_steps_interval$steps==max(activity_steps_interval$steps))
        print(paste("5-minute interval which contains the maximum number of steps(average)",activity_steps_interval$interval[max_interval]," with ",max(activity_steps_interval$steps)," steps."))
}

InputingNAValues <-function(){
        library(dplyr)
        activity_data <- read.csv("activity.csv", header=T, sep=',', na.strings="?", 
                                  nrows=17568, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
        
        activity_data$steps <- as.numeric(activity_data$steps)
        activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")
        print(paste("Total Number of NA Values in the dataset = ",sum(is.na(activity_data$steps))))
        
        activity_steps_interval <- tapply(activity_data$steps, activity_data$interval, mean, na.rm = TRUE)
        missing_data <- activity_data[is.na(activity_data$steps),]
        non_missing_data <- activity_data[!is.na(activity_data$steps),]
        missing_data$steps <- as.factor(missing_data$interval)
        levels(missing_data$steps) <- activity_steps_interval
        levels(missing_data$steps) <- round(as.numeric(levels(missing_data$steps)))
        missing_data$steps <- as.integer(as.vector(missing_data$steps))
        
        combined_data <- rbind(missing_data, non_missing_data)
        
        print(paste("Total Number of NA Values in the new dataset = ",sum(is.na(combined_data$steps))))
        
        activity_steps_day <- aggregate(steps ~ date, data = combined_data, FUN = sum, na.rm = TRUE)
        hist(activity_steps_day$steps, main="Steps Per Day", 
             xlab="Number of Steps per day", ylab="Frequency", col="blue")
        
        print(paste("Mean of Number of Steps taken per day",mean(activity_steps_day$steps,na.rm = TRUE)))
        print(paste("Median of Number of Steps taken per day",median(activity_steps_day$steps,na.rm = TRUE)))
        
}
weekendWeekdaysAnalysis <- function(){
        library(dplyr)
        library(ggplot2)
        activity_data <- read.csv("activity.csv", header=T, sep=',', na.strings="?", 
                                  nrows=17568, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
        
        activity_data$steps <- as.numeric(activity_data$steps)
        activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")
        weekdays_list <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
        activity_data$dayType <- factor((weekdays(activity_data$date) %in% weekdays_list), 
                           levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
        activity_steps_day <- aggregate(steps ~ interval + dayType, data = activity_data, FUN = mean, na.rm = TRUE)
        names(activity_steps_day) <- c("interval", "dayType", "mean_steps")
        plot <- ggplot(activity_steps_day, aes(interval, mean_steps))
        plot + geom_line(color = "red") + facet_grid(dayType~.) +facet_wrap(dayType~.,ncol=1,strip.position="top")+ labs(x = "Intervals", y = "Average Number of Steps", title = "Activity on Weekends vs Weekdays")

}

