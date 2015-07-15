install.packages("ggplot2")
install.packages("knitr")

library(dplyr); library(ggplot2); library(lattice); library(knitr)

# Loading and preprocessing the data
#1
unzip(zipfile="activity.zip")
df <- read.csv("activity.csv")
df_na <- read.csv("activity.csv")
#2
df$date2 <- as.Date(df$date, "%Y-%m-%d")

# What is mean total number of steps taken per day?
#1
by_df <- group_by(df, date2)
df_sum <- summarise(by_df, steps_per_day = sum(steps))
df_sum$steps_per_day[is.na(df_sum$steps_per_day)] <- 0
#2
ggplot(df_sum, aes(steps_per_day)) + 
        geom_bar(binwidth = 1000) +
        xlab("Total steps per day")
#3
mean(df_sum$steps_per_day)
median(df_sum$steps_per_day)

# What is the average daily activity pattern?
#1
by_df2 <- group_by(df, interval)
df_sum3 <- summarise(by_df2, mean_steps_per_day = mean(steps, na.rm = TRUE))
plot(x = df_sum3$interval, y = df_sum3$mean_steps_per_day, 
     xlab = "5-minute interval", ylab = "Average steps per day", type= "l")
#2
res <- as.matrix(subset(df_sum3, 
       select = c(interval, mean_steps_per_day), 
       mean_steps_per_day == max(mean_steps_per_day)))
interval <- unname(res[1,1])
mean_steps_per_day <- unname(res[1,2])
interval
mean_steps_per_day

# Imputing missing values
#1
na <- df_na[is.na(df_na),]
miss <- nrow(na)
miss
#2
merged <- merge(x = df_na, y= df_sum3, by = "interval" , all.x = TRUE)
merged <- arrange(merged, date)
merged$steps[is.na(merged$steps)] <- merged$mean_steps_per_day[is.na(merged$steps)]
#3
df_imputed <- select(merged, steps, date, interval)
#4
df_imputed$date2 <- as.Date(df_imputed$date, "%Y-%m-%d")
by_df_imputed <- group_by(df_imputed, date2)
df_sum4 <- summarise(by_df_imputed, steps_per_day = sum(steps))
ggplot(df_sum4, aes(x = date2, y = steps_per_day)) + 
        geom_bar(stat = "identity") +
        xlab("Date") +
        ylab("Total steps per day")
mean(df_sum4$steps_per_day)
median(df_sum4$steps_per_day)

# Are there differences in activity patterns between weekdays and weekends?
#1
df_imputed$day <- as.factor(ifelse(weekdays(df_imputed$date2) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
#2
by_df_imputed2 <- group_by(df_imputed, day, interval)
df_sum5 <- summarise(by_df_imputed2, mean_steps_per_day = mean(steps))
xyplot(mean_steps_per_day ~ interval | day, df_sum5, type = "l", layout = c(1,2),
       ylab = "Number of steps", xlab = "Interval")