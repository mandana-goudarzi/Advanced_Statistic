library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(geosphere)

file1 <- read.csv("JC-201902-citibike-tripdata.csv", header = TRUE)
file2 <- read.csv("JC-201903-citibike-tripdata.csv", header=TRUE)
file3 <- read.csv("JC-201904-citibike-tripdata.csv", header=TRUE)
file4 <- read.csv("JC-201905-citibike-tripdata.csv", header = TRUE)
file5 <- read.csv("JC-201906-citibike-tripdata.csv", header=TRUE)

# Part 1
df1 <- as.data.frame(file1)
df2 <- as.data.frame(file2)
df3 <- as.data.frame(file3)
df4 <- as.data.frame(file4)
df5 <- as.data.frame(file5)

# Part 2
df <- do.call("rbind", list(df1, df2, df3, df4, df5))

# Part 3
print(colSums(is.na(df)))

# Part 3
avg <- mean(df$tripduration)
print(paste("The average trip duration:", avg))
med <- median(df$tripduration)
print(paste("the median trip duration is:", med))

# Part 4.1
min <- min(df$tripduration)
print(paste("minimum trip duration is:", min))
max <- max(df$tripduration)
print(paste("maximum trip duration is:", max))

# Part 4.2
print(paste("the minimum and maximum is not plausible since both of them are the same"))

# Part 4.3
filtered <- df |> filter(tripduration < 10800)
avg_filtered <- mean(filtered$tripduration)
print(paste("The average trip duration by excluding trips longer than 3 hours:", avg_filtered))
med_filtered <- median(filtered$tripduration)
print(paste("The median trip duration by excluding trips longer than 3 hours:", med_filtered))
skimmed <- nrow(df) - nrow(filtered)
print(paste("Number of skimmed entries:", skimmed))

# Part 4.4
ggplot(filtered, aes(x = tripduration)) + geom_histogram(binwidth = 60, fill = "skyblue", color = "black") + labs(
    title = "Distribution of Trip Duration (< 3 hours)",
    x = "Trip Duration (seconds)",
    y = "Frequency"
  ) +
  theme_minimal()

# Part 5
df$starttime <- as.POSIXct(df$starttime, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
df$month <- format(df$starttime, "%Y-%m")
monthly_avg <- df %>% group_by(month) %>% summarise(avg_duration_min = mean(tripduration) / 60)
ggplot(monthly_avg, aes(x = month, y = avg_duration_min)) + geom_bar(stat="identity") + labs(
  title = "Monthly Average Trip Duration",
  x = "Month",
  y = "Average Duration (minutes)") + theme_minimal()

# Part 6.1
df$weekday <- wday(df$starttime, label= TRUE, abbr= FALSE)
df$date <- as.Date(df$starttime)
rides_per_day <- df %>% group_by(date, weekday) %>% summarise(rides= n(), .groups = 'drop')
avg_rides_per_weekday <- rides_per_day %>% group_by(weekday) %>% summarise(avg_rides = mean(rides))
ggplot(avg_rides_per_weekday, aes(x = weekday, y = avg_rides)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(
    title = "Average Number of Rides per Weekday",
    x = "Weekday",
    y = "Average Number of Rides"
  ) + theme_minimal()

# Part 6.2
df$hour <- hour(df$starttime)
df$daytype <- ifelse(df$weekday %in% c("Sunday", "Saturday"), "weekend", "weekday")
hourly_dist <- df %>% group_by(hour, daytype) %>% summarise(rides_per_hour = n(), .groups = 'drop')
weekday_plot <- ggplot(filter(hourly_dist, daytype=="weekday"), aes(x= hour, y= rides_per_hour)) + geom_bar(stat= "identity") + labs(title = "Weekday", x = "Hour", y = "Rides") + theme_minimal()
weekend_plot <- ggplot(filter(hourly_dist, daytype=="weekend"), aes(x= hour, y= rides_per_hour)) + geom_bar(stat= "identity") + labs(title = "Weekend", x = "Hour", y = "Rides") + theme_minimal()
weekday_plot + weekend_plot

# Part 6.3
user_type_dist <- df %>% filter(daytype == "weekday") %>% group_by(hour, usertype) %>% summarise(rides_per_hour = n(), .groups = "drop")
customer_plot <- ggplot(filter(user_type_dist, usertype=="Customer"), aes(x= hour, y=rides_per_hour)) + geom_bar(stat= "identity") + labs(title= "Customer", x= "Hour", y = "Rides") + theme_minimal()
subscriber_plot <- ggplot(filter(user_type_dist, usertype=="Subscriber"), aes(x= hour, y=rides_per_hour)) + geom_bar(stat= "identity") + labs(title= "Subscriber", x= "Hour", y = "Rides") + theme_minimal()
customer_plot + subscriber_plot

# Part 7.1
df$dist_m <- distHaversine(matrix(c(df$start.station.longitude, df$start.station.latitude), ncol=2),matrix( c(df$end.station.longitude, df$end.station.latitude), ncol =2))
df$dist_km <- df$dist / 1000
df$duration_hr <- df$tripduration / 3600
df$speed_kmh <- df$dist_km / df$duration_hr
df_hour_filtered <- df %>% filter(tripduration <= 3600 )
mean_speed <- mean(df_hour_filtered$speed_kmh, na.rm= TRUE)
print(paste("average speed (in km/h) of a user", mean_speed ))

# Part 7.2
df$dist_gp <- case_when(
  df$dist_m < 500 ~ "<500 m",
  df$dist_m >= 500 & df$dist_m < 1000 ~ "500–1000 m",
  df$dist_m >= 1000 & df$dist_m < 2000 ~ "1000–2000 m",
  df$dist_m >= 2000 & df$dist_m < 3000 ~ "2000–3000 m",
  df$dist_m >= 3000 ~ ">3000 m"
)
speed_gp <- df %>% group_by(dist_gp) %>% summarise(avg_speed = mean(speed_kmh, na.rm=TRUE))
ggplot(speed_gp, aes(x = dist_gp, y = avg_speed)) +
  geom_col(fill = "orange") +
  labs(title = "Average Speed by Distance Group", x = "Distance Group", y = "Average Speed (km/h)") +
  theme_minimal()

# Part 8.1
most_common_start <- df %>% count(start.station.name, sort=TRUE) %>%slice(1)
least_common_end <- df %>% count(end.station.name, sort=TRUE) %>%slice_tail(n = 1)
print(paste("most common start station:"))
print(most_common_start)
print(paste("least popular end station:" ))
print(least_common_end)

# Part 8.2
routes <- df %>% count(start.station.name, end.station.name, sort = TRUE)
top_routes <- routes %>% slice(1:3)
bottom_routes <- routes %>% slice_tail(n= 3)
print(paste("three most common routes:"))
print(top_routes)
print(paste("three least popular ones:"))
print( bottom_routes)