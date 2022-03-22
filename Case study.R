## Installing the required packages 
install.packages("tidyverse")
library("tidyverse")
install.packages("lubridate")
library("lubridate")
library("dplyr")
library("ggplot2")


## Setting working directory
getwd()
setwd("~/Desktop/Cyclistic/Cyclistic_trip_data_12months")

## Importing datasets 

trip_data_2020_05 <- read.csv("202005-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2020_06 <- read.csv("202006-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2020_07 <- read.csv("202007-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2020_08 <- read.csv("202008-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2020_09 <- read.csv("202009-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2020_10 <- read.csv("202010-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2020_11 <- read.csv("202011-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2020_12 <- read.csv("202012-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2021_01 <- read.csv("202101-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2021_02 <- read.csv("202102-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2021_03 <- read.csv("202103-divvy-tripdata.csv", TRUE, sep = ",")
trip_data_2021_04 <- read.csv("202104-divvy-tripdata.csv", TRUE, sep = ",")

## I'll use various functions to make sure my data imported correctly. 
## Checking column names before joining all data into a single file

colnames(trip_data_2020_05)
colnames(trip_data_2020_06)
colnames(trip_data_2020_07)
colnames(trip_data_2020_08)
colnames(trip_data_2020_09)
colnames(trip_data_2020_10)
colnames(trip_data_2020_11)
colnames(trip_data_2020_12)
colnames(trip_data_2021_01)
colnames(trip_data_2021_02)
colnames(trip_data_2021_03)
colnames(trip_data_2021_04)

## Inspecting the data frames and look for inconsistencies
str(trip_data_2020_05)
str(trip_data_2020_06)
str(trip_data_2020_07)
str(trip_data_2020_08)
str(trip_data_2020_09)
str(trip_data_2020_10)
str(trip_data_2020_11)
str(trip_data_2020_12)
str(trip_data_2021_01)
str(trip_data_2021_02)
str(trip_data_2021_03)
str(trip_data_2021_04)

## Converting data types

trip_data_MayNov <- bind_rows(trip_data_2020_05,trip_data_2020_06,trip_data_2020_07,trip_data_2020_08,trip_data_2020_09,trip_data_2020_10,trip_data_2020_11)

trip_data_MayNov <- mutate(trip_data_MayNov, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id)) 

trip_data_DecApr <- bind_rows(trip_data_2020_12,trip_data_2021_01,trip_data_2021_02,trip_data_2021_03,trip_data_2021_04)

## Binding data frames into one big data frame for further analysis
all_trips <- bind_rows(trip_data_MayNov,trip_data_DecApr)
 
## Checking out the data
glimpse(all_trips)
summary(all_trips)
head(all_trips)
table(all_trips$member_casual)

## Cleaning the data to prepare for analysis
## Firstly, I will add columns that list the date, month, day, year and start time of each ride.

all_trips$started_at <- as_datetime(all_trips$started_at) # converting strings to date-times
all_trips$ended_at <- as_datetime(all_trips$ended_at)
str(all_trips$started_at)
str(all_trips$ended_at)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%B")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$start_time <- format(as.POSIXct(all_trips$started_at), format = "%H:%M")

## Adding a column for calculating trip_duration
all_trips$trip_duration <- difftime( all_trips$ended_at, all_trips$started_at, units="secs")
str(all_trips)
## Converting the data type of trip_duration to numeric for further calculations
all_trips$trip_duration <- as.numeric(all_trips$trip_duration)
is.numeric(all_trips$trip_duration)

summary(all_trips)
## The data has been processed to remove trips that are taken by staff as they service and inspect the system
## and any trips that were below 60 seconds in length (potentially false starts or users trying to re-dock a bike to ensure it was secure).
## To make sure I will remove trips that trip duration is less than 60 seconds or 'start_station_name' column contains invalid entries of HQ QR which means that the bike was taken out by the Cyclistic's team for maintenance.

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$trip_duration<60),]

## Checking the new data frame before analysis
install.packages("sqldf")
library(sqldf)
sqldf( "SELECT * from all_trips_v2
       where ride_id IS NULL")
sqldf("SELECT started_at from all_trips_v2 where started_at IS NULL")
sqldf("SELECT ended_at from all_trips_v2 where ended_at IS NULL")
sqldf("SELECT * from all_trips_v2 where rideable_type IS NULL or start_station_name IS NULL or end_station_name IS NULL or member_casual IS NULL")
sqldf("SELECT DISTINCT member_casual from all_trips_v2") 

which(duplicated(all_trips_v2))

## Now the data is stored appropriately and has been prepared for analysis. 
### DESCRIPTIVE ANALYSIS
all_trips_v3 <- all_trips_v2 %>% arrange(date)

mean(all_trips_v3$trip_duration) 
median(all_trips_v3$trip_duration)
min(all_trips_v3$trip_duration) 
max(all_trips_v3$trip_duration )

## Comparing Member and Casual users
aggregate(all_trips_v3$trip_duration ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$trip_duration ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$trip_duration ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$trip_duration ~ all_trips_v3$member_casual, FUN = min)
## Comparing Member and Casual users by day of the week
aggregate(all_trips_v3$trip_duration ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

## Fixing the order of the days of the week
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v3$trip_duration ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
## Analyzing the ridership data by user type and the days of week
all_trips_v4 <- all_trips_v3 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, day_of_week) %>%  
  summarise(number_of_rides = n(),average_duration = mean(trip_duration)) %>% 		
  arrange(member_casual, day_of_week)		
## Analyzing the ridership data by user type and the month
all_trips_v5 <- all_trips_v3 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>%  
  summarise(number_of_rides = n(),average_duration = mean(trip_duration)) %>% 		
  arrange(member_casual, month)	
## Visualizing the number of rides by user type during weekdays
all_trips_v4 %>% ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Ride by user type during weekdays", x = "Weekday", y= "Number of Rides", caption = "Data from 2020.05 to 2021.04") +
 scale_fill_manual(values = c("member"= "#5FD2AC","casual" = "#2C92D5"))
## Visualizing the number of rides by user type for each month
all_trips_v5 %>% ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Ride by user type for each month", x = "Months", y= "Number of Rides", caption = "Data from 2020.05 to 2021.04") +
  scale_fill_manual(values = c("member"= "#5FD2AC","casual" = "#2C92D5"))
## Visualizing the average trip duration by user type
ggplot(data=all_trips_v4,aes(x=day_of_week, y= average_duration)) + geom_point(aes(color=member_casual)) + scale_colour_manual(values = c("member"= "#5FD2AC","casual" = "#2C92D5"))


write.csv(all_trips_v2, file = '~/Desktop/Cyclistic/Cyclistic_trip_data_12months/all_trips_data.csv')
write.csv(all_trips_v3, file = '~/Desktop/Cyclistic/Cyclistic_trip_data_12months/all_trips_data_v2.csv')
write.csv(all_trips_v4, file = '~/Desktop/Cyclistic/Cyclistic_trip_data_12months/days_of_week.csv')
write.csv(all_trips_v5, file = '~/Desktop/Cyclistic/Cyclistic_trip_data_12months/months.csv')

str(all_trips_v3)


