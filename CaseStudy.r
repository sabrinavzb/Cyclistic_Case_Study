---
"All_trips_v2.Case_study"
date: "2023-01-27"
author: Sabrina Vazquez 

# Cyclistic 
#How do annual members and casual riders use Cyclistic bikes differently?


#load packages
library(tidyverse)
library(lubridate) 
library(ggplot2) 

getwd()

#load dataframes 
Jan_2022 <- read_csv("Jan.csv")
Feb_2022 <- read_csv("Feb.csv")
Mar_2022 <- read_csv("Mar.csv")
Apr_2022 <- read_csv("Apr.csv")
May_2022 <- read_csv("May.csv")
Jun_2022 <- read_csv("Jun.csv")
Jul_2022 <- read_csv("Jul.csv")
Aug_2022 <- read_csv("Aug.csv")
Sep_2022 <- read_csv("Sep.csv")
Oct_2022 <- read_csv("Oct.csv")
Nov_2022 <- read_csv("Nov.csv")
Dec_2022 <- read_csv("Dec.csv")

#The colnames() and str() functions were used to ensure consistency in the naming and structure of columns across all data files, facilitating the process of merging them and creating a comprehensive data frame.

colnames(Jan_2022)
colnames(Feb_2022)
colnames(Mar_2022)
colnames(Apr_2022)
colnames(May_2022)
colnames(Jun_2022)
colnames(Jul_2022)
colnames(Aug_2022)
colnames(Sep_2022)
colnames(Oct_2022)
colnames(Nov_2022)
colnames(Dec_2022)

str(Jan_2022)
str(Feb_2022)
str(Mar_2022)
str(Apr_2022)
str(May_2022)
str(Jun_2022)
str(Jul_2022)
str(Aug_2022)
str(Sep_2022)
str(Oct_2022)
str(Nov_2022)
str(Dec_2022)

#Merging into a single data frame. 

all_trips <- bind_rows(Jan_2022, Feb_2022, Mar_2022, Apr_2022, May_2022, Jun_2022, Jul_2022, Aug_2022, Sep_2022, Oct_2022, Nov_2022, Dec_2022)

#Remove columns that are not necessary for the analysis.

all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))

#Inspect the new table created.

colnames(all_trips)
nrow(all_trips) 
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#Create new column named "ride_length".

all_trips$duration <- as.numeric(difftime(all_trips$ended_at, all_trips$started_at, units = "sec"))

all_trips <- all_trips %>% rename(ride_length = duration)

#Add columns that list the date, month, day, and year of each ride.

all_trips <- all_trips %>% 
  mutate(month = month(started_at),
         year = year(started_at),
         day = day(started_at),
         day_of_week = wday(started_at, label = TRUE))

#Generate new data set by eliminating all instances of N/A or values less than or equal to 0 in the ride_length column -->refined data frame.

all_trips_v2 <- all_trips[!(all_trips$ride_length == "NA" | all_trips$ride_length<=0),]

#Analysis functions:

mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length)

#Comparison between Members and Casual Users.

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#Create new table to get the average ride time by day and user type.

avg_ride.time_by_day_and_user.type <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#Create new table to get Ridership data.

ridership_data <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%  
  arrange(member_casual, weekday)	

# Create csv files to export the data.

write.csv(all_trips_v2, "all_trips_v2.csv")
write.csv(avg_ride.time_by_day_and_user.type, "avg_ride.time_by_day_and_user.type.csv")
write.csv(ridership_data, "ridership_data.csv")