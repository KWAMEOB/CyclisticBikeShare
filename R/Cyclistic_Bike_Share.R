
#Load the packages.
install.packages('tidyverse')
install.packages('skimr')
library(tidyverse) #helps wrangle data
library(dplyr) #helps clean data
library(lubridate)  #helps wrangle date attributes
library(skimr) #helps get summary data
library(ggplot2) #helps visualize data
library(readr)


#=====================
# STEP 1: COLLECT DATA
#=====================

#Collect Data
getwd() #displays your working directory
setwd("/Users/nanaowusu/desktop/12 months of Cyclistic trip data(202205-202106(csv files)")

Trip_May_2022<-read.csv('202205-divvy-tripdata.csv')
Trip_Apr_2022<-read.csv('202204-divvy-tripdata.csv')
Trip_Mar_2022<-read.csv('202203-divvy-tripdata.csv')
Trip_Feb_2022<-read.csv('202202-divvy-tripdata.csv')
Trip_Jan_2022<-read.csv('202201-divvy-tripdata.csv')
Trip_Dec_2021<-read.csv('202112-divvy-tripdata.csv')
Trip_Nov_2021<-read.csv('202111-divvy-tripdata.csv')
Trip_Oct_2021<-read.csv('202110-divvy-tripdata.csv')
Trip_Sep_2021<-read.csv('202109-divvy-tripdata.csv')
Trip_Aug_2021<-read.csv('202108-divvy-tripdata.csv')
Trip_Jul_2021<-read.csv('202107-divvy-tripdata.csv')
Trip_Jun_2021<-read.csv('202106-divvy-tripdata.csv')



#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================


#Comparing Column names of each file and Check for Column consistency
colnames(Trip_May_2022)
colnames(Trip_Apr_2022)
colnames(Trip_Mar_2022)
colnames(Trip_Feb_2022)
colnames(Trip_Jan_2022)
colnames(Trip_Dec_2021)
colnames(Trip_Nov_2021)
colnames(Trip_Oct_2021)
colnames(Trip_Sep_2021)
colnames(Trip_Aug_2021)
colnames(Trip_Jul_2021)
colnames(Trip_Jun_2021)

# Checking for incongruencies
str(Trip_May_2022)
str(Trip_Apr_2022)
str(Trip_Mar_2022)
str(Trip_Feb_2022)
str(Trip_Jan_2022)
str(Trip_Dec_2021)
str(Trip_Nov_2021)
str(Trip_Oct_2021)
str(Trip_Sep_2021)
str(Trip_Aug_2021)
str(Trip_Jul_2021)
str(Trip_Jun_2021)


#Combine the data from June 2021 to May 2022 into one data frame.
bike_trip_data<-rbind(Trip_May_2022,
                      Trip_Apr_2022,
                      Trip_Mar_2022,
                      Trip_Feb_2022,
                      Trip_Jan_2022,
                      Trip_Dec_2021,
                      Trip_Nov_2021,
                      Trip_Oct_2021,
                      Trip_Sep_2021,
                      Trip_Aug_2021,
                      Trip_Jul_2021,
                      Trip_Jun_2021)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================


#Examine the dataframe
head(bike_trip_data)
colnames(bike_trip_data)
summary(bike_trip_data)
str(bike_trip_data)
dim(bike_trip_data)
nrow(bike_trip_data)


#Drop columns we don't need: start_lat, start_lng, end_lat, end_lng
bike_trip_data <- bike_trip_data %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

colnames(bike_trip_data)


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
bike_trip_data$date <- as.Date(bike_trip_data$started_at)
bike_trip_data$month <- format(as.Date(bike_trip_data$date), "%m")
bike_trip_data$day <- format(as.Date(bike_trip_data$date), "%d")
bike_trip_data$year <- format(as.Date(bike_trip_data$date), "%Y")
bike_trip_data$day_of_week <- format(as.Date(bike_trip_data$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
bike_trip_data$ride_length <- difftime(bike_trip_data$ended_at,bike_trip_data$started_at)

# Inspect the structure of the columns
str(bike_trip_data)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(bike_trip_data$ride_length)
bike_trip_data$ride_length <- as.numeric(as.character(bike_trip_data$ride_length))
is.numeric(bike_trip_data$ride_length)

#Removing the bad data and do analysis on the ride length.
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
skim(bike_trip_data$ride_length)

# We will create a new version of the dataframe (v2) since data is being removed
bike_trip_data_v2 <- bike_trip_data[!(bike_trip_data$ride_length<0),]
skim(bike_trip_data_v2)



# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================


# Descriptive analysis on ride_length (all figures in seconds)
mean(bike_trip_data_v2$ride_length)#straight average (total ride length / rides)
median(bike_trip_data_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(bike_trip_data_v2$ride_length) #longest ride
min(bike_trip_data_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(bike_trip_data_v2$ride_length)

# Compare members and casual users. Aggregate to analyze the data based on user type: member vs casualusers
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = mean)
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = median)
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = max)
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = min)

#average ride time by each day for members vs casual users
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual + bike_trip_data_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
bike_trip_data_v2$day_of_week <- ordered(bike_trip_data_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual + bike_trip_data_v2$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday
bike_trip_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
par(mfrow=c(2,2))

bike_trip_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)  %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
bike_trip_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)  %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Check for peak time for bike usage between member vs casual
hour_data <- bike_trip_data_v2
hour_data$start_hour <- as.numeric(format(strptime(bike_trip_data_v2$started_at,"%Y-%m-%d %H:%M:%OS"),'%H'))
ggplot(data = hour_data) +
  geom_bar(mapping = aes(x = start_hour, fill = member_casual), stat = 'count') +
  facet_wrap(~factor(day_of_week)) +
  labs(title = "bike usage by starting hour", x = "starting hour") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file

counts <- aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual + bike_trip_data_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/12 months of Cyclistic trip data(202205-202106(csv files)/avg_ride_length.csv')

#total and average weekly rides by rider type
summary_ride_weekly <- bike_trip_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

write_csv(summary_ride_weekly, "summary_ride_weekly.csv")


#total and average weekly rides by rider type
summary_ride_weekly_type <- bike_trip_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday, rideable_type) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

write_csv(summary_ride_weekly_type, "summary_ride_weekly_type.csv")

#total and avg monthly rides by rider type
summary_month <- bike_trip_data_v2 %>%
  mutate(month = month(started_at, label = TRUE)) %>%
  group_by(month,member_casual) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(month, member_casual)
write_csv(summary_month, "summary_ride_monthly.csv")

#total and avg monthly rides by rider type
summary_month <- bike_trip_data_v2 %>%
  mutate(month = month(started_at, label = TRUE)) %>%
  group_by(month,member_casual) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(month, member_casual)
write_csv(summary_month, "summary_ride_monthly.csv")

#most popular stations
popular_stations <- bike_trip_data_v2 %>%
  mutate(station = start_station_name) %>%
  drop_na(start_station_name) %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_rides=n())

# Exclude entries with no station name
popular_stations <- popular_stations[!(popular_stations$start_station_name == "" | is.na(popular_stations$start_station_name)),]
head(popular_stations)
write_csv(popular_stations, "popular_stations.csv")
head(popular_stations)
# Separate the popular_stations data frame by rider type
popuplar_stations_member <- popular_stations[popular_stations$member_casual == 'member',]
popular_stations_casual <- popular_stations[popular_stations$member_casual == 'casual',]
head(popular_stations_casual)
head(popuplar_stations_member)
# Get the top 10 popular stations all, members and casual riders
top_10_station <- popular_stations %>%
  group_by(start_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides)) %>%
  slice(1:10)
head(top_10_station)

#total membership types and rideable types
total_riders <- data.frame(table(bike_trip_data_v2$member_casual))
total_types <- data.frame(table(bike_trip_data_v2$rideable_type))
write_csv(total_riders, "total_riders.csv")
write_csv(total_types, "total_types.csv")








