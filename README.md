# CyclisticBikeShare
Data and Analysis

# Introduction
This is a Google Data Analytics Capstone Project and I have selected the Cyclistic Bike Share case study for analysis.
About the Company
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.

## Scenario
Scenario You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.


# Ask
## Business Task
•	How do annual members and casual riders use Cyclistic bikes differently?

•	And how the Cyclistic Bike Share can develop digital marketing strategies to convert Casual riders to Annual members.

## Key Stakeholders
•	Primary Skateholder: Director of Marketing, Lily Moreno and the Cyclistic Executive team

•	Secondary Stakeholder: Cyclistic Marketing Analytics Team



# Prepare

## Description of Data Source

### Data Source: 

12 Month (June 2021 to May 2022) of Cyclistic Bike Share trip data here made available by Motivate International Inc. data source under this license. The dataset has 12 CSV, 13 columns and about 5.9 million rows. The data is ROCCC.

#### Reliability: 
The data includes complete and accurate ride data from Divvy. https://ride.divvybikes.com/system-data.

#### Original: 
The data is from Motivate International Inc, which operates the City of Chicago’s Divvy bicycle sharing service.

#### Comprehensive: 
The data incudes type of bikes, ride_id, rideable_type, started_at, ended_at, start_station_name,, start_station_id, end_station_name, end_station_id, start_latitude, start_longitude, end_latitude, end_longitude, member vs casual memberships types.

#### Current: 
Up to date to May 2022.

#### Cited: 
The data is cited and under current license agreement. The dataset limitations: There were limitations prohibiting us from using riders’ personally identifiable information. This means that we won’t be able to connect pass purchases to credit card numbers to determine if casual riders live in the Cyclistic service area or if they have purchased multiple single passes.






#### Load require packages
Installed all the required library .

#### Load the packages.
```{r}
install.packages('tidyverse')
install.packages('skimr')
library(tidyverse) #helps wrangle data
library(dplyr) #helps clean data
library(lubridate)  #helps wrangle date attributes
library(skimr) #helps get summary data
library(ggplot2) #helps visualize data
library(readr)
```

### STEP 1: COLLECT DATA
#Collect data 
```{r}
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
```



### STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

#Comparing Column names of each file and Check for Column consistency
```{r}
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
```


#Checking for incongruencies
```{r}
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
```


#Combine the data from June 2021 to May 2022 into one data frame.
```{r}
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

```

:
### STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#Examine the dataframe

```{r}
head(bike_trip_data)
colnames(bike_trip_data)
summary(bike_trip_data)
str(bike_trip_data)
dim(bike_trip_data)
nrow(bike_trip_data)
```


#Drop columns we don't need: start_lat, start_lng, end_lat, end_lng
```{r}
bike_trip_data <- bike_trip_data %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

```{r}
colnames(bike_trip_data)
```


# Process

## CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

#CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS


#Examine the dataframe
```{r}
head(bike_trip_data)
colnames(bike_trip_data)
summary(bike_trip_data)
str(bike_trip_data)
dim(bike_trip_data)
nrow(bike_trip_data)
```


#Drop columns we don't need: start_lat, start_lng, end_lat, end_lng
```{r}
bike_trip_data <- bike_trip_data %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

```{r}
colnames(bike_trip_data)
```

#### Add columns that list the date, month, day, and year of each ride

#Add columns that list the date, month, day, and year of each ride
#This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
```{r}
bike_trip_data$date <- as.Date(bike_trip_data$started_at) 
bike_trip_data$month <- format(as.Date(bike_trip_data$date), "%m")
bike_trip_data$day <- format(as.Date(bike_trip_data$date), "%d")
bike_trip_data$year <- format(as.Date(bike_trip_data$date), "%Y")
bike_trip_data$day_of_week <- format(as.Date(bike_trip_data$date), "%A")
```


#### Add a "ride_length" calculation to all_trips (in seconds)
```{r}
bike_trip_data$ride_length <- difftime(bike_trip_data$ended_at,bike_trip_data$started_at)
```



#### Inspect the structure of the columns
```{r}
str(bike_trip_data)
```


#### Convert "ride_length" from Factor to numeric so we can run calculations on the data
```{r}
is.factor(bike_trip_data$ride_length)
bike_trip_data$ride_length <- as.numeric(as.character(bike_trip_data$ride_length))
is.numeric(bike_trip_data$ride_length)
```


#Deleting rows which have negative value in “ride_length”

#Removing the bad data and do analysis on the ride length.

#### The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
```{r}
skim(bike_trip_data$ride_length)
```


#We will create a new version of the dataframe (v2) since data is being removed

#### We will create a new version of the dataframe (v2) since data is being removed
```{r}
bike_trip_data_v2 <- bike_trip_data[!(bike_trip_data$ride_length<0),]

skim(bike_trip_data_v2)
```


# Analyze

## Identify trends and relationships

### STEP 4: CONDUCT DESCRIPTIVE ANALYSIS


#### Descriptive analysis on ride_length (all figures in seconds)
```{r}
mean(bike_trip_data_v2$ride_length)#straight average (total ride length / rides)
median(bike_trip_data_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(bike_trip_data_v2$ride_length) #longest ride
min(bike_trip_data_v2$ride_length) #shortest ride
```


#### You can condense the four lines above to one line using summary() on the specific attribute
```{r}
summary(bike_trip_data_v2$ride_length)
```

#### Compare members and casual users. Aggregate to analyze the data based on user type: member vs casual users

```{r}
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = mean)
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = median)
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = max)
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual, FUN = min)
```


#average ride time by each day for members vs casual users

```{r}
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual + bike_trip_data_v2$day_of_week, FUN = mean)
```


#### Notice that the days of the week are out of order. Let's fix that.
```{r}
bike_trip_data_v2$day_of_week <- ordered(bike_trip_data_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```



#### Now, let's run the average ride time by each day for members vs casual users
```{r}
aggregate(bike_trip_data_v2$ride_length ~ bike_trip_data_v2$member_casual + bike_trip_data_v2$day_of_week, FUN = mean)
```

#### Analyze ridership data by type and weekday
```{r}
bike_trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%                  #groups by usertype and weekday
  summarise(number_of_rides = n()                       #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%  # calculates the average duration
  arrange(member_casual, weekday)                       # sorts
```

#### Let's visualize the number of rides by rider type
```{r}
par(mfrow=c(2,2))

bike_trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```


#### Let's create a visualization for average duration
```{r}
bike_trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```



#### Check for peak time for bike usage between member vs casual
```{r}
hour_data <- bike_trip_data_v2
hour_data$start_hour <- as.numeric(format(strptime(bike_trip_data_v2$started_at,"%Y-%m-%d %H:%M:%OS"),'%H'))
ggplot(data = hour_data) + 
  geom_bar(mapping = aes(x = start_hour, fill = member_casual), stat = 'count') + 
  facet_wrap(~factor(day_of_week)) +
  labs(title = "bike usage by starting hour", x = "starting hour") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
```

![Weekly Bike usage by hour by rider type](https://user-images.githubusercontent.com/106181129/177826556-743f94f5-c873-4098-8fef-9f86237ca7b5.png)


#### Supporting visualizations and key findings**

#Weekly average duration per rider type
```{r}
bike_trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

![Weekly Average duration per rider type](https://user-images.githubusercontent.com/106181129/177826409-3ce39c05-9cb4-442f-b7f9-201414a7af91.png)

#Weekly number of rides by rider type
```{r}
par(mfrow=c(2,2))

bike_trip_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```
![Weekly number of rides by rider type](https://user-images.githubusercontent.com/106181129/177826473-90132bec-dc7f-46c9-94ce-1b81138317eb.png)


# Share

## Supporting visualizations and key findings

Tableau Cyclistic Bike Share Dashboard https://public.tableau.com/app/profile/kwame.owusu.baah/viz/CyclisticBikeShareDashBoard/CyclisticBikeShareDashboard

Tableau Cyclistic Bike Share Story https://public.tableau.com/app/profile/kwame.owusu.baah/viz/CyclisticBikeShare_16566198183380/CyclisticBikeShare


## A summary of your analysis

•	Casual riders ride more so in the warmer months of Chicago, summer season. That is June, July and August. The used the Cyclistic’s bike services more than the Registered annual members.

•	Casual riders ride more during the weekends, that is on Saturdays and Sundays whilst the Registered annual members ride more during the weekdays from Monday to Friday which are typical days of work.

•	Casual riders ride for longer trips or distances that the Registered annual members.

•	Most popular station for casual riders are Streeter Dr & Grand Ave, Millennium Park, Michigan Ave & Oak St, Wells St & Concord Ln and Clark St & Elm St.


# Act

## Recommendation

•	Marketing campaigns and promotions should be concentrated mostly at the Top 5 popular stations where the Casual riders mostly use, namely Streeter Dr & Grand Ave, Millennium Park, Michigan Ave & Oak St, Wells St & Concord Ln and Clark St & Elm St.

•	Marketing campaigns and promotions should be from March to August. Increasing marketing campaigns during springs through to the end of the summer which will be an incentive for casual who plan to ride more often during the summer. And also the Marketing campaigns and promotions should target casual members based on their prime usage periods which is mostly weekends and summer.

•	Design new Point-award incentive system and incentives for riders who ride for long trips or distance in a form membership discounts when they are renewing their annual membership
add Codeadd Markdown

