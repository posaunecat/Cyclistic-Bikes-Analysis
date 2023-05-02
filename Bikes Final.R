#Prepare Rstudio for the import and analysis of the divvy-bikes monthly data sets by loading the tidyverse suite of packages
library(tidyverse)
options(scipen = 999)
library(scales)

#Reading monthly bike data from April 2022 to March 2023 in RStudio for manipulation and analysis
apr <- read_csv("202204-divvy-tripdata.csv")
may <- read_csv("202205-divvy-tripdata.csv")
jun <- read_csv("202206-divvy-tripdata.csv")
jul <- read_csv("202207-divvy-tripdata.csv")
aug <- read_csv("202208-divvy-tripdata.csv")
sep <- read_csv("202209-divvy-tripdata.csv")
oct <- read_csv("202210-divvy-tripdata.csv")
nov <- read_csv("202211-divvy-tripdata.csv")
dec <- read_csv("202212-divvy-tripdata.csv")
jan <- read_csv("202301-divvy-tripdata.csv")
feb <- read_csv("202302-divvy-tripdata.csv")
mar <- read_csv("202303-divvy-tripdata.csv")

#Check the structure of each imported data frame for consistent attributes and data types
str(apr)
str(may)
str(jun)
str(jul)
str(aug)
str(sep)
str(oct)
str(nov)
str(dec)
str(jan)
str(feb)
str(mar)

#View the first few rows of each frame for consistency
head(apr)
head(may)
head(jun)
head(jul)
head(aug)
head(sep)
head(oct)
head(nov)
head(dec)
head(jan)
head(feb)
head(mar)

#Combine frames into one yearly frame
year <- bind_rows(apr,may,jun,jul,aug,sep,oct,nov,dec,jan,feb,mar)

#Create new rows in yearly frame for grouping and summarizing later
#New rows are weekday, abbreviated month, day as number, 4 digit year, hour of start time, and ride time in minutes converted to numeric data type
trips_data <- year %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  mutate(day = mday(started_at)) %>% 
  mutate(year = year(started_at)) %>% 
  mutate(start_time = as.numeric(hour(started_at))) %>% 
  mutate(ride_time = as.numeric((ended_at - started_at)/60))
 
#Filter rides from "docked_bikes"
#Filter rides with negative ride times
#Filter rides with no end station or start station
#Filter rides less than 1 minute for relevance
trips <- trips_data %>% 
  filter(rideable_type != "docked_bike") %>% 
  filter(ride_time > 1) %>% 
  filter(!is.na(end_station_id)) %>% 
  filter(!is.na(start_station_id))
  
#Filter and summarize trips to show mean ride times by membership status over the course of the year
by_month_member <- trips %>%
  filter(member_casual == "member") %>%
  group_by(month, rideable_type) %>% 
  summarize(mean_time = mean(ride_time, na.rm = TRUE), median_time = median(ride_time, na.rm = TRUE), max_time = max(ride_time), min_time = min(ride_time))
  
by_month_casual <- trips %>% 
  filter(member_casual == "casual") %>% 
  group_by(month, rideable_type) %>% 
  summarize(mean_time = mean(ride_time, na.rm = TRUE), median_time = median(ride_time, na.rm = TRUE), max_time = max(ride_time), min_time = min(ride_time))

#Filter and summarize trips to show mean ride times by membership status over the course of the week
by_day_member <- trips %>% 
  filter(member_casual == "member") %>%
  group_by(weekday, rideable_type) %>% 
  summarize(mean_time = mean(ride_time, na.rm = TRUE), median_time = median(ride_time, na.rm = TRUE), max_time = max(ride_time), min_time = min(ride_time))

by_day_casual <- trips %>% 
  filter(member_casual == "casual") %>%
  group_by(weekday, rideable_type) %>% 
  summarize(mean_time = mean(ride_time, na.rm = TRUE), median_time = median(ride_time, na.rm = TRUE), max_time = max(ride_time), min_time = min(ride_time))

#Filter and summarize trips to show number of rides by top 10 most popular start stations, member and casual
by_station_member <- trips %>%
  filter(member_casual == "member") %>% 
  group_by(start_station_name) %>% 
  summarise(num_rides = n()) %>% 
  arrange(desc(num_rides)) %>% 
  slice(1:10)

by_station_casual <- trips %>%
  filter(member_casual == "casual") %>% 
  group_by(start_station_name) %>% 
  summarise(num_rides = n()) %>% 
  arrange(desc(num_rides)) %>% 
  slice(1:10)

#Filter and summarize trips to show top 10 start station with longest ride times
bsm_time <- trips %>% 
  filter(member_casual == "member") %>% 
  group_by(start_station_name) %>% 
  summarise(mean_time = mean(ride_time, na.rm = TRUE)) %>% 
  arrange(desc(mean_time)) %>% 
  slice(1:10)

bsc_time <- trips %>% 
  filter(member_casual == "casual") %>% 
  group_by(start_station_name) %>% 
  summarise(mean_time = mean(ride_time, na.rm = TRUE)) %>% 
  arrange(desc(mean_time)) %>% 
  slice(1:10)

#Filter and summarize monthly trips to show %change month to month
monthly_rides_member <- trips %>%
  filter(member_casual == "member") %>% 
  group_by(month) %>% 
  summarize(total_rides = (n())) %>% 
  arrange(match(month, c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))) %>% 
  mutate(MoM = (total_rides - lag(total_rides))/ lag(total_rides)) %>% 
  mutate(MoM = round(MoM * 100, 1)) %>% 
  filter(!is.na(MoM))

monthly_rides_casual <- trips %>% 
  filter(member_casual == "casual") %>%
  group_by(month) %>% 
  summarize(total_rides = (n())) %>% 
  arrange(match(month, c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar'))) %>% 
  mutate(MoM = (total_rides - lag(total_rides))/ lag(total_rides)) %>% 
  mutate(MoM = round(MoM * 100, 1)) %>% 
  filter(!is.na(MoM))
  

#Plot bar graph for ridership by start time and bike type
ggplot(data = trips) +
  geom_bar(mapping = aes(x = start_time, fill = member_casual), position = "fill", color = "grey") +
  facet_wrap(~rideable_type) +
  scale_fill_manual(values = c("#009E73","#D55E00")) +
  labs(title = "Percentage of Ridership",
       subtitle = "By Bike Type and Member Status",
       fill = "Membership Status",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       x = "Start Time 24-hour clock",
       y = "Ridership Percentage")

#Plot bar graph for ridership by weekday/bike type
ggplot(data = trips) +
  geom_bar(mapping = aes(x = factor(weekday, level = c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')), fill = member_casual), position = "dodge") +
  facet_wrap(~rideable_type) +
  scale_fill_manual(values = c("#009E73","#D55E00")) +
  labs(title = "Daily Rides by Bike Type",
       subtitle = "Casual Vs. Annual Members",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Membership Status",
       x = "Day",
       y = "# of Rides")

#Plot bar graphs to show difference in mean ride time by bike type for members
ggplot(data = by_day_member, mapping = aes(x = weekday, y = mean_time, fill = rideable_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC79A7","#999999")) +
  labs(title = "Mean Ride Time",
       subtitle = "Annual Members by Weekday",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Bike Type",
       x = "Weekday",
       y = "Time in Minutes")

#Plot bar graphs to show difference in mean ride time by bike type for casual riders
ggplot(data = by_day_casual, mapping = aes(x = weekday, y = mean_time, fill = rideable_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC79A7","#999999")) +
  labs(title = "Mean Ride Time",
       subtitle = "Casual Users by Weekday",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Bike Type",
       x = "Weekday",
       y = "Time in Minutes")

#Plot bar graphs for ridership by month/bike type
ggplot(data = trips) +
  geom_bar(mapping = aes(x = factor(month, level = c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar')), fill = member_casual), position = "dodge") +
  facet_wrap(~rideable_type) +
  scale_fill_manual(values = c("#009E73","#D55E00")) +
  labs(title = "Monthly Rides by Bike Type",
       subtitle = "Casual Vs. Annual Members",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Membership Status",
       x = "Month",
       y = "# of Rides") +
  theme(axis.text.x = element_text(angle = 45))

#Plot bar graph to show difference in mean ride time by bike type for members
ggplot(data = by_month_member, mapping = aes(x = month, y = mean_time, fill = rideable_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC79A7","#999999")) +
  labs(title = "Mean Ride Time",
       subtitle = "Annual Members by Month",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Bike Type",
       x = "Month",
       y = "Time in Minutes")

#Plot bar graph to show difference in mean ride time by bike type for casual
ggplot(data = by_month_casual, mapping = aes(x = month, y = mean_time, fill = rideable_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC79A7","#999999")) +
  labs(title = "Mean Ride Time",
       subtitle = "Casual Users by Month",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Bike Type",
       x = "Month",
       y = "Time in Minutes")

#Plot bar graph for number of rides by starting location for members
ggplot(data = by_station_member) +
  geom_col(mapping = aes(x = fct_reorder(start_station_name, num_rides), y = num_rides, fill = start_station_name)) +
  coord_flip() +
  labs(title = "10 Most Popular Starting Stations",
       subtitle = "Total Rides by Annual Members",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Start Station",
       x = "# of Rides",
       y = "Start Station")

#Plot bar graph for number of rides by starting location for casuals
ggplot(data = by_station_casual) +
  geom_col(mapping = aes(x = fct_reorder(start_station_name, num_rides), y = num_rides, fill = start_station_name)) +
  coord_flip() +
  labs(title = "10 Most Popular Starting Stations",
       subtitle = "Total Rides by Casual Riders",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       fill = "Start Station",
       x = "# of Rides",
       y = "Start Station")

#Plot bar graph for mean ride time by starting location for members
ggplot(data = bsm_time) +
  geom_col(mapping = aes(x = fct_reorder(start_station_name, mean_time), y = mean_time, fill = start_station_name)) +
  coord_flip() +
  labs(title = "10 Most Popular Starting Stations",
       subtitle = "Average Ride Time for Annual Members",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       x = "# of Rides",
       y = "Start Station")

#Plot bar graph for mean ride time by starting location for casuals
ggplot(data = bsc_time) +
  geom_col(mapping = aes(x = fct_reorder(start_station_name, mean_time), y = mean_time, fill = start_station_name)) +
  coord_flip() +
  labs(title = "10 Most Popular Starting Stations",
       subtitle = "Average Ride Time for Casual Riders",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       x = "# of Rides",
       y = "Start Station")

ggplot(monthly_rides_member, aes(x = factor(month, level = c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar')), y = MoM)) +
  geom_col(fill = "#D55E00") +
  labs(title = "Change in Member Ridership",
       subtitle = "Month to Month",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       x = "Month",
       y = "Percent Change")

ggplot(monthly_rides_casual, aes(x = factor(month, level = c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar')), y = MoM)) +
  geom_col(fill = "#009E73") +
  labs(title = "Change in Casual Ridership",
       subtitle = "Month to Month",
       caption = "Data from Divvy Bikes Apr '22 - Mar '23",
       x = "Month",
       y = "Percent Change")