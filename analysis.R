#Firstly, import library(readr) and unzip folders
#Load all data in
data_2020_12 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202012-divvy-tripdata\\202012-divvy-tripdata.csv")
data_2021_01 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202101-divvy-tripdata\\202101-divvy-tripdata.csv")
data_2021_02 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202102-divvy-tripdata\\202102-divvy-tripdata.csv")
data_2021_03 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202103-divvy-tripdata\\202103-divvy-tripdata.csv")
data_2021_04 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202104-divvy-tripdata\\202104-divvy-tripdata.csv")
data_2021_05 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202105-divvy-tripdata\\202105-divvy-tripdata.csv")
data_2021_06 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202106-divvy-tripdata\\202106-divvy-tripdata.csv")
data_2021_07 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202107-divvy-tripdata\\202107-divvy-tripdata.csv")
data_2021_08 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202108-divvy-tripdata\\202108-divvy-tripdata.csv")
data_2021_09 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202109-divvy-tripdata\\202109-divvy-tripdata.csv")
data_2021_10 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202110-divvy-tripdata\\202110-divvy-tripdata.csv")
data_2021_11 <- read_csv("C:\\Users\\BigHokie\\Documents\\Biking Case Study\\202111-divvy-tripdata\\202111-divvy-tripdata.csv")

#Combine them into a single dataframe and gather the only relevant fields
full_dataframe <- rbind(data_2020_12, data_2021_01, data_2021_02, data_2021_03, data_2021_04, data_2021_05, data_2021_06, data_2021_07, data_2021_08, data_2021_09, data_2021_10, data_2021_11)
relevant_fields = subset(full_dataframe, select = c(rideable_type, started_at, ended_at, start_lat, start_lng, end_lat, end_lng, start_station_name, end_station_name, member_casual, ride_length, day_of_the_week, start_month))

#View(relevant_fields %>% filter(start_station_name == "Michigan Ave & Oak St"))

#Import library(tidyverse)
#Drop all NA rows
no_missings = relevant_fields %>% drop_na()

#Get the number of riders on each day of the week (1 = Sunday, 7 = Saturday)
riders_per_day = no_missings %>% group_by(member_casual, day_of_the_week) %>% summarize(num_riders = n())
ggplot(data = riders_per_day, aes(x = day_of_the_week, y = num_riders/1000, fill = member_casual)) +
  geom_col(colour = "black", position = position_dodge()) +
  xlab("Day of the Week") +
  ylab("Number of Riders (in thousands)") +
  scale_fill_manual(name="Membership\nStatus", values = c("#FF0000", "#000000")) +
  ggtitle("Riders Per Day Based on Membership") +
  theme(plot.title = element_text(hjust = 0.5))

Casuals_by_month = no_missings %>% 
  group_by(member_casual, start_month) %>%
  filter(member_casual == "casual") %>% 
  summarize(num_casuals = n())
ggplot(data = Casuals_by_month, aes(x = start_month, y = num_casuals/1000, fill = member_casual)) +
  geom_col(colour = "black", position = position_dodge()) +
  xlab("Month of the Year") +
  ylab("Number of Riders (in thousands)") +
  scale_fill_manual(name="Membership\nStatus", values = c("#1111BB")) +
  ggtitle("Casual Ridership Per Month") +
  theme(plot.title = element_text(hjust = 0.5))

common_start_locs = no_missings %>% group_by(day_of_the_week, start_station_name) %>% summarize(start_locs = n())
filtered_starts = common_start_locs %>% filter((day_of_the_week == 1 | day_of_the_week == 7) & start_locs > 5000) %>% arrange(-start_locs)

common_end_locs = no_missings %>% group_by(day_of_the_week, end_station_name) %>% summarize(end_locs = n())
filtered_ends = common_end_locs %>% filter((day_of_the_week == 1 | day_of_the_week == 7) & end_locs > 5000) %>% arrange(-end_locs)

#Streeter Dr & Grand Ave, Michigan Ave & Oak St, Millennium Park, Wells St & Concord Ln, Theater on the Lake
#lats <- c(41.89228, 41.90096, 41.88103, 41.91213, 41.92628, 41.91569)
#longs <- c(-87.61204, -87.62378, -87.62408, -87.63466, -87.63083, -87.63460)

#library(ggmap)
#chi <- get_map(location = "Chicago", zoom = 11)
