# Load necessary libraries
library(tidyverse)
library(lubridate)

# Step 1: Load the dataset
df_2020 <- read.csv("C:/Users/yaswa/Documents/New folder/RESUME/Divvy_Trips_2020_Q1.csv", stringsAsFactors = FALSE)

# Step 2: Print column names to verify
print(colnames(df_2020))  # Find the correct date column

# Step 3: Convert timestamps to datetime format
df_2020$started_at <- as.POSIXct(df_2020$started_at, format="%Y-%m-%d %H:%M:%S")
df_2020$ended_at <- as.POSIXct(df_2020$ended_at, format="%Y-%m-%d %H:%M:%S")

# Step 4: Calculate ride duration in minutes
df_2020$ride_duration <- as.numeric(difftime(df_2020$ended_at, df_2020$started_at, units="mins"))

# Step 5: Extract day of the week and hour of the day
df_2020$day_of_week <- weekdays(df_2020$started_at)
df_2020$hour_of_day <- hour(df_2020$started_at)

# Step 6: Remove negative or zero-duration rides
df_2020 <- df_2020 %>% filter(ride_duration > 0)

# Step 7: Summary statistics
summary(df_2020)

# Step 8: Plot Ride Duration by User Type
ggplot(df_2020, aes(x=member_casual, y=ride_duration, fill=member_casual)) +
  geom_bar(stat="summary", fun="mean") +
  labs(title="Average Ride Duration by User Type", x="User Type", y="Ride Duration (minutes)") +
  theme_minimal()

# Step 9: Ride Count by Day of the Week
ride_counts <- df_2020 %>% group_by(day_of_week, member_casual) %>% summarise(count = n())

ggplot(ride_counts, aes(x=day_of_week, y=count, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Ride Count by Day of the Week", x="Day of the Week", y="Ride Count") +
  theme_minimal()

# Step 10: Ride Count by Hour of the Day
hourly_counts <- df_2020 %>% group_by(hour_of_day, member_casual) %>% summarise(count = n())

ggplot(hourly_counts, aes(x=hour_of_day, y=count, color=member_casual)) +
  geom_line(size=1) +
  labs(title="Ride Count by Hour of the Day", x="Hour of Day", y="Ride Count") +
  theme_minimal()![Rplot10](https://github.com/user-attachments/assets/dde15cbf-1ece-4fb2-b52d-71348a7c27e9)
