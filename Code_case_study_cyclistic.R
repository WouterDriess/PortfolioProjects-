setwd("~/Dropbox/Data_Analytics/Case_study1")
library(janitor)
library(tidyverse)
library(lubridate)
library(scales)
library(gridExtra)
#   Loading the data files from April 2020 to march 2021 and combining into a data frame.
df1 <- read.csv("202004-divvy-tripdata.csv")
df2 <- read.csv("202005-divvy-tripdata.csv")
df3 <- read.csv("202006-divvy-tripdata.csv")
df4 <- read.csv("202007-divvy-tripdata.csv")
df5 <- read.csv("202008-divvy-tripdata.csv")
df6 <- read.csv("202009-divvy-tripdata.csv")
df7 <- read.csv("202010-divvy-tripdata.csv")
df8 <- read.csv("202011-divvy-tripdata.csv")
df9 <- read.csv("202012-divvy-tripdata.csv")
df10 <- read.csv("202101-divvy-tripdata.csv")
df11 <- read.csv("202102-divvy-tripdata.csv")
df12 <- read.csv("202103-divvy-tripdata.csv")
bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df9,df10,df11,df12)

#   The data was cleaned by setting appropriate data types and removing rows with negative duration times for bike rides. 
#   Cleaning potential empty rows or colomns 
bike_data <- remove_empty(bike_rides, which = c("rows","cols"))

#   Converting character data to date format
bike_rides$started_at <- ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- ymd_hms(bike_rides$ended_at)


#   Omitting negative duration
bike_rides$started_at <- ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- ymd_hms(bike_rides$ended_at)
bike_rides$duration <- difftime(bike_rides$ended_at, bike_rides$started_at, units = "min")
bike_rides <- bike_rides %>% filter(duration >0)

#   Define colors
c1 <- "#4C72B0" # dark blue
c2 <- "#DD8452" # orange
#   Line graph showing average duration over 12 months.
bike_rides %>% 
  group_by(month=format(started_at,"%Y-%m"), member_casual) %>% 
  summarise(avgduration= mean(duration)) %>% 
  ggplot(aes(x=month, y=avgduration, color=member_casual, group = member_casual)) +
  geom_line(aes(color = member_casual),size = 1.5)+
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_manual(values=c('casual'=c1, 'member'=c2),
                     labels=c('Casual users','Membership users')) +
  labs(title="Duration of casual and member bike rides over a year",
       x="Months",
       y="Duration of use in minutes",
       color="User type") 

#   Line graph indicating average duration of casual bike rides by start time of day.
plot1 <- bike_rides %>% 
  group_by(hour=format(started_at,"%H")) %>% 
  filter(member_casual=="casual") %>% 
  summarise(avgduration= mean(duration)) %>% 
  ggplot(aes(x=hour, y=avgduration,group=1)) +
  geom_line(col=c1,size = 1.5) +
  scale_x_discrete(labels = c("12AM", "1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM", "9AM", "10AM", "11AM", "12PM", "1PM", "2PM", "3PM", "4PM", "5PM", "6PM", "7PM", "8PM", "9PM", "10PM", "11PM")) +
  theme(axis.text.x = element_text(angle = 45, size = 10))  +
  labs(title="Avarage duration of casual bike rides by start time of day.",
       x="Time of the day",
       y="Avarage duration of the bike ride in minutes",
       color='User type') 

#   Line graph indicating average duration of member bike rides by start time of day.
plot2 <- bike_rides %>% 
  group_by(hour=format(started_at,"%H")) %>% 
  filter(member_casual=="member") %>% 
  summarise(avgduration= mean(duration)) %>% 
  ggplot(aes(x=hour, y=avgduration,group=1)) +
  geom_line(col=c2,size = 1.5) +
  scale_x_discrete(labels = c("12AM", "1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM", "9AM", "10AM", "11AM", "12PM", "1PM", "2PM", "3PM", "4PM", "5PM", "6PM", "7PM", "8PM", "9PM", "10PM", "11PM")) +
  theme(axis.text.x = element_text(angle = 45, size = 10))  +
  labs(title="Avarage duration of member bike rides by start time of day.",
       x="Time of the day",
       y="Avarage duration of the bike ride in minutes",
       color='User type') 

grid.arrange( plot1, plot2, ncol=1)

# Bar graph indicating the top 30 most used start stations in Chicago.
bike_rides %>% 
  group_by(start_station_name) %>% 
  summarise(start_member=sum(member_casual=="member"), start_casual=sum(member_casual=="casual"), total=start_member+start_casual) %>% 
  arrange(desc(total)) %>% 
  slice(2:33) %>% #to omit the first two that have no name 
  pivot_longer(cols=c(start_member, start_casual), names_to="member_type", values_to="count") %>% 
  ggplot(aes(x=reorder(start_station_name, total), y=count, fill=member_type,pct)) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values = c(c2, c1),
                    name = "User type",
                    labels = c("Membership users", "Casual users"))+
  coord_flip()+
  labs(title="Top 30 most used start stations in Chicago", 
       x="Starting station",
       y="Started bike rides",
       color="User type")

# Bar graph indicating the bike ride counts by bike type and user type of bikes 
bike_rides %>% 
  group_by(rideable_type) %>% 
  summarise(type_member=sum(member_casual=="member"), 
            type_casual=sum(member_casual=="casual"), 
            total=type_member+type_casual) %>% 
  pivot_longer(cols=c(type_member, type_casual), 
               names_to="member_type", 
               values_to="count") %>% 
  ggplot(aes(reorder(x=rideable_type,-total),y=count,fill=member_type))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c(c2, c1),
                    name = "User type",
                    labels = c("Membership users", "Casual users"))+
  theme(axis.text.x = element_text (size = 10))+
  scale_y_continuous(labels=comma)+
  labs(title='Bike Ride Counts by Bike Type and User Type', 
       x="Bike Type",
       y="Number of Rides",
       color="User Type")

#   Interpretation of the data can be found in a Rmarkdowndocument on my Githubpage. 







