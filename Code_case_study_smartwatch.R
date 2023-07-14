setwd("~/Dropbox/Data_Analytics/Case_study2/Rawdata")
library(janitor)
library(tidyverse)
library(lubridate)
library(scales)
library(gridExtra)
library(FSA)
library(ggpubr)
library(dunn.test)
library(rstatix)

heartrate_seconds <- read.csv("heartrate_seconds_merged.csv")
hourly_calories <- read.csv("hourlyCalories_merged.csv")
daily_activity <- read.csv("dailyActivity_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")
daily_intensities <- read.csv("dailyIntensities_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")
hourly_intensities <- read_csv("hourlyIntensities_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
minute_calories_narrow<- read.csv("minuteCaloriesNarrow_merged.csv")
minute_intensities_narrow <- read_csv("minuteIntensitiesNarrow_merged.csv")
minute_met_narrow <- read_csv("minuteMETsNarrow_merged.csv")
minute_sleep <- read.csv("minuteSleep_merged.csv")
minute_steps_narrow <- read_csv("minuteStepsNarrow_merged.csv")
minute_steps_wide<- read.csv("minuteStepsWide_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight_log <- read_csv("weightLogInfo_merged.csv")

#   Data aggregation in 3 categories based on timescale. It creates three distinct data frames: 'daily', 'hourly', 
#   and 'minute'.The data aggragetes give a more comprehensive insight into individuals' fitness and activity patterns 
#   through inner joins on common columns like 'Id', 'ActivityDate', 'ActivityHour', and 'ActivityMinute'. This format makes 
#   the data more accessible for further analysis. 
daily <- daily_activity %>% 
  inner_join(daily_calories,by=c("Id","Calories","ActivityDate" = "ActivityDay")) %>% 
  inner_join(daily_intensities,by=c("Id","VeryActiveDistance","ModeratelyActiveDistance",
                                    "LightActiveDistance","SedentaryActiveDistance",
                                    "VeryActiveMinutes","FairlyActiveMinutes",
                                    "LightlyActiveMinutes","SedentaryMinutes",
                                    "ActivityDate" = "ActivityDay")) %>% 
  inner_join(daily_steps,by=c("Id", "ActivityDate" = "ActivityDay")) 


hourly <- hourly_calories %>% inner_join(hourly_intensities,by=c("Id","ActivityHour")) %>% 
  inner_join(hourly_steps,by=c("Id","ActivityHour"))

minute <- minute_calories_narrow %>% inner_join(minute_intensities_narrow,by=c("Id","ActivityMinute")) %>% 
  inner_join(minute_met_narrow,by=c("Id","ActivityMinute")) %>%
  inner_join(minute_steps_narrow,by=c("Id","ActivityMinute"))

#   After the data was joined together in 3 distinct data frames, any unnecessary data was removed from the environment 
#   to tidy up the work space. 
rm(daily_activity,daily_calories,daily_intensities,daily_steps,minute_calories_narrow,minute_intensities_narrow,minute_met_narrow,
   minute_steps_narrow,hourly_calories,hourly_intensities,hourly_steps)

#   The data frames were inspected for duplicate entries and removed upon identification.
count(daily)
count(distinct(daily))
count(hourly)
count(distinct(hourly))
count(minute)
count(distinct(minute))
count(weight_log)
count(distinct(weight_log))
count(sleep_day)
count(distinct(sleep_day)) #found  duplicate rows 
sleep_day <- distinct(sleep_day) #romove duplicates

#   The variable names were reformatted to improve the consistency and readability of the data.
daily <- clean_names(daily)
hourly <- clean_names(hourly)
minute <- clean_names(minute)
sleep_day <- clean_names(sleep_day)
weight_log <- clean_names(weight_log)

#   The overview of the data structure showed that the dates were stored as character types. 
#   To enhance their usability, the    dates were converted into the date data type.
daily$activity_date <- mdy(daily$activity_date)
format(daily$activity_date, format = "%Y-%m-%d")
hourly$activity_hour <- mdy_hms(hourly$activity_hour)
format(hourly$activity_hour,format="%Y-%m-%d %H:%M:%S")
sleep_day$sleep_day <- mdy_hms(sleep_day$sleep_day)
format(sleep_day$sleep_day,format="%Y-%m-%d %H:%M:%S")
minute$activity_minute <- mdy_hms(minute$activity_minute)
format(minute$activity_minute,format="%Y-%m-%d %H:%M:%S")

#   Three data frames were created, each containing the counts of days that individuals made use 
#   of different functionality types. These data frames were then joined together using a full join operation. 
#   The 'summarise' function was applied to calculate the average number of days per functionality type. 
#   Finally, the results were displayed in a bar graph.

#   The ifelse function counts the elements that are higher than 0 for each individual resulting in the days that every individual made use of the function 
df1 <- daily %>% 
  group_by(id) %>% 
  summarise(step = ifelse(sum(total_steps) > 0, n(), 0),                # Calculates the count of usage of the steps function
            distance_track = ifelse(sum(logged_activities_distance) > 0, n(), 0)) # Calculates the count of usage of the distance track                                                                          function

df2 <- sleep_day %>%
  group_by(id) %>% summarise(count_sleep=n())                           #Calculates the count of usage of the sleep function
df3 <- weight_log %>% group_by(id) %>% summarise(counts_weight=n())     #Calculates the count of usage of the weights                                                                                  function
#   Joining the groups
Usage_by_group <- df1 %>% 
  full_join(df2,by="id") %>% 
  full_join(df3,by="id") %>% 
  replace_na(list(step = 0, distance_track = 0, count_sleep = 0, counts_weight = 0)) # replacing the na for 0 values 


#   Color choice 
c1 <- "palegreen2"
c2 <- "deepskyblue2"
c3 <- "tomato1"
c4 <- "yellow2"

#   Bar graph of the avarage use per functionality in a 31-day period. 
Usage_by_group %>% 
  select(step, count_sleep, counts_weight, distance_track) %>% 
  summarise(avg_step = mean(step), # calculates avarge days of use in a 31 day period 
            avg_sleep = mean(count_sleep), 
            avg_weight = mean(counts_weight), 
            avg_distance_track = mean(distance_track)) %>% 
  gather(key = "variable", value = "count") %>%
  mutate(variable = factor(variable, levels = c("avg_step", "avg_sleep", "avg_distance_track", "avg_weight"))) %>% # creating factors of the variables in order to display the legend in the correct order
  ggplot(aes(x = reorder(variable, -count), y = count, fill = variable)) + 
  geom_col() +
  scale_fill_manual(values = c("avg_step" = c1 , "avg_sleep" = c2, "avg_distance_track" = c3, "avg_weight" = c4),
                    labels = c("Steps", "Sleep", "Distance Tracking", "Weight")) +
  labs(title = "Average days of use per functionality over 31-day period, n=30", 
       x = "Functionality type",
       y = "Records of use within 31 days") +
  guides(fill = guide_legend(title = "Functionality type")) + # adds a title to the legend
  theme(axis.text.x = element_blank()) # removes the variable names on the x-axis 

#   The distance tracking function tracks the intensity of use. To analyze any overtime patterns in 
#   the intensity of use, the average daily distance traveled for different intensity categories over 
#   time was visualized with a line graph. The graph includes blue rectangles to highlight weekends, 
#   enabling the identification of distinctive patterns or differences in average daily distance traveled 
#   between weekends and weekdays.
#   Group the data by activity date and calculate the average distances for different intensity categories
daily %>%
  group_by(day = activity_date) %>%
  summarise(
    avg_very_active = mean(very_active_distance),
    avg_moderately_active = mean(moderately_active_distance),
    avg_light_active = mean(light_active_distance),
    avg_sedentary_active = mean(sedentary_active_distance)
  ) %>%
  ggplot(aes(x = day)) +
#   Plotting lines for each intensity category
  geom_line(aes(y = avg_very_active, color = "High Intensity")) +
  geom_line(aes(y = avg_moderately_active, color = "Moderate Intensity")) +
  geom_line(aes(y = avg_light_active, color = "Low Intensity")) +
#   Highlighting weekends with rectangles
  geom_rect(
    aes(xmin = as.Date("2016-04-16"), xmax = as.Date("2016-04-17"), ymin = -Inf, ymax = Inf),
    fill = "lightblue",
    alpha = 0.03
  ) +
  geom_rect(
    aes(xmin = as.Date("2016-04-23"), xmax = as.Date("2016-04-24"), ymin = -Inf, ymax = Inf),
    fill = "lightblue",
    alpha = 0.03
  ) +
  geom_rect(
    aes(xmin = as.Date("2016-04-30"), xmax = as.Date("2016-05-01"), ymin = -Inf, ymax = Inf),
    fill = "lightblue",
    alpha = 0.03
  ) +
  geom_rect(
    aes(xmin = as.Date("2016-05-07"), xmax = as.Date("2016-05-08"), ymin = -Inf, ymax = Inf),
    fill = "lightblue",
    alpha = 0.03
  ) +
  # Manually setting colors and labels for legend
  scale_color_manual(
    values = c("High Intensity" = "red", "Moderate Intensity" = "green", "Low Intensity" = "blue"),
    labels = c("High Intensity", "Moderate Intensity", "Low Intensity")
  ) +
  # Setting plot titles and axis labels
  labs(
    title = "Average Daily Distance Traveled by Intensity Categories, \nwith Weekends Highlighted in Blue",
    x = "Date",
    y = "Average Daily Distance Traveled (in Miles)",
    color = "User Category"
  )

#   To conduct a more detailed analysis of usage patterns, the data was categorized into three groups based on the distribution 
#   of individual step counts: lightly active, moderately active, and highly active. The mean number of steps per hour was then 
#   plotted against the time of day, resulting in a line graph that illustrates the average steps taken per hour for each activity 
#   category. This graph provides insights into the variations in step count patterns throughout the day for the three activity categories.

#   This code calculates the mean total steps for each 'id' in the 'hourly' dataset. It then arranges the mean step totals in ascending order 
#   and uses the quantile function to divide the data into three equal groups based on the 33rd and 66th percentiles.
hourly %>% 
  group_by(id) %>% 
  summarize(mean_step_total = mean(step_total)) %>% 
  arrange(mean_step_total) %>% 
  pull(mean_step_total) %>% 
  quantile(probs = c(0.33, 0.66))

hourly_activity <- hourly %>% 
  group_by(id) %>% 
  summarize(mean_step_total = mean(step_total)) %>% 
  mutate(step_category = case_when(
    mean_step_total < 245.8118 ~ "Lightly Active",
    mean_step_total >=245.8118 & mean_step_total < 365.7832 ~ "Moderately Active",
    mean_step_total > 365.7832 ~ "Highly Active"))


#   "This code calculates the mean total steps for each 'id' in the 'hourly' dataset. It then assigns a step category to each 'id' 
#   based on their mean step total using the 'case_when' function. The categories are 'Lightly Active' for mean step totals below 245.8118, 
#   'Moderately Active' for mean step totals between 245.8118 and 365.7832, and 'Highly Active' for mean step totals above 365.7832."
hourly %>%
  left_join(hourly_activity, by = "id") %>%
  mutate(hour = hour(activity_hour)) %>%
  group_by(step_category, hour) %>%
  summarise(mean_steps = mean(step_total)) %>% 
  mutate(step_category = factor(step_category, levels = c("Highly Active", "Moderately Active", "Lightly Active"))) %>%
  ggplot(aes(x = hour, y = mean_steps, color = step_category)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  labs(x = "Hour", y = "Mean Steps", color = "Step Category") +
  theme_minimal() +
  labs(
    title = "Average Steps Taken per Hour in a 2-Day Period",
    x = "Hour of the Day",
    y = "Mean Steps",
    color = "Step Category"
  )

#   To explore the potential relationship between sleep time and sedentary behavior, the data was categorized 
#   into three groups based on the distribution of sedentary time spent per individual: low sedentary, moderately 
#   sedentary, and high sedentary. Boxplots were constructed to compare the sleep time across these groups, providing 
#   visual representations of the differences in sleep duration among the sedentary categories.

#   Calculating quantiles for mean_sedentary_minutes
daily %>%
  group_by(id) %>%
  summarise(
    mean_sedentary_minutes = mean(sedentary_minutes)
  ) %>%
  inner_join(
    sleep_day %>%
      group_by(id) %>%
      summarise(mean_total_min_asleep = mean(total_minutes_asleep))
  ) %>% 
  arrange(mean_sedentary_minutes) %>% 
  pull(mean_sedentary_minutes) %>% 
  quantile(probs = c(0.33, 0.66))

#   Creating daily_sleep data frame
daily_sleep <- daily %>%
  group_by(id) %>%
  summarise(
    mean_sedentary_minutes = mean(sedentary_minutes)
  ) %>%
  inner_join(
    sleep_day %>%
      group_by(id) %>%
      summarise(mean_total_min_asleep = mean(total_minutes_asleep))
  ) %>%
  mutate(sedentary_time_category = case_when(
    mean_sedentary_minutes < 762 ~ "low sedentary",
    mean_sedentary_minutes >= 762 & mean_sedentary_minutes < 1066 ~ "moderately sedentary",
    mean_sedentary_minutes >= 1066 ~ "high sedentary"
  )) %>%
  select(sedentary_time_category, mean_total_min_asleep, mean_sedentary_minutes)

#   Change the order of factor levels
daily_sleep$sedentary_time_category <- factor(daily_sleep$sedentary_time_category, levels = c("low sedentary", "moderately sedentary", "high sedentary"))

#   Creating a box plot
ggplot(daily_sleep, aes(x = sedentary_time_category, y = mean_total_min_asleep, fill = sedentary_time_category)) +
  geom_boxplot() +
  xlab("Sedentary Time Category") +
  ylab("Sleep Time (minutes)") +
  ggtitle("Sleep Time by Activity Category") +
  scale_fill_manual(values = c(c1, c2, c3)) +
  guides(fill = guide_legend(title = "Sedentary Time Category"))

#   Performing Kruskal-Wallis test and post hoc Dunn's test
kruskal.test(mean_total_min_asleep ~ sedentary_time_category, data = daily_sleep)
dunnTest(daily_sleep$mean_total_min_asleep, daily_sleep$sedentary_time_category, method = "bonferroni")

#   The interpretation of the data can be found in a Rmarkdown document on my github page.




