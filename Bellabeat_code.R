#Import library
library(janitor)
library(lubridate)
library(tidyverse)
library(skimr)
library(readr)
library(dplyr)

#Import data
d_activity <- read.csv("C:/Users/User/Documents/Bellabeat-Projects/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
h_calories <- read.csv("C:/Users/User/Documents/Bellabeat-Projects/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
h_intensities <- read.csv("C:/Users/User/Documents/Bellabeat-Projects/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
sleep <- read.csv("C:/Users/User/Documents/Bellabeat-Projects/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("C:/Users/User/Documents/Bellabeat-Projects/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

#Check if there is null in dataset

skim_without_charts(d_activity)
skim_without_charts(h_calories)
skim_without_charts(h_intensities)
skim_without_charts(sleep)
skim_without_charts(weight)

## In column Fat of table Weight there is 65 Null. It is similiar with original data ==) datasets were imported properly

#Exploratory Data Analysis

n_distinct(d_activity$Id)
n_distinct(h_calories$Id)
n_distinct(h_intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)


# activity
d_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# explore num of active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()

# Fix formatting
# h_intensities
h_intensities$ActivityHour=as.POSIXct(h_intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
h_intensities$time <- format(h_intensities$ActivityHour, format = "%H:%M:%S")
h_intensities$date <- format(h_intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
d_activity$ActivityDate=as.POSIXct(d_activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
d_activity$date <- format(d_activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")


# Merged data
merged_data <- merge(sleep, d_activity, by=c('Id', 'date'))
head(merged_data)
# Visualization
ggplot(data=d_activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Time in Bed")

int_new <- h_intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")