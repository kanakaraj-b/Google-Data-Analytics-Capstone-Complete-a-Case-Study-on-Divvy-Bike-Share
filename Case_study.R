#Loading all the required libraries

library(tidyverse)
library(tibble)
library(dplyr)
library(lubridate)
library(hms)

#Reading all the CSV files for analysis

year2020_04 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202004-divvy-tripdata.csv")
year2020_05 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202005-divvy-tripdata.csv")
year2020_06 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202006-divvy-tripdata.csv")
year2020_07 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202007-divvy-tripdata.csv")
year2020_08 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202008-divvy-tripdata.csv")
year2020_09 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202009-divvy-tripdata.csv")
year2020_10 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202010-divvy-tripdata.csv")
year2020_11 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202011-divvy-tripdata.csv")
year2020_12 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202012-divvy-tripdata.csv")
year2021_01 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202101-divvy-tripdata.csv")
year2021_02 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202102-divvy-tripdata.csv")
year2021_03 <- read_csv("C:/Users/WIN7i/Downloads/Bike-Share Case_Study/Trip_data/csv_file/2020/202103-divvy-tripdata.csv")

#Check for the columns in the files

colnames(year2020_04)
colnames(year2020_05)
colnames(year2020_06)
colnames(year2020_07)
colnames(year2020_08)
colnames(year2020_09)
colnames(year2020_10)
colnames(year2020_11)
colnames(year2020_12)
colnames(year2021_01)
colnames(year2021_02)
colnames(year2021_03)

#Checking the structure of each file

str(year2020_04)
str(year2020_05)
str(year2020_06)
str(year2020_07)
str(year2020_08)
str(year2020_09)
str(year2020_10)
str(year2020_11)
str(year2020_12)
str(year2021_01)
str(year2021_02)
str(year2021_03)

#converting the format of date in the file 2020-05 from string to date.

year2020_05[['started_at']]<-as.POSIXct(year2020_05[['started_at']],format("%d-%m-%Y %H:%M:%S"))
year2020_05[['ended_at']]<-as.POSIXct(year2020_05[['started_at']],format("%d-%m-%Y %H:%M:%S"))
str(year2020_05)

#Converting the datatype of column from string to integer

year2020_12[['start_station_id']]<-strtoi(year2020_12[['start_station_id']])
year2021_01[['start_station_id']]<-strtoi(year2021_01[['start_station_id']])
year2021_02[['start_station_id']]<-strtoi(year2021_02[['start_station_id']])
year2021_03[['start_station_id']]<-strtoi(year2021_03[['start_station_id']])
year2020_12[['end_station_id']]<-strtoi(year2020_12[['end_station_id']])
year2021_01[['end_station_id']]<-strtoi(year2021_01[['end_station_id']])
year2021_02[['end_station_id']]<-strtoi(year2021_02[['end_station_id']])
year2021_03[['end_station_id']]<-strtoi(year2021_03[['end_station_id']])

#Merge all the files together to single file.

complete_trip_data=bind_rows(year2020_04,year2020_05,year2020_06,year2020_07,year2020_08,
                             year2020_09,year2020_10,year2020_11,year2020_12,year2021_01,
                             year2021_02,year2021_03)

#Check the structure of the file

str(complete_trip_data)

##Cleaning and processing of data

#Remove the unwanted columns from the file

complete_trip_data[,c("start_lat","end_lat","start_lng","end_lng")]<-list(NULL)

str(complete_trip_data)

head(complete_trip_data)

#Add a coloum to check the ride time

complete_trip_data["Time_Taken"]=complete_trip_data["ended_at"]-complete_trip_data["started_at"]

head(complete_trip_data["Time_Taken"])

complete_trip_data<-complete_trip_data %>%
  mutate(start_time=as_hms(format(started_at,format="%H:%M:%S")))

head(complete_trip_data["start_time"])

complete_trip_data<-complete_trip_data %>%
  mutate(end_time=as_hms(format(ended_at,format="%H:%M:%S")))

head(complete_trip_data["end_time"])

complete_trip_data["Time_Taken_check"]=complete_trip_data["end_time"]-complete_trip_data["start_time"]

tail(complete_trip_data["Time_Taken_check"])

complete_trip_data["Time_Taken"]%>%
  mutate(taken_hours=hour(seconds_to_period(Time_Taken)),
         taken_min=minute(seconds_to_period(Time_Taken)))


head(complete_trip_data["Time_Taken"])

colnames(complete_trip_data)

#Add a column for days in week

complete_trip_data <- add_column(complete_trip_data, days_in_week = 
                                   weekdays(complete_trip_data$started_at))



complete_trip_data$days_in_week<- ordered(complete_trip_data$days_in_week,
                                          levels=c("Sunday", "Monday", "Tuesday",
                                                   "Wednesday", "Thursday", "Friday", "Saturday"))

head(complete_trip_data)

#Remove the null values

any(is.na(complete_trip_data))

complete_trip_data <- drop_na(complete_trip_data)

any(is.na(complete_trip_data))


complete_trip_data<-complete_trip_data%>%
  select(-c("Time_Taken"))

colnames(complete_trip_data)

complete_trip_data<- rename(complete_trip_data,Time_Taken=Time_Taken_check)

colnames(complete_trip_data)

table(complete_trip_data$member_casual)


complete_trip_data$Full_date<-as.Date(complete_trip_data$started_at)
head(complete_trip_data["Full_date"])

#Pull out month and year from the complete date

complete_trip_data$month<-format(as.Date(complete_trip_data$Full_date),"%m")
head(complete_trip_data["month"])

complete_trip_data$year<-format(as.Date(complete_trip_data$Full_date),"%y")
head(complete_trip_data["year"])


#Descriptive Analysis


str(complete_trip_data)


complete_trip_data$month<-as.numeric(as.character(complete_trip_data$month))
is.numeric(complete_trip_data$month)
is.numeric(complete_trip_data$year)
complete_trip_data$year<-as.numeric(as.character(complete_trip_data$year))
is.numeric(complete_trip_data$year)

complete_trip_data$Time_Taken<-as.numeric(as.character(complete_trip_data$Time_Taken))
is.numeric(complete_trip_data$Time_Taken)

mean(complete_trip_data$Time_Taken)
median(complete_trip_data$Time_Taken)
max(complete_trip_data$Time_Taken)


summary(complete_trip_data$Time_Taken)

#Overall summary
aggregate(complete_trip_data$Time_Taken ~ complete_trip_data$member_casual , FUN=mean)
aggregate(complete_trip_data$Time_Taken ~ complete_trip_data$member_casual, FUN=median)
aggregate(complete_trip_data$Time_Taken ~ complete_trip_data$member_casual, FUN=max)


#Replaced 3 letter abbreviation in place of number of month

complete_trip_data<-complete_trip_data%>%
  mutate(month=month.abb[as.numeric(month)])
complete_trip_data$month<-ordered(complete_trip_data$month,
                                  levels=c("Apr","May","Jun","Jul","Aug","Sep","Oct",
                                           "Nov","Dec","Jan","Feb","Mar"))

#Monthly summary
aggregate(complete_trip_data$Time_Taken ~ complete_trip_data$member_casual+
            complete_trip_data$month, FUN=mean)
aggregate(complete_trip_data$Time_Taken ~ complete_trip_data$member_casual+
            complete_trip_data$month, FUN=median)
aggregate(complete_trip_data$Time_Taken ~ complete_trip_data$member_casual+
            complete_trip_data$month, FUN=max)

#Weekly summary

aggregate(complete_trip_data$Time_Taken ~complete_trip_data$member_casual+
          complete_trip_data$days_in_week, FUN=max)
aggregate(complete_trip_data$Time_Taken ~complete_trip_data$member_casual+
            complete_trip_data$days_in_week,FUN=mean)



#Visualization of ride count by week and month

ggplot(data=complete_trip_data)+stat_count(mapping = aes(x=member_casual,
                                                        fill=member_casual))+
         ggtitle("Rider Count")+ ylab("Number of Riders")+xlab("Rider_type")+
         labs(fill="legend")+
  scale_fill_manual(values = c("casual"="green",
                               "member"="blue"))

#Number of riders per day in a week

complete_trip_data%>%
  group_by(member_casual,days_in_week)%>%
  summarise(ride_count=n())%>%
  arrange(member_casual,days_in_week,skip_absent=TRUE)%>%
  ggplot(aes(x=days_in_week,y=ride_count,fill=member_casual))+
  scale_fill_manual(values = c("casual"="green",
                               "member"="blue"))+
  geom_col(position="dodge")+
  labs(title="Ride_Count by days_in_week")


#Number of rider in a month per year

complete_trip_data%>%
  group_by(member_casual,month)%>%
  summarise(ride_count=n())%>%
  arrange(member_casual,month,skip_absent=TRUE)%>%
  ggplot(aes(x=month,y=ride_count,fill=member_casual))+
  scale_fill_manual(values = c("casual" = "green",
                               "member" = "blue")) +
  geom_col(position="dodge")+
  labs(title="Ride_Count by month")


