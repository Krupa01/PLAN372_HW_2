# I will Load libraries
library(tidyverse)
library(lubridate)
#Load the CSV file 

#The CSV file wil be reffered to as Fire_incidents 
Fire_incidents = read_csv("Fire_Incidents.csv")


# -------------------- QUESTION ONE -----------------------
#How long does it take Wake County Fire to respond to incidents, on average (i.e. the 
#time between when an incident is dispatched and when firefighters arrive on the 
#scene)? (hint: you can subtract lubridate date columns from each other).

## I will use the arrive_date_time and dispatch_date_time to find the mean of when fire fighters arrived. 
#I first wanted to start with converting the arrive_date_time and dispatch_date_time to a date time information using ymd_hms. 
#I used lubridate library to parse the date time, and then I subtracted arrive_date_time and dispatch_date_time, to find the amount of time it takes for firefighters to respond. 
#Finally, the mean will give us the average time it took firefighters to respond to the fire calls from time og dispatch.

Fire_incidents$arrive_date_time = ymd_hms(Fire_incidents$arrive_date_time)
Fire_incidents$dispatch_date_time = ymd_hms(Fire_incidents$dispatch_date_time)
Fire_incidents$response_time = Fire_incidents$arrive_date_time - Fire_incidents$dispatch_date_time
mean(Fire_incidents$response_time, na.rm = T)

# Answer: It takes an average of 318.7497 secs between dispatch and arrival. 

# -------------------- QUESTION TWO -----------------------
#Does this response time vary by station? What stations have the highest and lowest average response times?

#I wanted to start of with grouping the Fire incidents by the station column. Then I can get the mean/ average response time in each station. 

station_response_time = group_by(Fire_incidents, station) %>%
  summarize(
    response_time=mean(response_time, na.rm = T)
  )

station_response_time 

#station_response_time gives me the average of all the stations in a table, but I wanted to create tables where it would give me just one answer.
#I had to search up how to find max and min. I know how to find it just by looking at the table but was not sure how to write the code to have it give me just the table for the fastest and slowest response time, but this could worked. 
#This code looks at the fastest (max) and slowest (min) response times and then tells me which station it is in a table. 

highest_station = station_response_time[which.max(station_response_time$response_time), "station"]
lowest_station = station_response_time[which.min(station_response_time$response_time), "station"]

# Answer: Yes, the response times vary by station looking at the station_response_time table. The station with the highest response time is station 29 and the lowest average response time is station 13.

# -------------------- QUESTION THREE -----------------------
#Have Wake County Fire’s response times been going up or down over time? What might be the reason for these changes?

#Because I want to create a graph with the year and average response time I first needed to add a year column on the Fire_incidents table. 
#I used the dispatch_date_time column to assign the year value. From there I grouped the Fire_incident by year and 
# using the floor_date function I can round to the nearest year. Finally I can then use the mean to get the average of the response times by year. 
# Creating a line graph would allow me to see the if the  response time has increased or decreased over time.

Fire_incidents$year = year(Fire_incidents$dispatch_date_time)

Response_overtime = group_by(Fire_incidents, year = floor_date(dispatch_date_time, unit = "year")) %>%
  summarize(response_overtime_avg=mean(response_time,na.rm = T))

ggplot(Response_overtime, aes(x=year, y=response_overtime_avg)) +
  geom_line() 

# Answer: Response rates have varied over the years, rising and falling, however in 2019 there was a large increase and a drastic decrease the years following.
# There could be a variety of reasons why calls have been increasing/decreasing. It could be that fire departments got more funding, more volunteers/employees, addition or closure of fire stations, new training, advanced technologies,and etc. 

# -------------------- QUESTION FOUR -----------------------

#At what times of day are fire calls most likely to occur?

# I started off with using ymd_hms so I can convert the dispatch_date_time into a time format, I used hour. 
#I then created a column called hour in the table that extracted the time from the dispatch_date_time column.
#Using group by, I grouped the Fire_incidents data and the hour column and wanted to count the number of calls made each hour of the day using summarize, I named this new column Amount_of_call. 
#Finally by creating a histogram I was able to see the times of days the calls occur and find when most occur. 

Fire_incidents$dispatch_date_time = ymd_hms(Fire_incidents$dispatch_date_time)

Fire_incidents$hour= hour(Fire_incidents$dispatch_date_time)

Time_fire = group_by(Fire_incidents, hour) %>% 
  summarize(Amount_of_calls = n()) 

ggplot(Time_fire, aes(x=hour, y=Amount_of_calls)) + 
  geom_histogram(stat="identity")

# Answer: Most calls happen during the night at 10 pm being the highest. Most times occur between 9-11pm.

# -------------------- QUESTION FIVE -----------------------

#How many calls to Wake County Fire are recorded in this dataset, and how many of them are actual fires?

#I started off by looking at the  National Fire Incident Reporting System Complete Reference Guide and writing down the codes for all the actual fire incidents and called it actual_fires. 
#I then counted the total number of incidents and stored it under the variable Amount_of_calls. This will give me How many calls to Wake County Fire are recorded. 
#Next, I filtered the Fire incidents table to only include all the incidents with actual fire. 
# I found the %in% function online so that I could select rows where the incident_type is one of the codes in the actual_fires. 
#I then grouped the filtered the table by incident_type and summarized it show the number of calls and the number of fire calls for each incident type.
#Finally using sum I summed the number of  calls to get the total number of actual fire calls

#This question also created a table called Amount_of_calls, that contains all fire incidents that were actually fires. 

actual_fires = c(111, 112, 113, 114, 115, 116, 117, 118, 121, 122, 123, 120, 131, 132, 133, 134, 135, 136, 137, 138, 130, 141, 142, 143, 140, 151,152,153,154,155,150, 161, 162, 163, 164, 160, 171, 172, 173, 170, 100)
Amount_of_calls = Fire_incidents %>% count()

Actual_Fire_call = filter(Fire_incidents, incident_type %in% actual_fires)

fire_calls = Actual_Fire_call %>%
  group_by(incident_type) %>%
  summarize(Amount_of_calls = n(),
            fire_calls = sum(incident_type %in% actual_fires)) 
fire_calls= sum(fire_calls$fire_calls)

Amount_of_calls
fire_calls

# Answer 
# The total amount of calls are 229047. 
# The total amount of calls that were actual fire calls were 17230. 

# -------------------- QUESTION SIX -----------------------

# Is this response time faster than the average response time for all incidents?

#Again  I stared similarly to the question about with the actual fire code
# I then created the Actual_Fire_call table in which I filtered the Fire_incidents table to only keep the incident type row that has a code included in the actual fire list. I was able to do this with the %in% function 
#Then in order to get the average response time and the average response time for the actual fire calls, I used mean to extract the response time in the Actual_Fire_call and Fire_incidents table. 

actual_fires = c(111, 112, 113, 114, 115, 116, 117, 118, 121, 122, 123, 120, 131, 132, 133, 134, 135, 136, 137, 138, 130, 141, 142, 143, 140, 151,152,153,154,155,150, 161, 162, 163, 164, 160, 171, 172, 173, 170, 100)
Actual_Fire_call = filter(Fire_incidents, incident_type %in% actual_fires)

Avg_fire_response_time = mean(Actual_Fire_call$response_time, na.rm = T)
Avg_response_time = mean(Fire_incidents$response_time, na.rm = T)

Avg_fire_response_time  
Avg_response_time
Avg_response_time - Avg_fire_response_time

#The average response time for actual fire calls is 310.9914 secs 
#The average response time for all incidents is 318.7497 secs
#So, there is a 7.758282 secs faster response time

# -------------------- QUESTION SEVEN -----------------------

#Repeat the analysis for questions 2-4 for actual fires, rather than all incidents. 
#For this following codes, I just copied what I had done earlier just changing the tables I used. 
#Instead of using the Fire_incidents table I used the Actual_Fire_call table, which I created in question 5. 

#Q2 
station_response_time = group_by(Actual_Fire_call, station) %>%
  summarize(
    response_time=mean(response_time, na.rm = T)
  )

station_response_time 

highest_station = station_response_time[which.max(station_response_time$response_time), "station"]
lowest_station = station_response_time[which.min(station_response_time$response_time), "station"]

highest_station
lowest_station

#The station with the highest response time is 23
#The station with the lowest response time is 3

#Q3
Actual_Fire_call$year = year(Actual_Fire_call$dispatch_date_time)

Response_overtime = group_by(Actual_Fire_call, year = floor_date(dispatch_date_time, unit = "year")) %>%
  summarize(response_overtime_avg=mean(response_time,na.rm = T))

ggplot(Response_overtime, aes(x=year, y=response_overtime_avg)) +
  geom_line() 

#The graph has varied throughout the years following an increase decrease pattern. I noticed that the climax(point where it decreases) has been getting lower every year. 
# I would think that the reason for this decrease is that fire calls are a priority so perhaps stations got more funding, more volunteers/workers, new vehicles, more stations, even new training tactics, or just safer technology in homes. 

#Q4

Actual_Fire_call$dispatch_date_time = ymd_hms(Actual_Fire_call$dispatch_date_time)

Actual_Fire_call$hour= hour(Actual_Fire_call$dispatch_date_time)

Time_fire = group_by(Actual_Fire_call, hour) %>% 
  summarize(Amount_of_calls = n()) 

ggplot(Time_fire, aes(x=hour, y=Amount_of_calls)) + 
  geom_histogram(stat="identity")

#Most of the calls occur at 9 PM. most calls come at night between 8-10

