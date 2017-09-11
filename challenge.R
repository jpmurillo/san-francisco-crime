# rm(list = ls())
#library(tidyr)
library(dplyr)
library(lubridate)
library(ggmap)
library(ggvis)

setwd("/Users/jpablom1/Downloads/Police_Department_Incident/")

df = read.csv("Police_Department_Incident.csv", stringsAsFactors = FALSE)

df <- df %>% 
  rename(Longitude = X,
         Latitude = Y)

df <- df %>%
  mutate(DateTime = paste(Date, Time, sep = " "))

df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

df$Hour <- hour(hm(df$Time)) 
# Total by category
df %>% 
  group_by(Category) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Most common crime by date
crime_date <- df %>% 
  group_by(Date, Category) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1)

# Most popular crime daily is Larceny/Theft 85%
table(crime_date$Category) / 5276 * 100

df %>% select(Date) %>% unique() %>% count()




# Most common crime by Day of the week
crime_day <- df %>% 
  group_by(DayOfWeek, Category) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1)

# Most popular crime daily is Larceny/Theft 100%
table(crime_day$Category) / 7 * 100



################
## Question 2 ##
###############

# Finds the districts with high crime
dist_cat <- df %>%
  group_by(PdDistrict) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  as.data.frame()

# southern crime by month-year
southern_monthly <- df %>%
  filter(PdDistrict == "SOUTHERN") %>% 
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(crime = n()) %>%
  as.data.frame() %>%
  mutate(date = as.Date(paste(year, "/", month, "/", "15", sep=""))) %>%
  select(date, crime) %>%
  filter(date < "2017-06-01")

ggvis(southern_monthly, x = ~date, y = ~crime) %>%
  layer_lines() %>%
  scale_numeric("y", domain = c(0, 3000), nice = FALSE, clamp = TRUE)

# southern crime by month
southern_month <- df %>%
  filter(PdDistrict == "SOUTHERN" & Date < "2017-06-01") %>% 
  group_by(month=month(Date)) %>%
  summarise(crime = n()) %>%
  as.data.frame()

ggvis(southern_month, x = ~month, y = ~crime) %>%
  layer_lines() %>%
  scale_numeric("y", domain = c(0, 40000), nice = FALSE, clamp = TRUE)

# southern crime by week
southern_week <- df %>%
  filter(PdDistrict == "SOUTHERN" & Date < "2017-06-01") %>% 
  group_by(week=week(Date)) %>%
  summarise(crime = n()) %>%
  as.data.frame() %>%
  filter(week<53)

ggvis(southern_week, x = ~week, y = ~crime) %>%
  layer_lines() %>%
  scale_numeric("y", domain = c(0, 40000), nice = FALSE, clamp = TRUE)

