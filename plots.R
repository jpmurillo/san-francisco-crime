# rm(list = ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(ggvis)


#####################
# Data Preparation  #
#####################

setwd("/Users/jpablom1/Downloads/Police_Department_Incident/")

df = read.csv("Police_Department_Incident.csv", stringsAsFactors = FALSE)

df <- df %>% 
  rename(Longitude = X,
         Latitude = Y)

df <- df %>%
  mutate(DateTime = paste(Date, Time, sep = " "))

df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

df$Hour <- hour(hm(df$Time)) 

df <- df %>%
  filter(Category != "NON-CRIMINAL")

df %>%
  filter(Category=="LARCENY/THEFT")%>%
  group_by(Descript)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%View()

# Most popular Crime Categories - across entire period
ggplot(df %>% filter(year(Date) >= 2010) %>%
         group_by(Category) %>%
         summarise(Count = n()) %>%
         arrange(desc(Count)) %>%
         mutate(Freq = Count / sum(Count)) %>%
         head(20), aes(x = reorder(Category, Freq), y = Freq)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Crime Categories",
       y = "Number of Crimes 2003 - 2017",
       title = "Crime Distribution by Category")

ggplot(df %>% filter(year(Date) >= 2010) %>%
         group_by(Descript) %>%
         summarise(Count = n()) %>%
         arrange(desc(Count)) %>%
         head(20), aes(x = reorder(Descript, Count), y = Count)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Crime Descriptions",
       y = "Number of Crimes 2003 - 2017",
       title = "Crime Distribution by Description (20)")

# District with most number of hits for entire period
ggplot(df %>% 
         group_by(PdDistrict) %>% filter(year(Date) >= 2010) %>%
         summarise(Count = n()) %>%
         filter(Count > 5)
       , aes(x = reorder(PdDistrict, Count), y = Count)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Districts",
       y = "Number of Crimes 2003 - 2017",
       title = "Total Count of Crimes Per District")


# Most popular Crime Categories - thorughout the day: LARCENY from 9am on
df %>% 
  group_by(Hour,Category) %>%
  summarise(Count = n()) %>%
  arrange(Hour, desc(Count)) %>%
  group_by(Hour) %>%
  top_n(2)%>% View()

df %>% 
  group_by(Hour,Descript) %>%
  summarise(Count = n()) %>%
  arrange(Hour, desc(Count)) %>%
  group_by(Hour) %>%
  top_n(2) %>% View()

# District with most number of hits throughout the day: SOUTHERN
df %>% 
  group_by(Year = year(Date), Hour, PdDistrict) %>%
  summarise(Count = n()) %>%
  arrange(Year, Hour, desc(Count)) %>%
  #group_by(Hour) %>%
  top_n(1) %>% View()


#####################################################################
# Top 20 Addresses by number of incidents
df %>%
  group_by(Address) %>%
  summarise(Count = n()) %>%
  left_join(df %>%
              select(Address, PdDistrict) %>%
              unique(), by = c("Address")) %>%
  select(Address, PdDistrict, Count) %>%
  arrange(desc(Count)) %>%
  head(10)


######################################
# Distributions of Districts with Most Crime Occurrences
######################

ggplot(df %>% 
         filter(PdDistrict == "SOUTHERN") %>%
         group_by(Category) %>%
         summarise(Count = n()) %>%
         filter(Count >  5000) %>%
         arrange(desc(Count)), aes(x = reorder(Category, Count), y = Count)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Crime Categories",
       y = "Number of Crimes 2003 - 2017",
       title = "Crime Categories in SOUTHERN")

ggplot(df %>% 
         filter(PdDistrict == "SOUTHERN") %>%
         group_by(Descript) %>%
         summarise(Count = n()) %>%
         filter(Count >  5000) %>%
         arrange(desc(Count)) %>%
         head(10), aes(x = reorder(Descript, Count), y = Count)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Crime Descriptions",
       y = "Number of Crimes 2003 - 2017",
       title = "Crime Descriptions in SOUTHERN")

ggplot(df %>% 
         filter(PdDistrict == "MISSION") %>%
         group_by(Category) %>%
         summarise(Count = n()) %>%
         filter(Count >  5000) %>%
         arrange(desc(Count)), aes(x = reorder(Category, Count), y = Count)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Crime Categories",
       y = "Number of Crimes 2003 - 2017",
       title = "Crime Categories in MISSION")

ggplot(df %>% 
         filter(PdDistrict == "MISSION") %>%
         group_by(Descript) %>%
         summarise(Count = n()) %>%
         filter(Count >  5000) %>%
         arrange(desc(Count)) %>%
         head(10), aes(x = reorder(Descript, Count), y = Count)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "Crime Descriptions",
       y = "Number of Crimes 2003 - 2017",
       title = "Crime Descriptions in MISSION")



############################################
### Seasonality
############################
# 
ggplot(df %>% 
         filter(Category %in% c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL", 
                                "ASSAULT", "VEHICLE THEFT", "DRUG/NARCOTIC")) %>%
         group_by(Category, Hour) %>%
         summarise(Count = n()), aes(Hour)) + 
  geom_line(aes(y = Count, colour = Category))+
  scale_x_discrete(limits=seq(0,23,2)) +
  labs(x = "Hour",
      y = "Hourly Count",
      title = "Hourly Trends for Top Crime Categories")

ggplot(df %>% 
         filter(Category %in% c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL", 
                                "ASSAULT", "VEHICLE THEFT", "DRUG/NARCOTIC")) %>%
         group_by(Category, weekday = wday(Date)) %>%
         summarise(Count = n()), aes(weekday)) + 
  geom_line(aes(y = Count, colour = Category)) +
  scale_x_discrete(name="Day of the Week", limits=c("Su","Mo","Tu","We","Th","Fr","Sa"))+
  labs(x = "Day",
       y = "Daily Count",
       title = "Daily Trends for Top Crime Categories")

ggplot(df %>% 
         filter(Category %in% c("LARCENY/THEFT", "OTHER OFFENSES", "NON-CRIMINAL", 
                                "ASSAULT", "VEHICLE THEFT", "DRUG/NARCOTIC") &
                  year(Date) <= 2016) %>%
         group_by(Category, month = month(Date)) %>%
         summarise(Count = n()), aes(month)) + 
  geom_line(aes(y = Count, colour = Category)) + 
  scale_x_discrete(name ="Month", limits=seq(1,12,1))+
  labs(x = "Month",
       y = "Montly Count",
       title = "Monthly Trends for Top Crime Categories")





ggplot(df %>%
         filter(Descript %in% c("GRAND THEFT FROM LOCKED AUTO", "DRIVERS LICENSE, SUSPENDED OR REVOKED", "PETTY THEFT OF PROPERTY", 
                                "STOLEN AUTOMOBILE", "STOLEN AUTOMOBILE", "BATTERY", "WARRANT ARREST")) %>%
         group_by(Descript, Hour) %>%
         summarise(Count = n()), aes(Hour)) + 
  geom_line(aes(y = Count, colour = Descript))+
  scale_x_discrete(limits=seq(0,23,2)) +
  labs(x = "Hour",
       y = "Hourly Count",
       title = "Hourly Trends for Top Incident Descriptions")

ggplot(df %>% 
         filter(Descript %in% c("GRAND THEFT FROM LOCKED AUTO", "DRIVERS LICENSE, SUSPENDED OR REVOKED", "PETTY THEFT OF PROPERTY", 
                                "STOLEN AUTOMOBILE", "STOLEN AUTOMOBILE", "BATTERY", "WARRANT ARREST")) %>%
         group_by(Descript, weekday = wday(Date)) %>%
         summarise(Count = n()), aes(weekday)) + 
  geom_line(aes(y = Count, colour = Descript)) +
  scale_x_discrete(name="Day of the Week", limits=c("Su","Mo","Tu","We","Th","Fr","Sa"))+
  labs(x = "Day",
       y = "Daily Count",
       title = "Daily Trends for Top Incident Descriptions")

ggplot(df %>% 
         filter(Descript %in% c("GRAND THEFT FROM LOCKED AUTO", "DRIVERS LICENSE, SUSPENDED OR REVOKED", "PETTY THEFT OF PROPERTY", 
                                "STOLEN AUTOMOBILE", "STOLEN AUTOMOBILE", "BATTERY", "WARRANT ARREST")) %>%
         group_by(Descript, month = month(Date)) %>%
         summarise(Count = n()), aes(month)) + 
  geom_line(aes(y = Count, colour = Descript)) + 
  scale_x_discrete(name ="Month", limits=seq(1,12,1))+
  labs(x = "Month",
       y = "Montly Count",
       title = "Monthly Trends for Top Incident Descriptions")








#############
## HEATMAP ##
#############

# Download the base map
sf <- get_map(location = "san francisco", zoom = 12, maptype = "roadmap", color = "bw")
# Draw the heat map
ggmap(sf, extent = "device") + 
  geom_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "GRAND THEFT FROM LOCKED AUTO"), 
                 aes(x = Longitude, y = Latitude), size = .15) + 
  stat_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "GRAND THEFT FROM LOCKED AUTO"), 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.02, 
                 bins = 100, geom = "polygon", contour = TRUE) + 
  scale_fill_gradient(low = "black", high = "red") + 
  scale_alpha(range = c(0.15, 0.2), guide = FALSE) +
  labs(title = "GRAND THEFT FROM LOCKED AUTO")



# Download the base map
sf <- get_map(location = "san francisco", zoom = 12, maptype = "roadmap", color = "bw")
# Draw the heat map
ggmap(sf, extent = "device") + 
  geom_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "PETTY THEFT OF PROPERTY"), 
                 aes(x = Longitude, y = Latitude), size = .15) + 
  stat_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "PETTY THEFT OF PROPERTY"), 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.02, 
                 bins = 100, geom = "polygon", contour = TRUE) + 
  scale_fill_gradient(low = "black", high = "red") + 
  scale_alpha(range = c(0.15, 0.2), guide = FALSE) +
  labs(title="PETTY THEFT OF PROPERTY")


# Download the base map
sf <- get_map(location = "san francisco", zoom = 12, maptype = "roadmap", color = "bw")
# Draw the heat map
ggmap(sf, extent = "device") + 
  geom_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "DRIVERS LICENSE, SUSPENDED OR REVOKED"), 
                 aes(x = Longitude, y = Latitude), size = .15) + 
  stat_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "DRIVERS LICENSE, SUSPENDED OR REVOKED"), 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.02, 
                 bins = 100, geom = "polygon", contour = TRUE) + 
  scale_fill_gradient(low = "black", high = "red") + 
  scale_alpha(range = c(0.1, 0.15), guide = FALSE)+
  labs(title="DRIVERS LICENSE, SUSPENDED OR REVOKED")



sf <- get_map(location = "san francisco", zoom = 12, maptype = "roadmap", color = "bw")
# Draw the heat map
ggmap(sf, extent = "device") + 
  geom_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "STOLEN AUTOMOBILE"), 
                 aes(x = Longitude, y = Latitude), size = .15) + 
  stat_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "STOLEN AUTOMOBILE"), 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.02, 
                 bins = 100, geom = "polygon", contour = TRUE) + 
  scale_fill_gradient(low = "black", high = "red") + 
  scale_alpha(range = c(0.05, 0.15), guide = FALSE)+
  labs(title="STOLEN AUTOMOBILE")



sf <- get_map(location = "san francisco", zoom = 12, maptype = "roadmap", color = "bw")
# Draw the heat map
ggmap(sf, extent = "device") + 
  geom_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "BATTERY"), 
                 aes(x = Longitude, y = Latitude), size = .15) + 
  stat_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "BATTERY"), 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.02, 
                 bins = 100, geom = "polygon", contour = TRUE) + 
  scale_fill_gradient(low = "black", high = "red") + 
  scale_alpha(range = c(0.1, 0.15), guide = FALSE)+
  labs(title="BATTERY")



sf <- get_map(location = "san francisco", zoom = 12, maptype = "roadmap", color = "bw")
# Draw the heat map
ggmap(sf, extent = "device") + 
  geom_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "WARRANT ARREST"), 
                 aes(x = Longitude, y = Latitude), size = .15) + 
  stat_density2d(data = df %>% 
                   filter(year(Date) == 2016 & 
                            Descript == "WARRANT ARREST"), 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.02, 
                 bins = 100, geom = "polygon", contour = TRUE) + 
  scale_fill_gradient(low = "black", high = "red") + 
  scale_alpha(range = c(0.15, 0.2), guide = FALSE)+
  labs(title="WARRANT ARREST")
