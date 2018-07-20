library(tidyverse)
library(lubridate)



filesToLoad = list.files()
fileList = lapply(filesToLoad,read.csv)
dataset = do.call("rbind",fileList)

rm(fileList)

summary(dataset)

#Clean the dataset
df <- dataset %>%
    filter(Duration != "",
           Distance_Miles != "",
           !is.na(StartLatitude),
           !is.na(StartLongitude),
           Distance_Miles < 100
           ) %>%
    mutate(StartDate = mdy(StartDate),
           EndDate = mdy(EndDate),
           StartTime = hm(StartTime),
           EndTime = hm(EndTime),
           RouteID = as.character(RouteID),
           BikeID = as.character(BikeID),
           Duration = as.numeric(Duration))

df_heatmap <- df %>%
         mutate(year = year(StartDate),
          month = month(StartDate, label=TRUE),
          day = day(StartDate),
          hour = hour(StartTime) ) %>%
     group_by(day, month) %>%
     summarise(count = n()) 

head(df_heatmap)

ggplot(df_heatmap,aes(day,month, fill = count))+
  geom_tile(color= "white",size=0.1) +
  scale_y_continuous(trans = "reverse", breaks = unique(df_heatmap$month))+
  scale_x_continuous(breaks =c(1,10,20,31))


#Start hub with highest number of trips (map)
#Most common end trip stations
#Trips by Day and Hour (1 month)
#Change in Bike share subscribers(ove all months)
