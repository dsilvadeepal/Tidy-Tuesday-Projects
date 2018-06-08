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
           StartHub != "",
           EndHub != "") %>%
    mutate(StartDate = mdy(StartDate),
           EndDate = mdy(EndDate),
           StartTime = hm(StartTime),
           EndTime = hm(EndTime),
           RouteID = as.character(RouteID),
           BikeID = as.character(BikeID),
           Duration = as.numeric(Duration))


summary(df)

df %>%
  group_by(StartTime, PaymentPlan) %>%
  summarise(Trips = n()) %>%
  ggplot(aes(StartTime, Trips, color = PaymentPlan)) +
  geom_line()


#Start hub with highest number of trips (map)
#Most common end trip stations
#Trips by Day and Hour (1 month)
#Change in Bike share subscribers(ove all months)
