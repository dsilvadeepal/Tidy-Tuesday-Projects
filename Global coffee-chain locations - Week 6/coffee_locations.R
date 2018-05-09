library(tidyverse)
library(readxl)
library(janitor)
library(ggmap)

coffee <- read_xlsx("week6_coffee_chains.xlsx", sheet = 1) %>%
  clean_names()

table(coffee$country)

coffee_US <- coffee %>%
    filter(country == "US")

#US map
US_map<-get_map(location='united states', source = "google", zoom=4, maptype = "roadmap", color = "bw")

ggmap(US_map) + geom_point(aes(x=longitude, y=latitude),
                        data = coffee_US, na.rm = T, alpha = 0.3, color = "darkgreen")

#Georgia map
coffee_georgia <- coffee %>%
  filter(state_province == "GA")

atlanta_map <- get_map(location='georgia', source = "google", zoom = 7, maptype = "roadmap", color = "bw")

ggmap(atlanta_map) + geom_point(aes(x=longitude, y=latitude),
                           data = coffee_georgia, na.rm = T, alpha = 0.5, color = "darkgreen")

# US map for Starbucks coffee using ggplot
ggplot(data=coffee_US, aes(longitude, latitude)) +
  borders("usa") +
  geom_point(color = "#007706", alpha = 0.3)+
  coord_equal() +
  xlab("Starbucks Stores in United States") +
  ylab("")

    