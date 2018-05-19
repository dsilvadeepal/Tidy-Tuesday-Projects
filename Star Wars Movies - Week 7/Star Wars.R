library(fivethirtyeight)
library(tidyverse)
library(janitor)
library(ggmap)

URL<-"https://raw.githubusercontent.com/fivethirtyeight/data/master/star-wars-survey/StarWars.csv"

raw_data<-read.csv(URL, stringsAsFactors = FALSE,header = TRUE) %>% 
  clean_names() %>%
  mutate(respondent_id=as.character(respondent_id))

sw_seen_fan <- subset(raw_data[-1, c(1,2,3,34,35,36,37,38)]) %>%
      filter(have_you_seen_any_of_the_6_films_in_the_star_wars_franchise == "Yes")

names(sw_seen_fan) <- c("id", "seen", "fan", "gender", "age", "income", "edu", "region")

sw_seen_fan[,2:8] = lapply(sw_seen_fan[,2:8], as.factor)
str(sw_seen_fan)
summary(sw_seen_fan)

ggplot(sw_seen_fan, aes(x = fan)) +
  geom_bar(aes(fill = gender)) +
  facet_grid(~age) +
  ggtitle("Temp")
