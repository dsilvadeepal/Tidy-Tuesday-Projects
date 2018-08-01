setwd("C:/Users/dsilv/Desktop/Learning/Data Science/Tidy-Tuesday-Projects/Dallas Animal Shelter - Week 18")

library(tidyverse)
library(readxl)

raw_data <- read_xlsx("week18_dallas_animals.xlsx", sheet = "simple")

#Visualizing Outcomes for all Animals

raw_data %>%
  group_by(animal_type, outcome_type) %>%
  summarise(animal_count = n()) %>%
  ggplot(aes(x = animal_type, y = animal_count, fill = outcome_type)) +
  geom_bar(stat = "identity", position = "fill", color = "#303030") +
  labs(title = "Outcome for all Animals",
       x = "Animal",
       y = "Outcome") +
  theme_minimal()

ggsave("Week18_plot1.png")

#####################

#Top Dog Breeds that are Adopted

tidy_data <- raw_data %>%
            filter (outcome_type == "ADOPTION") %>%
            filter(animal_type == "DOG") %>%
            group_by(animal_breed)%>%
            tally() %>%
            top_n(10) %>%
            arrange(desc(n))

#To retain the order in the plot
tidy_data$animal_breed = factor(tidy_data$animal_breed, levels = tidy_data$animal_breed)


tidy_data %>% ggplot(aes(x = animal_breed, y=n)) +
  geom_bar(stat="identity", width = 0.5, fill = "#0e668b") +
  labs(title="Top 10 Dog Breed Adoptions",
       y = "Number of Adoptions",
       x = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Week18_plot2.png")