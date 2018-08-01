
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)


raw_data <- read_xlsx("week18_dallas_animals.xlsx", sheet = "simple")

str(raw_data)

tidy_data <- raw_data %>%
            filter (outcome_type == "ADOPTION") %>%
            filter(animal_type == "DOG") %>%
            group_by(animal_breed)%>%
            tally() %>%
            top_n(10) %>%
            arrange(desc(n))

#To retain the order in the plot
tidy_data$animal_breed = factor(tidy_data$animal_breed, levels = tidy_data$animal_breed)

glimpse(tidy_data)

#Top Dog Breeds that are Adopted
tidy_data %>% ggplot(aes(x = animal_breed, y=n)) +
  geom_bar(stat="identity", width = 0.5, fill = "#0e668b") +
  labs(title="Top 10 Dog Breeds That Are Adopted",
       y = "Number of Adoptions",
       x = "")


