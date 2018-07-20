library(tidyverse)
library(readxl)

#Import Data from Source Sheet
raw_data <- read_xlsx("week16_exercise.xlsx", sheet = "source", range = cell_cols("B:I"))

#Convert to proper data types
raw_data$state = factor(raw_data$state)
raw_data$men_nonworking = as.numeric(raw_data$men_nonworking)
raw_data$women_nonworking = as.numeric(raw_data$women_nonworking)

#Convert data to long format
tidy_data <- raw_data %>% 
  select(state, men_working, men_nonworking, women_working, women_nonworking) %>% 
  gather(key = "class", value = "Percent", -state) %>% 
  separate(col = class, into = c("Gender","Employment")) %>%
  na.omit()

glimpse(tidy_data)

tidy_data %>%
  ggplot() + 
  geom_boxplot(aes(x=Gender,y=Percent,fill = Gender)) + 
  geom_jitter(aes(x=Gender,y=Percent),
              position=position_jitter(width=0.15,height=0),
              alpha=0.6,
              size=1.5,
              show_legend=FALSE,
              color = "#495f77") + 
  guides(fill=FALSE) +
  facet_wrap(~Employment) + 
  labs(title = "Distribution of Exercise in the US", 
       y = "% of adults that meet exercises guidelines",
       x = "") + 
  theme_minimal() +
  scale_fill_manual(values=c("#37Bc98", "#F48F82"))

ggsave("Week16_Boxplot.png")