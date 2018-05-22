library(tidyverse)
library(janitor)

honey <- read.csv("honeyproduction.csv")

str(honey)
summary(honey)

honey_spread <- honey %>% 
  filter(year %in% c(1998,2012)) %>%
  select("state", "totalprod", "year") %>%
  spread(year, totalprod)%>%
  mutate(prodchange = round((`2012` - `1998`)*100/`2012`)) %>%
  filter(!is.na(prodchange)) %>%
  mutate(prodchange_type = ifelse(prodchange < 0, "decreased", "increased") )

honey_spread <- honey_spread[order(honey_spread$prodchange),]
honey_spread$state <- factor(honey_spread$state, levels = honey_spread$state)

#Diverging Chart

ggplot(honey_spread, aes(x=state, y=prodchange, label=prodchange)) + 
  geom_point(stat="identity", aes(col=prodchange_type), size=7)  +
  scale_color_manual(name="Production Change %", 
                     labels = c("Decreased", "Increased"), 
                     values = c("increased"="#00ba38", "decreased"="#f8766d")) + 
  geom_text(color="white", size=2.5) +
  labs(title="Change in Honey Production Across US (1998 to 2012)", y = "Percent change in Production", x = NULL) +
  coord_flip() +
  theme_minimal()

#Analyze the consumption vs production of honey
honey$consumption <- honey$totalprod - honey$stocks

honey_deficit<- honey %>%
  group_by(year) %>%
  summarise(TotConsumption = sum(consumption)/1000000, TotProd = sum(totalprod)/1000000) %>%
  gather(Totals, values, 2:3)

ggplot(honey_deficit, aes(x = year)) +
  geom_line(aes(y = values, col = Totals) ) +
  labs(title="Production vs Consumption in US",
       y="Honey in Million lbs", x = NULL,
       color=NULL) + 
  scale_color_manual(labels = c("Total Consumption", "Total Production"), 
                     values = c("TotConsumption"="green", "TotProd"="blue"))

#Amount of honey produced by colony
honey_by_col <- honey %>% 
  filter(year %in% c(1998,2012)) %>%
  select("state", "yieldpercol", "year") %>%
  group_by(state) %>%
  summarise(avgyield = mean(yieldpercol)) %>%
  top_n(20)
  

ggplot(honey_by_col, aes(x = reorder(state, -avgyield), y = avgyield)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#A9A807") +
  labs(x = NULL, y = "Avg Yield per Colony", title = "Top 20 States - Average Yield per Colony")
  
