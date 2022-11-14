###Visualize distributions
library(tidyverse)
usda_nutrients_raw <- read_csv("DRI-aquatic-foods/data-raw/raw/usda_nutrients_raw.csv") %>% 
  select(food_catg, nutrient, value)# %>% 
  #mutate(value_sqt = sqrt(sqrt(value)))

afcd_nutrients_raw <- read_csv("DRI-aquatic-foods/data-raw/raw/afcd_nutrients_raw.csv") %>% 
  mutate(food_catg = "Aquatic Foods") %>% 
  filter(food_part == "muscle tissue") %>% 
  select(food_catg, nutrient, value)

dta_all = rbind(usda_nutrients_raw, afcd_nutrients_raw) %>% 
  mutate(value_sqt = sqrt(sqrt(value))) %>% 
  group_by(food_catg, nutrient) %>% 
  summarise(value_med = median(value_sqt)) %>% 
  mutate(value = value_med^4)

##Visualize distributions
ggplot(data = usda_nutrients_raw, aes(x = food_catg, y = value_sqt)) +
  #geom_boxplot() +
  geom_violin() +
  facet_wrap(~nutrient, scales = "free_y") +
  #scale_y_sqrt() +
  labs(y = "Nutrient concentration (per 100g)", x = "Food category") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 15))
