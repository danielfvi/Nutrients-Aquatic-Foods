###Visualize distributions

usda_nutrients_raw <- read_csv("DRI-aquatic-foods/data-raw/raw/usda_nutrients_raw.csv") %>% 
  select(food_catg, nutrient, value) %>% 
  filter(food_part == "muscle tissue")

afcd_nutrients_raw <- read_csv("DRI-aquatic-foods/data-raw/raw/afcd_nutrients_raw.csv") %>% 
  mutate(food_catg = "Aquatic Foods") %>% 
  select(food_catg, nutrient, value)

dta_all = rbind(usda_nutrients_raw, afcd_nutrients_raw)
##Visualize distributions
ggplot(data = usda_nutrients_raw, aes(x = food_catg, y = value)) +
  geom_boxplot() +
  facet_wrap(~nutrient, scales = "free_y") +
  #scale_y_log10() +
  labs(y = "Nutrient concentration (per 100g)", x = "Food category") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 15))
