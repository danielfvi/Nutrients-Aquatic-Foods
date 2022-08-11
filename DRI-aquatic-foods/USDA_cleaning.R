library(tidyverse)
## merge and clean USDA data 
# downloaded from https://fdc.nal.usda.gov/download-datasets.html (SR-legacy)
# documentation: https://www.ars.usda.gov/arsuserfiles/80400525/data/sr-legacy/sr-legacy_doc.pdf

indir <- "DRI-aquatic-foods/data-raw/raw/FoodData_Central_sr_legacy_food_csv_ 2019-04-02"
outdir <- "DRI-aquatic-foods/data-raw/processed"

usda_nutr_orig = read.csv(file.path(indir, "food_nutrient.csv")) 
usda_food_key = read.csv(file.path(indir, "food.csv")) 
usda_portion = read.csv(file.path(indir, "food_portion.csv")) 

usda_nutr_key = read.csv(file.path(indir, "support/nutrient.csv")) 
usda_catg = read.csv(file.path(indir, "support/food_category.csv")) 

# combine food_key database with category, filter for relevant animals (beef, chicken, pork) and raw foods
# nutrients calculated for 100g portions
usda_food_key_animal = usda_food_key %>%
  left_join(usda_catg, by=c("food_category_id"="id")) %>%
  filter(str_detect(description.x, "Chicken|Beef|Pork"), str_detect(description.x, "raw"))

# merge filter animal foods with nutrient database
usda_nutr_cleaned = usda_food_key_animal %>%
  left_join(usda_nutr_orig, by="fdc_id") %>%
  select(-c(publication_date, data_type, code)) %>%
  select(id, everything()) %>%
  left_join(usda_nutr_key, by=c("nutrient_id"="id"))

# format for joining with AFCD
usda_nutr = usda_nutr_cleaned %>%
  rename(animal_food_catg=description.y, 
         animal_food_name=description.x, 
         value=amount, 
         nutrient=name,
         nutrient_units=unit_name) %>%
  mutate(nutrient_orig=nutrient, 
         nutrient_units=tolower(nutrient_units), 
         animal_food_catg=gsub(" Products", "", animal_food_catg)) %>%
  select(animal_food_catg, animal_food_name, nutrient, nutrient_orig, value, nutrient_units)
  