library(tidyverse)

# Clear workspace
rm(list = ls())

indir_FC <- "DRI-aquatic-foods/data-raw/raw/FoodData_Central_sr_legacy_food_csv_ 2019-04-02"
indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

### Import data

## USDA data
# downloaded from https://fdc.nal.usda.gov/download-datasets.html (SR-legacy)
# documentation: https://www.ars.usda.gov/arsuserfiles/80400525/data/sr-legacy/sr-legacy_doc.pdf

usda_nutr_orig = read.csv(file.path(indir_FC, "food_nutrient.csv")) 
usda_food_key = read.csv(file.path(indir_FC, "food.csv")) 
usda_portion = read.csv(file.path(indir_FC, "food_portion.csv")) 

usda_nutr_key = read.csv(file.path(indir_FC, "support/nutrient.csv")) 
usda_catg = read.csv(file.path(indir_FC, "support/food_category.csv")) 

## AFCD data
afcd = readRDS("~/AFCD/data-raw/processed/AFCD_data_taxa.Rds") ## LAST UPDATED 8/11/22 

##lysine
lysine = afcd %>% 
  filter(nutrient == "Lysine",
         study_type == "Food Composition Table (FCT)") %>%  
  mutate(value2 = case_when(food_name %in% c("Fish; herring; Atlantic; raw", "Fish; cod; Atlantic; raw") ~ value/1000,
                            TRUE ~ value),
         value3 = value2/1000,
         value = value3,
         nutrient_units = "g") %>%
  select(-value2, -value3)
  
afcd = afcd %>% 
  filter(!nutrient == "Lysine") %>% 
  rbind(lysine)

# ggplot(data = lysine) +
#   geom_boxplot(aes(y = value2)) +
#   facet_wrap(~study_id, scales = "free_y")

## Nutrient key 
# from: https://docs.google.com/spreadsheets/d/1p-bPfBcuVpRLMrJ4zixl-jmoj5yLezHJnptnlu66quk/edit#gid=2124066346
nutrient_key <- read.csv(file.path(indir,'ph_nutrient_key_all_foods.csv'), na.strings=c("","NA"))

# Reformat nutrient key
nutrient_key = nutrient_key %>%
  gather("data_type", "nutrient", 4:5) %>%
  drop_na(nutrient)

# combine food_key database with category, filter for relevant animals (beef, chicken, pork) and raw foods
# nutrients calculated for 100g portions
usda_food_key_all = usda_food_key %>%
  left_join(usda_catg, by=c("food_category_id"="id")) %>% 
  filter(description.y %in% c("Cereal Grains and Pasta",
                              "Poultry Products",
                              "Fruits and Fruit Juices",
                              "Pork Products",
                              "Beef Products",
                              "Vegetables and Vegetable Products",
                              "Nut and Seed Products",
                              "Legumes and Legume Products",
                              "Finfish and Shellfish Products")) %>% 
  filter(str_detect(description.x, "raw"), 
         !str_detect(description.x, "trawberry"), 
         !str_detect(description.x, "Seaweed"))

##Dairy and eggs
dairy_egg = rbind(
  usda_food_key %>%
  left_join(usda_catg, by=c("food_category_id"="id")) %>% 
  filter(description.y %in% c("Dairy and Egg Products"),
         str_detect(description.x, "Egg"), 
         str_detect(description.x, "raw")),
  usda_food_key %>%
    left_join(usda_catg, by=c("food_category_id"="id")) %>% 
    filter(description.y %in% c("Dairy and Egg Products"),
           description.x %in% c("Milk, dry, whole, without added vitamin D",
                                "Milk, fluid, 1% fat, without added vitamin A and vitamin D",
                                "Milk, whole, 3.25% milkfat, without added vitamin A and vitamin D",
                                "Milk, reduced fat, fluid, 2% milkfat, without added vitamin A and vitamin D",
                                "Milk, nonfat, fluid, without added vitamin A and vitamin D (fat free or skim)",
                                "Milk, indian buffalo, fluid")))

##Vegetables (without tubers)
veg_tubers = usda_food_key %>%
    left_join(usda_catg, by=c("food_category_id"="id")) %>% 
    filter(description.y %in% c("Vegetables and Vegetable Products"),
           !str_detect(description.x, regex('potato', ignore_case = T)), 
           !str_detect(description.x, regex('seaweed', ignore_case = T)),
           str_detect(description.x, "raw")) %>% 
  mutate(description.y = "Vegetables (without potatoes)")

usda_food_key_all = rbind(usda_food_key_all, dairy_egg, veg_tubers)

# merge filter animal foods with nutrient database & reformats
usda_raw_cleaned = usda_food_key_all %>%
  left_join(usda_nutr_orig, by="fdc_id") %>%
  select(-c(publication_date, data_type, code)) %>%
  select(id, everything()) %>%
  left_join(usda_nutr_key, by=c("nutrient_id"="id")) %>%
  rename(food_catg=description.y,  # format for joining with AFCD
         food_name=description.x, 
         value=amount, 
         nutrient=name,
         nutrient_units=unit_name) %>%
  mutate(nutrient_orig=nutrient, 
         nutrient_units=tolower(nutrient_units), 
         food_catg=gsub(" Products", "", food_catg),
         food_catg = recode(food_catg, 
                            "Vegetables and Vegetable" = "Vegetables",
                            "Fruits and Fruit Juices" = "Fruits",
                            "Legumes and Legume" = "Legumes",
                            "Cereal Grains and Pasta" = "Cereal Grains",
                            "Finfish and Shellfish" = "Aquatic Foods (USDA)"),
         portion_size="per 100g"
         ) %>%
  select(food_catg, food_name, nutrient, nutrient_orig, value, nutrient_units) %>%
  merge(nutrient_key %>% filter(data_type=="USDA_name"), by="nutrient") %>% # merge nutrient key
  mutate(nutrient=PH_nutrient) %>% #rename nutrient
  select(-PH_nutrient)

# nutrient_key2 = nutrient_key %>% 
#   mutate(nutrient = recode(nutrient, "Total fatty acids, polyunsaturated2" = "Total fatty acids, polyunsaturated"))

### CLEAN AFCD
afcd_raw_cleaned = afcd %>%
  merge(nutrient_key %>% filter(data_type=="AFCD_name"), by="nutrient") %>% # merge nutrient key
  mutate(nutrient=PH_nutrient) %>% #rename nutrient
  select(-PH_nutrient) %>%
  filter(food_prep == "raw") %>%
  mutate(edible_value = case_when(is.na(edible_prop) ~ value, TRUE ~ value * edible_prop)) %>%
  drop_na(nutrient_units, value, edible_value)


# unit conversion function
convert_units <- function(value, unit_orig, unit_new) {  
  if(unit_new=="" | is.na(unit_new)) {
    return(value)
  }
  else { 
    unit_conversionf <- c("g"=1, "mg"=1000, "mcg"=1000000, "ug"=1000000, "µg"=1000000)
    result <- unname(value*unit_conversionf[unit_new]/unit_conversionf[unit_orig])[1]
    return(result)
  }
}

convert_units  <- Vectorize(convert_units)

med_afcd = afcd_raw_cleaned %>% 
  mutate(value_sqt = sqrt(sqrt(value))) %>% 
  group_by(nutrient, nutrient_units) %>% 
  summarise(value_med = median(value_sqt)) %>% 
  mutate(food_catg = "Aquatic Foods",
         value_med = value_med^4)

##units key
nut_key = med_afcd %>% 
  select(nutrient, nutrient_units) %>% 
  unique() %>% 
  rename(unit_conversion = nutrient_units)

usda_data = usda_raw_cleaned %>% 
  left_join(nut_key) %>%
  mutate(unit_conversion = if_else(is.na(unit_conversion), nutrient_units, unit_conversion),
         value=convert_units(value, nutrient_units, unit_conversion),
         nutrient_units=unit_conversion) %>% 
  select(-unit_conversion)

med_usda = usda_data %>% 
  ungroup() %>% 
  mutate(value_sqt = sqrt(sqrt(value))) %>% 
  group_by(nutrient, nutrient_units, food_catg) %>% 
  summarise(value_med = median(value_sqt)) %>% 
  mutate(value_med = value_med^4)

med_all = rbind(med_afcd, med_usda) %>% 
  group_by(nutrient) %>% 
  mutate(max_value = max(value_med),
         cc =n()) %>% 
  ungroup() %>% 
  mutate(prop_value = value_med/max_value)

write.csv(med_all, file.path(indir, "med_usda_afcd_nutrients.csv"), row.names = F)
write.csv(usda_data, file.path(indir, "usda_nutrients_raw.csv"), row.names = F)
write.csv(afcd_raw_cleaned, file.path(indir, "afcd_nutrients_raw.csv"), row.names = F)
#write.csv(dri_cleaned, file.path(indir, "dri_nutrients.csv"))

##Visualize distributions
ggplot(data = usda_data, aes(x = food_catg, y = value)) +
  geom_boxplot() +
  facet_wrap(~nutrient, scales = "free_y") +
  labs(y = "Nutrient concentration (per 100g)", x = "Food category") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 15))
