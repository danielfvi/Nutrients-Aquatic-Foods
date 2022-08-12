library(tidyverse)


indir_FC <- "DRI-aquatic-foods/data-raw/raw/FoodData_Central_sr_legacy_food_csv_ 2019-04-02"
indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

### Import data
## DRI data
dri_orig <- readRDS(file.path(indir,"dietary_reference_intake_data.Rds"))

## USDA data
# downloaded from https://fdc.nal.usda.gov/download-datasets.html (SR-legacy)
# documentation: https://www.ars.usda.gov/arsuserfiles/80400525/data/sr-legacy/sr-legacy_doc.pdf

usda_nutr_orig = read.csv(file.path(indir_FC, "food_nutrient.csv")) 
usda_food_key = read.csv(file.path(indir_FC, "food.csv")) 
usda_portion = read.csv(file.path(indir_FC, "food_portion.csv")) 

usda_nutr_key = read.csv(file.path(indir_FC, "support/nutrient.csv")) 
usda_catg = read.csv(file.path(indir_FC, "support/food_category.csv")) 

## AFCD data
afcd = readRDS(file.path(indir, "AFCD_data_sci.Rds")) ## LAST UPDATED 8/11/22 

## Nutrient key 
# from: https://docs.google.com/spreadsheets/d/1p-bPfBcuVpRLMrJ4zixl-jmoj5yLezHJnptnlu66quk/edit#gid=2124066346
nutrient_key <- read.csv(file.path(indir,'ph_nutrient_key.csv'), na.strings=c("","NA")) %>%
  select(-(6:7))

# Reformat nutrient key
nutrient_key = nutrient_key %>%
  gather("data_type", "nutrient", 3:5) %>%
  drop_na(nutrient)

### CLEAN DRI data
dri_cleaned = dri_orig %>%
  mutate(dri_type=recode(dri_type, 
                         "Estimated Average Requirement (EAR)"="EAR",
                         "Adequate Intake (AI)" = "AI", 
                         "Tolerable Upper Intake Level (UL)" = "UL", 
                         "Recommended Dietary Allowance (RDA)" = "RDA")
  ) %>%
  rename(ref_value=value) %>%
  mutate(ref_units=gsub("/d", "", units)) %>%
  drop_na(ref_value, ref_units) %>%
  mutate(nutrient_units=ref_units) %>%
  merge(nutrient_key %>% filter(data_type=="RDI_name"), by="nutrient") %>% # merge nutrient key
  mutate(nutrient=PH_nutrient) %>% #rename nutrient
  select(-PH_nutrient)


# combine food_key database with category, filter for relevant animals (beef, chicken, pork) and raw foods
# nutrients calculated for 100g portions
usda_food_key_animal = usda_food_key %>%
  left_join(usda_catg, by=c("food_category_id"="id")) %>%
  filter(str_detect(description.x, "Chicken|Beef|Pork"), str_detect(description.x, "raw"))

# merge filter animal foods with nutrient database & reformats
usda_raw_cleaned = usda_food_key_animal %>%
  left_join(usda_nutr_orig, by="fdc_id") %>%
  select(-c(publication_date, data_type, code)) %>%
  select(id, everything()) %>%
  left_join(usda_nutr_key, by=c("nutrient_id"="id")) %>%
  rename(animal_food_catg=description.y,  # format for joining with AFCD
         animal_food_name=description.x, 
         value=amount, 
         nutrient=name,
         nutrient_units=unit_name) %>%
  mutate(nutrient_orig=nutrient, 
         nutrient_units=tolower(nutrient_units), 
         animal_food_catg=gsub(" Products", "", animal_food_catg),
         portion_size="per 100g"
         ) %>%
  select(animal_food_catg, animal_food_name, nutrient, nutrient_orig, value, nutrient_units) %>%
  merge(nutrient_key %>% filter(data_type=="USDA_name"), by="nutrient") %>% # merge nutrient key
  mutate(nutrient=PH_nutrient) %>% #rename nutrient
  select(-PH_nutrient)

### CLEAN AFCD
afcd_raw_cleaned = afcd %>%
  merge(nutrient_key %>% filter(data_type=="AFCD_name"), by="nutrient") %>% # merge nutrient key
  mutate(nutrient=PH_nutrient) %>% #rename nutrient
  select(-PH_nutrient) %>%
  filter(food_prep == "raw") %>%
  mutate(edible_value = case_when(is.na(edible_prop) ~ value, TRUE ~ value * edible_prop)) %>%
  drop_na(nutrient_units, value, edible_value) 

write.csv(usda_raw_cleaned, file.path(indir, "usda_nutrients.csv"))
write.csv(afcd_raw_cleaned, file.path(indir, "afcd_nutrients.csv"))
write.csv(dri_cleaned, file.path(indir, "dri_nutrients.csv"))


  