library(tidyverse)
library(maditr)

# Clear workspace
rm(list = ls())

indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

dri_data <- read.csv(file.path(indir, "dri_nutrients.csv"))
afcd_data <- read.csv(file.path(indir, "afcd_nutrients.csv"))
usda_data <- read.csv(file.path(indir, "usda_nutrients.csv"))


### Functions - defined here for later
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

## get units and nutrients from DRI 
nutr_unit_key = dri_data %>%
  select(nutrient, nutrient_units) %>%
  rename(unit_conversion=nutrient_units) %>%
  filter(nutrient!="Protein") %>% # removing protein for now
  unique() 

## merge usda + afcd data 
species_data = usda_data %>%
  bind_rows(afcd_data) 

# convert units 
species_data2 = species_data %>% 
  merge(nutr_unit_key) %>%
  mutate(value=convert_units(value, nutrient_units, unit_conversion)) %>%
  mutate(edible_value = case_when(is.na(edible_prop) ~ value, TRUE ~ value * edible_prop)) %>%
  mutate(nutrient_units_orig=nutrient_units, 
         nutrient_units=unit_conversion) %>%
  select(-unit_conversion)
  
## Inspect: all nutrients have unique units
species_data2 %>%
  select(nutrient, nutrient_units) %>%
  unique() 


### DRI: get upper and lower bounds for all RDI groups (can also change this code to find the mean)
ref_ul = dri_data %>% 
  filter(nutrient_units!="g/kg" | nutrient!="Protein" ) %>%
  group_by(nutrient, dri_type) %>%
  mutate(
    max_ref = max(ref_value),
    min_ref = min(ref_value), 
    mean_ref = mean(ref_value)
  ) %>%
  select(-age_range, -sex, -stage, -sex_stage, -data_type, -relevance, -X, -units, -ref_value) %>%
  unique() 

species_long = species_data2 %>%
  group_by(nutrient, data_type) %>%
  mutate(n = n()) %>%
  filter((data_type=="AFCD_name") | data_type=="USDA_name") %>%
  merge(ref_ul, by=c("nutrient", "nutrient_units")) %>%
  select(1:44, animal_food_catg, n, dri_type, nutrient_units, min_ref, max_ref, mean_ref) %>%
  mutate(ID = row_number(), 
         ref_proportion_lb = edible_value/max_ref,
         ref_proportion_ub = edible_value/min_ref,
         ref_proportion_mean = edible_value/mean_ref) %>%
  select(-c(min_ref, max_ref, mean_ref)) %>%
  select(ID, everything()) 

# species_wide = species_long %>%
#   dcast(... ~ data_type, value.var=c("ref_proportion_lb", "ref_proportion_ub", "ref_proportion_mean")) %>%
#   mutate(data_type=case_when(!is.na(ref_proportion_lb_AFCD_name) ~ "AFCD", TRUE ~ "USDA"))

# unmerge datasets
usda_data = species_long %>%
  filter(data_type=="USDA_name") %>%
  select(where(function(x) any(!is.na(x))))

afcd_data = species_long %>%
  filter(data_type=="AFCD_name") %>%
  select(where(function(x) any(!is.na(x))))

# write data
write.csv(usda_data, file.path(indir, "usda_ear.csv"))
write.csv(afcd_data, file.path(indir, "afcd_ear.csv"))


