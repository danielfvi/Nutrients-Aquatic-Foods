
library(tidyverse)
library(ggplot2)

### import data 
indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

afcd = readRDS(file.path(indir, "AFCD_data_taxa.Rds")) ## LAST UPDATED 8/10/22
nutrient_relevance = read.csv(file.path(indir, "nutrient_relevance.csv")) 
usda_orig = read.csv(file.path(indir,'USDA_food_comp_raw.csv'))
rdi_orig <- readRDS(file.path(indir,"dietary_reference_intake_data.Rds"))
nutrient_key <- read.csv(file.path(indir,'nutrient_key.csv')) %>%
  select(-1)

## rename AFCD combined nutrients

# vitamin_a_combined (nutrient orig) = Vitamin A (nutrient)
nutrients_afcd = afcd %>% select(nutrient, nutrient_orig) %>%
  unique()

### functions
# unit conversions: 

## filter and prepare afcd data 
# focus on raw foods 
afcd_orig = afcd 
afcd_raw = afcd_orig %>% 
  filter(food_prep == "raw") %>%
  mutate(edible_value = case_when(is.na(edible_prop) ~ value, TRUE ~ value * edible_prop)) %>%
  drop_na(nutrient_units, value, edible_value) %>%
  mutate(source="AFCD")

### clean/format usda data 
usda_raw = usda_orig %>%
  gather('nutrient', 'value', 3:22) %>%
  mutate(nutrient=gsub('PER100g', "", nutrient),
         nutrient=gsub('_', " ", nutrient)) %>%
  mutate(units=stringi::stri_extract_last_words(nutrient)) %>%
  mutate(nutrient=gsub("\\s*\\w*$", "", nutrient)) %>%
  filter(value!="--") %>%
  filter(str_detect(name, "Chicken|Beef|Pork"), str_detect(name, "raw")) %>%
  mutate(animal_food_name=gsub(",", "", word(name, 1))) %>%
  mutate(nutrient_orig=nutrient) %>%
  rename(nutrient_units=units) %>%
  select(c(animal_food_name, nutrient, nutrient_orig, value, nutrient_units))  

# find median values for each meat
usda_med = usda_raw %>%
  group_by(animal_food_name, nutrient, nutrient_units) %>%
  summarise(value = median(as.numeric(value))) %>%
  ungroup() %>%
  mutate(source="USDA")
  
## clean RDI data  
rdi_orig = rdi_orig %>%
  mutate(dri_type=recode(dri_type, 
                  "Estimated Average Requirement (EAR)"="EAR",
                  "Adequate Intake (AI)" = "AI", 
                  "Tolerable Upper Intake Level (UL)" = "UL", 
                  "Recommended Dietary Allowance (RDA)" = "RDA")
        ) %>%
  rename(ref_value=value) %>%
  #filter(dri_type == "Estimated Average Requirement (EAR)") %>%
  mutate(ref_units=gsub("/d", "", units)) %>%
  drop_na(ref_value, ref_units) %>%
  mutate(nutrient_units=ref_units) #replace nutrient units

### merge USDA + AFCD data 
species_data = usda_med %>%
  bind_rows(afcd_raw) 

### export nutrient names and units for cleaning 
nutrients_to_clean = species_data %>%
  select(nutrient, nutrient_units, nutrient_type, nutrient_desc, source) %>%
  unique() %>%
  bind_rows(rdi_orig %>% select(nutrient, nutrient_units)) %>%
  mutate(source=case_when(is.na(source) ~ "RDI", TRUE ~ source)) %>%
  drop_na(nutrient) %>%
  unique()

write.csv(nutrients_to_clean, "output/nutrients_to_clean.csv")

### merge new nutrient names, units & relevance
# unit convnersion function
convert_units <- function(value, unit_orig, unit_new) {  
  if(unit_new=="" | is.na(unit_new)) {
    return(value)
  }
  else { 
    unit_conversionf <- c("g"=1, "mg"=1000, "mcg"=1000000)
    result <- unname(value*unit_conversionf[unit_new]/unit_conversionf[unit_orig])[1]
    return(result)
  }
}

convert_units  <- Vectorize(convert_units)

# apply unit conversion and replace nutrient_units
species_data2 = species_data %>% 
  left_join(nutrient_key) %>%
  mutate(value=convert_units(value, nutrient_units, units_conversion)) %>%
  mutate(edible_value = case_when(is.na(edible_prop) ~ value, TRUE ~ value * edible_prop)) %>%
  mutate(nutrient_units_orig=nutrient_units, 
         nutrient_units=case_when(units_conversion!="" ~ units_conversion, 
                                  TRUE ~ nutrient_units)) %>%
  select(-units_conversion)

# ------------------------------------------------------------
  
# find units to convert 
t <- species_data2 %>%
  select(nutrient, nutrient_units, units_conversion) %>%
  unique() %>%
  filter(units_conversion!="", units_conversion!=nutrient_units)

# get upper and lower bounds for all RDI groups 
ref_ul = rdi_orig %>% 
  group_by(nutrient, dri_type) %>%
  mutate(
    max_ref = max(ref_value),
    min_ref = min(ref_value)
  ) %>%
  select(c(sex, age_range,  dri_type, nutrient_type, nutrient, units, max_ref, min_ref, nutrient_units)) %>%
  unique()

# # find incongruous units and change above 
# change_units = nutrients_ul %>%
#   mutate(units=paste(nutrient_units, ref_units)) %>%
#   # filter(units=="iu µg") %>%
#   select(nutrient, units) %>%
#   unique()


# get upper and lower bound of proportion given (avg0 )
nutrients_ul = species_data2 %>%
  merge(ref_ul, by=c("nutrient", "nutrient_units")) %>%
  select(1:36, animal_food_name, source, relevance, units, sex, age_range, dri_type, nutrient_units, min_ref, max_ref) %>%
  # need to move this up
  # mutate(edible_value=case_when(nutrient_units=="g" & ref_units=="mg" ~ edible_value*1000,
  #                        TRUE ~ edible_value), # when species units is g and ear_units are mg
  #        nutrient_units=case_when(nutrient_units=="g" & ref_units=="mg" ~ "mg",
  #                                 TRUE ~ nutrient_units)) %>% # change units
  # mutate(edible_value=case_when(nutrient_units=="mg" & ref_units=="g" ~ edible_value/1000,
  #                        TRUE ~ edible_value), # when species units is g and ear_units are mg
  #        nutrient_units=case_when(nutrient_units=="mg" & ref_units=="g" ~ "g",
  #                                 TRUE ~ nutrient_units)) %>% # change units
  # mutate(edible_value=case_when(nutrient_units=="mg" & ref_units=="µg" ~ edible_value*1000,
  #                        TRUE ~ edible_value), # when species units is g and ear_units are mg
  #        nutrient_units=case_when(nutrient_units=="mg" & ref_units=="µg" ~ "µg",
  #                                 TRUE ~ nutrient_units)) %>% # change units
  # mutate(edible_value=case_when(nutrient_units=="mcg" & ref_units=="mg" ~ edible_value/1000,
  #                        TRUE ~ edible_value), # when species units is g and ear_units are mg
  #        nutrient_units=case_when(nutrient_units=="mcg" & ref_units=="mg" ~ "mg",
  #                                 TRUE ~ nutrient_units)) %>% # change units
  # need to deal with IU, and MCG (case by case_
  mutate(ref_proportion_lb = edible_value/max_ref,
         ref_perc_lb = 100*edible_value/max_ref,
         ref_proportion_ub = edible_value/min_ref,
         ref_perc_ub = 100*edible_value/min_ref) %>%
  select(-c(age_range, sex)) %>%
  unique() %>%
  arrange(dri_type, nutrient, sciname) %>%
  group_by(nutrient) %>%
  mutate(n = n()) %>%
  spread(source, ref_proportion_lb) %>%
  filter(n>100) 

## Inspect nutrients matched
nutrients_ul %>%
  select(c(nutrient, relevance, nutrient_units)) %>%
  unique()

t = nutrients_ul %>%
  filter(is.na(AFCD))

  
# important nutrients for public health 
# based on erin's table
nutrients_important = nutrients_ul %>%
  filter(relevance==3)

nutrients_medium = nutrients_ul %>%
  filter(relevance==2)

nutrients_foodpart = nutrients_ul %>% 
  left_join(filter(nutrients_ul, dri_type=="AI")) %>% 
  group_by(nutrient, food_part) %>%
  filter(n>15) %>%
  ungroup() %>%
  group_by(food_part) %>%
  mutate(n_part = n()) %>%
  filter(food_part=="muscle tissue" | food_part=="whole")

#important nutrients x = nutrient y = ear prop
nutrients_ul %>% filter(dri_type=="AI") %>%
  mutate(nutrient_label=paste0(nutrient,' (n = ', n,')')) %>%
  ggplot(aes(x=nutrient)) +
  geom_violin(aes(y=AFCD), width=1, size=.2, alpha=.3) +
  geom_point(position = position_jitter(seed = 1, width = 0.5), aes(y=AFCD, color=nutrient_label), size=1, alpha=.4) +
  geom_boxplot(aes(y=AFCD), width=.2,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(position = position_jitter(seed = 1, width = 0.3), aes(y=USDA, shape=animal_food_name), colour="red", size=2) +
  scale_y_log10()  # scale data 

nutrients_foodpart %>% 
  filter(!is.na(food_part), food_part!="other") %>%
  mutate(part_label=paste0(food_part,' (n = ',n_part,')')) %>%
  ggplot(aes(x=nutrient, y=AFCD)) +
  geom_point(position = position_jitter(seed = 1, width = 0.5), aes(color=nutrient), size=1, alpha=.4) +
  geom_violin(width=1, size=.2, alpha=.3) + 
  geom_boxplot(width=.2, alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) + 
  scale_y_log10() + # scale data 
  facet_wrap(vars(part_label), scales="free") + 
  theme(axis.text = element_text(size = 10, angle=45))  

# for each DRI type, which nutrients matched to AFCD are available
count_dri = nutrients_ul %>% select(nutrient, dri_type) %>%
  unique() %>%
  table() %>%
  as.data.frame() %>%
  spread(dri_type, Freq)

kableExtra::kable(count_dri) %>%
  kableExtra::kable_styling(latex_options = "striped")

# standard deviation by species
species_means =plyr::ddply(nutrients_ul,~sciname+nutrient, summarise, mean=mean(edible_value), sd=sd(edible_value)) %>%
  drop_na(sd, mean, sciname) %>%
  mutate(sd.norm=sd/mean) %>%
  filter(sd!=0, sd.norm>=1) %>% # filter out the scinames that have low normalized sd 
  arrange(sd.norm) 

ggplot(species_means, aes(x=sciname, y=mean)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  facet_wrap(vars(nutrient), scales="free") 







# export data 
# write.csv(median_nutrients,"output/median_nutrients.csv", row.names = FALSE)
# write.csv(nutrients_ul,"output/ub_nutrients.csv", row.names = FALSE)
# write.csv(nutrients,"output/nutrients.csv", row.names = FALSE)
# write.csv(nutrient_outliers,"output/outliers.csv", row.names = FALSE)
# write.csv(outliers_count,"output/outliers_count.csv", row.names = FALSE)


