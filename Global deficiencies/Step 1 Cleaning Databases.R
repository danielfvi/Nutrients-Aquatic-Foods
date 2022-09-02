## Load libraries
library(tidyverse)
library(countrycode)

# Clear workspace
rm(list = ls())

# Directories
indir <- "Global deficiencies/data-raw/raw"
outdir <- "Global deficiencies/data-raw/processed"
plotdir <- "Global deficiencies/data-raw/figures"

## To load the .RDS file into R
Zhou_Liang_Iodine_Iron_zinc_orig <- read.csv(file.path(indir, "Zhou_Liang_Iodine_Iron_zinc.csv"))
Zhou_Liang_Vitamins_orig <- read.csv(file.path(indir, "Zhou_Liang_Vitamins.csv"))
Beal_et_al_2017_orig <- read.csv(file.path(indir, "Beal_et_al-2017.csv"))
Passarelli2022_orig <- readRDS(file.path(indir, "nutrient_intake_distributions_31countries_expanded_final(1).Rds"))
Zhou_Liang_Pop_deficiencies_orig <- read.csv(file.path(indir, "Zhou_Liang_Pop_deficiencies.csv"))
Golden_et_al_orig <- read_csv(file.path(indir, "2017_perc_pop_deficient.csv")) 

## To create new dataframe with selected columns
Beal_et_al_2017 <- Beal_et_al_2017_orig %>% 
  rename(inadequate_intake = Prevalence.of.Inadequate.Intake,
         nutrient = Micronutrient,
         country = Country,
         year = Year,
         iso3c = ISO3) %>%
  mutate(sex = "Both sexes",
         age_group = "All_ages",
         study_id = "beal_et_al",
         reference = "Beal, T., Massiot, E., Arsenault, J. E., Smith, M. R., & Hijmans, R. J. (2017). Global trends in dietary micronutrient supplies and estimated prevalence of inadequate intakes. PLOS ONE, 12(4), e0175554. https://doi.org/10.1371/journal.pone.0175554") %>% 
  select(study_id, country, iso3c, year, nutrient, sex, age_group, inadequate_intake, reference)

Passarelli2022 <- Passarelli2022_orig %>% 
  rename(inadequate_intake = sev,
         iso3c = iso3) %>% 
  mutate(year = 2017,
         study_id = "passarelli_et_al",
         reference = "Passarelli, S., Free, C. M., et al (2022). Estimating national and subnational nutrient intake distributions of global diets. The American Journal of Clinical Nutrition, 116(2), 551–560. https://doi.org/10.1093/ajcn/nqac108") %>% 
  select(study_id, country, iso3c, year, nutrient, sex, age_group, inadequate_intake, reference)

Zhou_Liang_Pop_deficiencies <- Zhou_Liang_Pop_deficiencies_orig %>% 
  mutate(nutrient = recode(Indmeta_text,
                           "Proportion of people not meeting lower intake threshold for calcium intake" = "Calcium",
                           "Proportion of people not meeting lower intake threshold for dietary fiber intake" = "Fiber",
                           "Proportion of people not meeting lower intake threshold for iodine intake" = "Iodine",
                           "Proportion of people not meeting lower intake threshold for iron intake" = "Iron",
                           "Proportion of people not meeting lower intake threshold for total carbohydrates intake" = "Carbohydrates",
                           "Proportion of people not meeting lower intake threshold for total protein intake" = "Protein",
                           "Proportion of people not meeting lower intake threshold for vitamin A with supplements intake" = "Vitamin A",
                           "Proportion of people not meeting lower intake threshold for vitamin B9(Folate) intake" = "Folate",
                           "Proportion of people not meeting lower intake threshold for vitamin C intake" = "Vitamin C",
                           "Proportion of people not meeting lower intake threshold for vitamin D intake" = "Vitamin D",
                           "Proportion of people not meeting lower intake threshold for zinc intake" = "Zinc")) %>% 
  filter(!Indmeta_text %in% c("Proportion of poeple exceeding upper intake threshold for added sugars intake",
                              "Proportion of poeple exceeding upper intake threshold for dietary fiber intake",
                              "Proportion of poeple exceeding upper intake threshold for dietary sodium intake",
                              "Proportion of poeple exceeding upper intake threshold for saturated fat intake",
                              "Proportion of poeple exceeding upper intake threshold for total carbohydrates intake",
                              "Proportion of poeple exceeding upper intake threshold for total protein intake")) %>% 
  rename(inadequate_intake = Val,
         country = Country,
         year = Year,
         sex = Gender,
         age_group = Age) %>% 
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c'),
         study_id = "zhou_liang_2021",
         reference = "Zhou, B., & Liang, S. (2021). 2. Dietary Intake—Global Nutrition and Health Atlas [Data set]. Harvard Dataverse. https://doi.org/10.7910/DVN/V53P8D") %>% 
  select(study_id, country, iso3c, year, nutrient, sex, age_group, inadequate_intake, reference)

Golden_et_al <- Golden_et_al_orig %>% 
  rename(inadequate_intake = perc_deficient,
         iso3c = iso3) %>% 
  mutate(year = 2017,
         sex = "Both sexes",
         age_group = "All_ages",
         study_id = "golden_et_al",
         reference = "Golden, Christopher D., et al. Aquatic foods to nourish nations. Nature 598.7880 (2021): 315-320.") %>% 
  select(study_id, country, iso3c, year, nutrient, sex, age_group, inadequate_intake, reference)


##Combine raw databases
def_pop_databases = rbind(Passarelli2022, Beal_et_al_2017, Golden_et_al, Zhou_Liang_Pop_deficiencies) %>% 
  drop_na(inadequate_intake) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Vitamin A (RAE)" = "Vitamin A",
                           "Vitamin A, RAE" = "Vitamin A",
                           "Vitamin B-12" = "Vitamin B12"))

saveRDS(def_pop_databases, file.path(outdir, "global_deficiency_raw.Rds"))

##Calculate deficincy by country
def_pop = def_pop_databases %>% 
  group_by(study_id, country, iso3c, year, nutrient, reference) %>% 
  summarise(inadequate_intake = mean(inadequate_intake)) %>% 
  group_by(study_id, country, nutrient) %>% 
  mutate(recent_year = max(year)) %>% 
  ungroup() %>% 
  mutate(is_recent = if_else(recent_year == year, "yes", "no")) %>% 
  filter(is_recent == "yes") %>% 
  select(-recent_year, -is_recent)

saveRDS(def_pop, file.path(outdir, "global_deficiency_ave.Rds"))

##Create one value per country
def_pop_combined = def_pop %>% 
  group_by(country, iso3c, nutrient) %>% 
  summarise(inadequate_intake = mean(inadequate_intake)) %>% 
  ungroup()
  
saveRDS(def_pop_combined, file.path(outdir, "global_deficiency_combined.Rds"))
















## Create new column with study_id and reference
study_id <- "beal_et_al_2017"
Beal_et_al_2017$study_id <- study_id
View(Beal_et_al_2017)
Beal_et_al_2017$reference <- "Beal, T., Massiot, E., Arsenault, J. E., Smith, M. R., & Hijmans, R. J. (2017). Global trends in dietary micronutrient supplies and estimated prevalence of inadequate intakes. PLOS ONE, 12(4), e0175554. https://doi.org/10.1371/journal.pone.0175554
"
Zhou_Liang_Vitamins$study_id <- "zhou_liang_2021"
Zhou_Liang_Iodine_Iron_zinc$study_id <- "zhou_liang_2021"
Zhou_Liang_Pop_deficiencies$study_id <- "zhou_liang_2021"

Zhou_Liang_Vitamins$reference <- "Zhou, B., & Liang, S. (2021). 3. Nutritional Status—Global Nutrition and Health Atlas [Data set]. Harvard Dataverse. https://doi.org/10.7910/DVN/AZAEWH
"
Zhou_Liang_Iodine_Iron_zinc$reference <- "Zhou, B., & Liang, S. (2021). 3. Nutritional Status—Global Nutrition and Health Atlas [Data set]. Harvard Dataverse. https://doi.org/10.7910/DVN/AZAEWH
"
Zhou_Liang_Pop_deficiencies$reference <- "Zhou, B., & Liang, S. (2021). 2. Dietary Intake—Global Nutrition and Health Atlas [Data set]. Harvard Dataverse. https://doi.org/10.7910/DVN/V53P8D
"



## Change name of columns
Zhou_Liang_Iodine_Iron_zinc <- rename(Zhou_Liang_Iodine_Iron_zinc, Indmeta_text = indmeta_text)

## Merge Zhou datasets
Zhou_Liang_2021_Combined <- rbind(Zhou_Liang_Vitamins, Zhou_Liang_Iodine_Iron_zinc, Zhou_Liang_Pop_deficiencies)

## Change names of columns
Zhou_Liang_2021_Combined <- (rename(Zhou_Liang_2021_Combined, Nutrient = Theme_text))
Zhou_Liang_2021_Combined <- (rename(Zhou_Liang_2021_Combined, Value = Val))
Zhou_Liang_2021_Combined <- (rename(Zhou_Liang_2021_Combined, Variable = Indmeta_text))

Beal_et_al_2017 <- (rename(Beal_et_al_2017, Nutrient = Micronutrient))
Beal_et_al_2017 <- (rename(Beal_et_al_2017, Value = 'Prevalence of Inadequate Intake'))
Passarelli2022 <- Passarelli2022 %>%
  #rename
  janitor::clean_names() %>%
  rename(Country = country,
         Nutrient = nutrient,
         Gender = sex_ar,
         Age = age_group_ar,
         Value = g_sev_ar) 
                        
## Add columns for merging
Age <- NA
Beal_et_al_2017$Age <- Age
Gender <- NA
Beal_et_al_2017$Gender <- Gender
Link <- NA
Beal_et_al_2017$Link <- Link
Variable <- '% Population Inadequate Intake'
Beal_et_al_2017$Variable <- Variable

study_id <- "passarelli_2022"
Passarelli2022$study_id <- study_id
reference <- "Passarelli, S., Free, C. M., Allen, L. H., Batis, C., Beal, T., Biltoft-Jensen, A. P., Bromage, S., Cao, L., Castellanos-Gutiérrez, A., Christensen, T., Crispim, S. P., Dekkers, A., De Ridder, K., Kronsteiner-Gicevic, S., Lee, C., Li, Y., Moursi, M., Moyersoen, I., Schmidhuber, J., … Golden, C. D. (2022). Estimating national and subnational nutrient intake distributions of global diets. The American Journal of Clinical Nutrition, 116(2), 551–560. https://doi.org/10.1093/ajcn/nqac108
"
Passarelli2022$reference <- reference
Link <- NA
Passarelli2022$Link <- Link
Year <- NA
Passarelli2022$Year <- Year
Variable <- '% Population Inadequate Intake'
Passarelli2022$Variable <- Variable

# Arrange names
nutrient_def_merged <- rbind(Zhou_Liang_2021_Combined, Beal_et_al_2017, Passarelli2022)



## Export datasets
saveRDS(Zhou_Liang_2021_Combined, file.path(outdir, "Zhou_Liang_2021_Combined.Rds"))
saveRDS(Beal_et_al_2017, file.path(outdir, "Beal_et_al_2017.Rds"))
saveRDS(Passarelli2022, file.path(outdir, "Passarelli2022.Rds"))
saveRDS(Beal_et_al_2017_orig, file.path(indir, "Beal_et_al_2017_orig.Rds"))
saveRDS(Passarelli2022_orig, file.path(indir, "Passarelli2022_orig.Rds"))
saveRDS(Zhou_Liang_Iodine_Iron_zinc_orig, file.path(indir, "Zhou_Liang_Iodine_Iron_zinc_orig.Rds"))
saveRDS(Zhou_Liang_Pop_deficiencies_orig, file.path(indir, "Zhou_Liang_Pop_deficiencies_orig.Rds"))
saveRDS(Zhou_Liang_Vitamins_orig, file.path(indir, "Zhou_Liang_Vitamins_orig.Rds"))
saveRDS(nutrient_def_merged, file.path(outdir, "nutrient_def_merged.Rds"))

