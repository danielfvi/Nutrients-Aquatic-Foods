## Load libraries
install.packages("dplyr")
library(tidyverse)
library(dplyr)

# Directories
indir <- "data-raw/raw"
outdir <- "data-raw/processed"
plotdir <- "data-raw/figures"

## To load the .RDS file into R
Zhou_Liang_Iodine_Iron_zinc_orig <- read.csv("Global deficiencies/data-raw/raw/Zhou_Liang_Iodine_Iron_zinc.csv")
Zhou_Liang_Vitamins_orig <- read.csv("Global deficiencies/data-raw/raw/Zhou_Liang_Vitamins.csv")
Beal_et_al_2017_orig <- read.csv("Global deficiencies/data-raw/raw/Beal_et_al-2017.csv", comment.char="#")
Passarelli2022_orig <- readRDS("Global deficiencies/data-raw/raw/nutrient_intake_distributions_31countries_expanded_final(1).Rds")
Zhou_Liang_Pop_deficiencies_orig <- read.csv("Global deficiencies/data-raw/raw/Zhou_Liang_Pop_deficiencies.csv")
Golden_et_al_orig <- read_csv("Global deficiencies/data-raw/raw/2017_perc_pop_deficient.csv") 

## To create new dataframe with selected columns
Beal_et_al_2017 <- Beal_et_al_2017_orig[,c("Country", "Micronutrient", "Prevalence of Inadequate Intake", "Year")]

Passarelli2022 <- Passarelli2022_orig[,c("country", "nutrient", "sex_ar", "age_group_ar", "g_sev_ar")]

Zhou_Liang_Iodine_Iron_zinc <- Zhou_Liang_Iodine_Iron_zinc_orig[,c("Country", "indmeta_text", "Val", "Theme_text", "Year", "Age", "Gender", "Link")]

Zhou_Liang_Vitamins <- Zhou_Liang_Vitamins_orig[,c("Country", "Indmeta_text", "Val", "Theme_text", "Year", "Age", "Gender", "Link")]

Zhou_Liang_Pop_deficiencies <- Zhou_Liang_Pop_deficiencies_orig[,c("Country", "Indmeta_text", "Val", "Theme_text", "Year", "Age", "Gender", "Link")]
                
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
