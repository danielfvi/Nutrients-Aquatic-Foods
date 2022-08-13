
library(tidyverse)
library(ggplot2)

### import data 
indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

usda_nutrients <- read.csv(file.path(indir, "usda_ear.csv"))
afcd_nutrients <- read.csv(file.path(indir, "afcd_ear.csv"))

## Inspect nutrients matched
nutrients %>%
  select(c(nutrient, relevance, nutrient_units))

## filter dri_type
## Get whole & muscle tissue data only
afcdEAR3 = afcd_nutrients %>%
  filter(dri_type=="EAR", relevance==3) %>%
  ungroup() %>%
  group_by(nutrient, food_part) %>%
  mutate(n_part = n()) %>%
  mutate(n_part_label=paste0(nutrient,' (n = ', n_part,')')) %>%
  mutate(n_label=paste0(nutrient,' (n = ', n,')'))

usdaEAR3 = usda_nutrients %>% 
  filter(dri_type=="EAR", relevance==3)

# EAR - MUSCLE TISSUE
# RELEVANCE = 3
ggplot(data=filter(afcdEAR3, food_part== "muscle tissue"), aes(x=nutrient)) +
  geom_violin(mapping=aes(y=ref_proportion_lb), width=1, size=.2, alpha=.3) +
  geom_point(position= position_jitter(seed = 1, width = 0.5), mapping=aes(y=ref_proportion_lb, color=n_part_label), size=1, alpha=.4) +
  geom_boxplot(mapping=aes(y=ref_proportion_lb), width=.2,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(data=usdaEAR3, position = position_jitter(width = 0.3), aes(y=ref_proportion_lb, shape=animal_food_catg), colour="red", size=3) +
  scale_y_log10() 

# EAR - WHOLE
# RELEVANCE = 3
ggplot(data=filter(afcdEAR3, food_part== "whole"), aes(x=nutrient)) +
  geom_violin(mapping=aes(y=ref_proportion_lb), width=1, size=.2, alpha=.3) +
  geom_point(position= position_jitter(seed = 1, width = 0.5), mapping=aes(y=ref_proportion_lb, color=n_part_label), size=1, alpha=.4) +
  geom_boxplot(mapping=aes(y=ref_proportion_lb), width=.2,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(data=usdaEAR3, position = position_jitter(width = 0.3), aes(y=ref_proportion_lb, shape=animal_food_catg), colour="red", size=3) +
  scale_y_log10() 

## Get relevant taxa only 
# 1) Ray-finned fish - “actinopterygii”
# 2) Sharks and rays - “chondrichthyes”
# 3) bivalves - “bivalvia”
# 4) crabs, shrimps, lobsters - “malacostraca”
# 5) seaweed - Kingdom: “plantae”, “chromista”
taxa_relevant_class = c("actinopterygii", "chondrichthyes", "bivalvia", "malacostraca")
taxa_relevant_kingdom = c("plantae", "chromista")

afcdEAR3_taxa = nutrients %>%
  filter(dri_type=="EAR" | relevance==3) %>%
  filter(class %in% taxa_relevant_class | kingdom %in% taxa_relevant_kingdom)


# The DRIs apply to the healthy population. RDAs and AIs are levels of intake recommended for individuals




# #important nutrients x = nutrient y = ear prop

# 
# nutrients_foodpart %>% 
#   filter(!is.na(food_part), food_part!="other") %>%
#   mutate(part_label=paste0(food_part,' (n = ',n_part,')')) %>%
#   ggplot(aes(x=nutrient, y=AFCD)) +
#   geom_point(position = position_jitter(seed = 1, width = 0.5), aes(color=nutrient), size=1, alpha=.4) +
#   geom_violin(width=1, size=.2, alpha=.3) + 
#   geom_boxplot(width=.2, alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) + 
#   scale_y_log10() + # scale data 
#   facet_wrap(vars(part_label), scales="free") + 
#   theme(axis.text = element_text(size = 10, angle=45))  
# 




