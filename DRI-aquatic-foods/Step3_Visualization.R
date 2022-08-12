
library(tidyverse)
library(ggplot2)

### import data 
indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

nutrients <- read.csv(file.path(indir, "nutrients_combined.csv"))

## Inspect nutrients matched
nutrients %>%
  select(c(nutrient, relevance, nutrient_units))

## Get whole & muscle tissue data only
nutrients_whole_muscle = nutrients %>%
  filter(food_part=="muscle tissue" | food_part=="whole") %>%
  ungroup() %>%
  group_by(food_part) %>%
  mutate(n_part = n()) 

# The DRIs apply to the healthy population. RDAs and AIs are levels of intake recommended for individuals
nutrients_whole_muscle %>% filter(dri_type=="AI", relevance==3) %>%
  mutate(nutrient_label=paste0(nutrient,' (n = ', n,')')) %>%
  ggplot(aes(x=nutrient)) +
  geom_violin(aes(y=ref_proportion_lb_AFCD_name), width=1, size=.2, alpha=.3) +
  geom_point(position = position_jitter(seed = 1, width = 0.5), aes(y=ref_proportion_lb_AFCD_name, color=nutrient_label), size=1, alpha=.4) +
  geom_boxplot(aes(y=ref_proportion_lb_AFCD_name), width=.2,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(data=subset(nutrients %>% filter(dri_type=="AI", relevance==3), !is.na(ref_proportion_lb_USDA_name)), position = position_jitter(seed = 1, width = 0.3), aes(y=ref_proportion_lb_USDA_name, shape=food_name), colour="red", size=2) +
  scale_y_log10()  # scale data
  

nutrients_whole_muscle %>% filter(dri_type=="AI", relevance==2) %>%
  mutate(nutrient_label=paste0(nutrient,' (n = ', n,')')) %>%
  ggplot(aes(x=nutrient)) +
  geom_violin(aes(y=ref_proportion_lb_AFCD_name), width=1, size=.2, alpha=.3) +
  geom_point(position = position_jitter(seed = 1, width = 0.5), aes(y=ref_proportion_lb_AFCD_name, color=nutrient_label), size=1, alpha=.4) +
  geom_boxplot(aes(y=ref_proportion_lb_AFCD_name), width=.2,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(data=subset(nutrients %>% filter(dri_type=="AI", relevance==2), !is.na(ref_proportion_lb_USDA_name)), position = position_jitter(seed = 1, width = 0.3), aes(y=ref_proportion_lb_USDA_name, shape=food_name), colour="red", size=2) +
  scale_y_log10()  # scale data

nutrients_whole_muscle %>% filter(dri_type=="AI", relevance==1) %>%
  mutate(nutrient_label=paste0(nutrient,' (n = ', n,')')) %>%
  ggplot(aes(x=nutrient)) +
  geom_violin(aes(y=ref_proportion_lb_AFCD_name), width=1, size=.2, alpha=.3) +
  geom_point(position = position_jitter(seed = 1, width = 0.5), aes(y=ref_proportion_lb_AFCD_name, color=nutrient_label), size=1, alpha=.4) +
  geom_boxplot(aes(y=ref_proportion_lb_AFCD_name), width=.2,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(data=subset(nutrients %>% filter(dri_type=="AI", relevance==1), !is.na(ref_proportion_lb_USDA_name)), position = position_jitter(seed = 1, width = 0.3), aes(y=ref_proportion_lb_USDA_name, shape=food_name), colour="red", size=2) +
  scale_y_log10()  # scale data


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




