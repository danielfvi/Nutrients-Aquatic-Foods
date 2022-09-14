
library(tidyverse)
library(scales)
library(ggpubr)

# Clear workspace
rm(list = ls())

### import data 
indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

usda_nutrients <- read.csv(file.path(indir, "usda_ear.csv")) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Dietary fiber (total)" = "Dietary fiber",
                           "Folate (vitamin B9)" = "Folate"))

afcd_nutrients <- read.csv(file.path(indir, "afcd_ear.csv")) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Dietary fiber (total)" = "Dietary fiber",
                           "Folate (vitamin B9)" = "Folate"))

# ## Inspect nutrients matched
# nutrients %>%
#   select(c(nutrient, relevance, nutrient_units))

## filter dri_type
## Get whole & muscle tissue data only
afcdEAR3 = afcd_nutrients %>%
  filter(dri_type=="EAR") %>%
  ungroup() %>%
  group_by(nutrient, food_part) %>%
  mutate(n_part = n(),
         n_part_label=paste0(nutrient,' (n = ', n_part,')'),
         n_label=paste0(nutrient,' (n = ', n,')'),
         perc_ref = 100*ref_proportion_mean,
         perc_ref = if_else(perc_ref>100, 100, perc_ref))

usdaEAR3 = usda_nutrients %>% 
  filter(dri_type=="EAR") %>% 
  mutate(perc_ref = 100*ref_proportion_mean,
         perc_ref = if_else(perc_ref>100, 100, perc_ref))

x = afcdEAR3 %>% 
  ungroup() %>% 
  filter(food_part== "muscle tissue") %>% 
  distinct(nutrient, n_part_label) %>%
  mutate(n_label = gsub(" (", '\n(', n_part_label, fixed = T))

x_breaks = x$nutrient
x_labels = x$n_label

# EAR - MUSCLE TISSUE
# RELEVANCE = 3
p1 = ggplot(data=filter(afcdEAR3, food_part== "muscle tissue"), aes(y=nutrient)) +
  geom_violin(mapping=aes(x=ref_proportion_mean), width=1, size=.2, alpha=.3) +
  geom_point(mapping=aes(x=ref_proportion_mean, color=n_part_label), size=1, alpha=.4) +
  geom_boxplot(mapping=aes(x=ref_proportion_mean), width=.5,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(data=usdaEAR3, aes(x=ref_proportion_mean, shape=animal_food_catg), colour="red", size=3) +
  scale_y_discrete(breaks=x_breaks,
                   labels=x_labels) +
  scale_x_log10() +
  #ylim(0, 100) +
  guides(color = "none", shape=guide_legend(nrow=2,byrow=TRUE)) +
  labs(y = "Proportion of EAR (in 100g of muscle tissue)", x = "", shape = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        text = element_text(size = 16),
        plot.margin = ggplot2::margin(t = 0.2, r = 0.2, b = 0.2, l = 0, "cm"))

#p1

p5 = ggarrange(p1, p4,
               ncol = 2)
p5
ggsave(filename = "DRI-Aquatic-Foods/Figures/EAR_prop_raw.jpeg",
       plot = p1, width = 17, height = 5)

# EAR - WHOLE
# RELEVANCE = 3
x = afcdEAR3 %>% 
  ungroup() %>% 
  filter(food_part== "whole") %>% 
  distinct(nutrient, n_part_label) %>%
  mutate(n_label = gsub(" (", '\n(', n_part_label, fixed = T))

x_breaks = x$nutrient
x_labels = x$n_label

p2 = ggplot(data=filter(afcdEAR3, food_part== "whole"), aes(x=nutrient)) +
  geom_violin(mapping=aes(y=ref_proportion_mean), width=1, size=.2, alpha=.3) +
  geom_point(mapping=aes(y=ref_proportion_mean, color=n_part_label), size=1, alpha=.4) +
  geom_boxplot(mapping=aes(y=ref_proportion_mean), width=.2,alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  geom_point(data=usdaEAR3, aes(y=ref_proportion_mean, shape=animal_food_catg), colour="red", size=3) +
  scale_x_discrete(breaks=x_breaks,
                   labels=x_labels) +
  scale_y_log10() +
  #ylim(0, 100) +
  guides(color = "none") +
  labs(y = "Proportion of EAR\n(in 100g of muscle tissue)", x = "", shape = "") +
  theme_bw() +
  theme(legend.position = c(0.95, 0.9),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        text = element_text(size = 16))

p2

ggsave(filename = "DRI-Aquatic-Foods/Figures/EAR_prop_whole.jpeg",
       plot = p2, width = 17, height = 5)

p3 = ggarrange(p1, p2,
               ncol = 1)
p3
ggsave(filename = "DRI-Aquatic-Foods/Figures/EAR_prop_whole_raw.jpeg",
       plot = p3, width = 17, height = 10)

## Get relevant taxa only 
# 1) Ray-finned fish - “actinopterygii”
# 2) Sharks and rays - “chondrichthyes”
# 3) bivalves - “bivalvia”
# 4) crabs, shrimps, lobsters - “malacostraca”
# 5) seaweed - Kingdom: “plantae”, “chromista”
taxa_relevant_class = c("actinopterygii", "chondrichthyes", "bivalvia", "malacostraca")
taxa_relevant_kingdom = c("plantae", "chromista")


afcdEAR3_taxa = afcd_nutrients %>%
  filter(dri_type=="EAR") %>%
  mutate(broad_group = case_when(class == "actinopterygii" ~ "Ray-finned fish",
                                 class == "chondrichthyes" ~ "Sharks and rays",
                                 class == "bivalvia" ~ "Bivalves",
                                 class == "malacostraca" ~ "Crustaceans",
                                 kingdom %in% c("plantae", "chromista") ~ "Seaweed",
                                 TRUE ~ "Other")) %>% 
  filter(!broad_group %in% c("Other", "Seaweed")) %>% 
  group_by(food_part, broad_group) %>%
  mutate(n_part = n(),
         n_part_label=paste0(broad_group,' (n = ', n_part,')'),
         n_label=paste0(broad_group,' (n = ', n,')'))

x = afcdEAR3_taxa %>% 
  ungroup() %>% 
  filter(food_part== "muscle tissue") %>% 
  distinct(broad_group, nutrient, n_part_label) %>%
  mutate(n_label = gsub(" (", '\n(', n_part_label, fixed = T))

x_breaks = x$nutrient
x_labels = x$n_label

# EAR - MUSCLE TISSUE
# RELEVANCE = 3
p4 = ggplot(data=filter(afcdEAR3_taxa, food_part== "muscle tissue"), aes(y=nutrient)) +
  #geom_violin(mapping=aes(y=ref_proportion_mean), width=1, size=.2, alpha=.3) +
  geom_point(mapping=aes(x=ref_proportion_mean, color=broad_group), size=1, alpha=.7) +
  geom_boxplot(mapping=aes(x=ref_proportion_mean, color=broad_group, fill=broad_group),alpha=.3, outlier.colour=rgb(0.1,0.1,0.1, 0.01)) +
  #geom_point(data=usdaEAR3, aes(y=ref_proportion_mean, shape=animal_food_catg), colour="red", size=3) +
  # scale_x_discrete(breaks=x_breaks,
  #                  labels=x_labels) +
  scale_x_log10() +
  #facet_wrap(~broad_group) +
  #ylim(0, 100) +
  guides(color = "none", fill=guide_legend(nrow=2,byrow=TRUE)) +
  labs(y = " ", x = "", fill = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        text = element_text(size = 16))

p4

ggsave(filename = "DRI-Aquatic-Foods/Figures/EAR_prop_raw_taxa.jpeg",
       plot = p4, width = 17, height = 5)

p5 = ggarrange(p1, p4,
               ncol = 2)
p5
ggsave(filename = "DRI-Aquatic-Foods/Figures/EAR_prop_raw_taxa_prop.jpeg",
       plot = p5, width = 17, height = 10)

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




