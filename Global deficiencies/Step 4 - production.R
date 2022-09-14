## Load libraries
library(tidyverse)
library(ggpubr)

# Clear workspace
rm(list = ls())

FAO_prod <- read_csv("~/AFCD-R-package/Outputs/FAO_prod_clean.csv")

global_deficiency_combined <- readRDS("Global deficiencies/data-raw/processed/global_deficiency_combined.Rds") %>% 
  select(iso3c, nutrient, inadequate_intake) %>% 
  group_by(nutrient) %>% 
  mutate(sev_med = median(inadequate_intake)) %>% 
  arrange(sev_med)

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

##Mozambique


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

nut_vector = unique(global_deficiency_combined$nutrient)
global_deficiency_combined$nutrient = factor(global_deficiency_combined$nutrient, levels = nut_vector)

p1 = ggplot(data = global_deficiency_combined, aes(x = inadequate_intake, y = nutrient)) +
  #geom_violin(width=1, size=.2, alpha=.3) +
  geom_point(aes(color = nutrient), size=1, alpha=.7) +
  geom_boxplot(aes(fill = nutrient), alpha = 0.3) +
  guides(fill = "none", color = "none") +
  labs(y = "", x = "Inadequate\nintake (%)", title = "Global") +
  theme_bw() +
  theme(text = element_text(size = 16))
p1

###Case studies
p2 = ggplot(data = global_deficiency_combined %>% 
              filter(iso3c == "MOZ") %>% 
              mutate(cc = if_else(inadequate_intake>50, "high", "low")), aes(x = inadequate_intake, y = reorder(nutrient, inadequate_intake), fill = cc)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 50) +
  scale_fill_manual(values = c("grey30", "grey80")) +
  guides(fill = "none") +
  labs(y = "", x = "Inadequate\nintake (%)", title = "Mozambique") +
  theme_bw() +
  theme(text = element_text(size = 16))

p3 = ggplot(data = global_deficiency_combined %>% 
              filter(iso3c == "PHL") %>% 
              mutate(cc = if_else(inadequate_intake>50, "high", "low")), aes(x = inadequate_intake, y = reorder(nutrient, inadequate_intake), fill = cc)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 50) +
  scale_fill_manual(values = c("grey30", "grey80")) +
  guides(fill = "none") +
  labs(y = "", x = "Inadequate\nintake (%)", title = "Phillipines") +
  theme_bw() +
  theme(text = element_text(size = 16))

p = ggarrange(p2, p3, ncol = 2)
p = ggarrange(p1, p2, p3, ncol = 3)
p

