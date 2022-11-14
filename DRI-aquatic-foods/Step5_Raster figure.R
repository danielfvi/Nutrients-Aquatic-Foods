
library(tidyverse)
library(scales)
library(ggpubr)

# Clear workspace
rm(list = ls())

indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

med_all = read_csv("DRI-aquatic-foods/data-raw/raw/med_usda_afcd_nutrients.csv")

afcd_nutrients_raw <- read_csv("DRI-aquatic-foods/data-raw/raw/afcd_nutrients_raw.csv")

nutrient_key <- read.csv(file.path(indir,'ph_nutrient_key_all_foods.csv'), na.strings=c("","NA")) %>% 
  arrange(desc(order))

nutrient_vec = unique(nutrient_key$PH_nutrient)

med_all$nutrient = factor(med_all$nutrient, levels = nutrient_vec)

nut_cat = nutrient_key %>%
  rename(nutrient = PH_nutrient) %>%
  select(nutrient, type) %>% 
  distinct(nutrient, .keep_all = T) %>% 
  mutate(col_nut = case_when(type == "Protein" ~ "deepskyblue",
                             type == "Vitamins" ~ "tan1",
                             type == "Minerals" ~ "plum3",
                             type == "Carbohydrates" ~ "olivedrab3",
                             type == "Fats" ~ "yellow2"))

col_vec = nut_cat$col_nut

##Raster figure of median values
p1 = ggplot(med_all %>% filter(!food_catg %in% c("Vegetables (without potatoes)", "Aquatic Foods (USDA)")), aes(y = nutrient, x = food_catg, fill = prop_value))+
  geom_tile(colour = "white", size = 1, height = 1) +
  #geom_text(aes(label = pct(perc_value))) +
  #scale_fill_distiller() +
  scale_fill_gradient(low = "white", high = "dodgerblue", breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 3, barheight = 1)) +
  labs(x = "", y = "", title = "", fill = "Relative\nconcentration") +
  theme(text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(color = col_vec),
        legend.position="top",
        #plot.margin = ggplot2::margin(t = 0.18, r = 0, b = 0.89, l = 0, "cm"),
        plot.title = element_text(hjust = 0.5)
  )

p1

## Plot by relevant taxa 
# 1) Ray-finned fish - “actinopterygii”
# 2) Sharks and rays - “chondrichthyes”
# 3) bivalves - “bivalvia”
# 4) crabs, shrimps, lobsters - “malacostraca”
# 5) seaweed - Kingdom: “plantae”, “chromista”
taxa_relevant_class = c("actinopterygii", "chondrichthyes", "bivalvia", "malacostraca")
taxa_relevant_kingdom = c("plantae", "chromista")

med_AFCD = med_all %>% 
  filter(food_catg == "Aquatic Foods") %>% 
  select(nutrient, value_med)

afcd_taxa = afcd_nutrients_raw %>%
  mutate(broad_group = case_when(class == "actinopterygii" ~ "Ray-finned fish",
                                 class == "chondrichthyes" ~ "Sharks and rays",
                                 class == "bivalvia" ~ "Bivalves",
                                 class == "malacostraca" ~ "Crustaceans",
                                 kingdom %in% c("plantae", "chromista") ~ "Seaweed",
                                 TRUE ~ "Other")) %>% 
  filter(!broad_group %in% c("Other", "Seaweed"),
         food_part == "muscle tissue",
         !nutrient == "Sugar") %>%
  group_by(nutrient, broad_group) %>% 
  summarise(value = median(value)) %>% 
  left_join(med_AFCD) %>% 
  drop_na(value_med) %>% 
  mutate(prop_value = value/value_med,
         prop_value = if_else(is.na(prop_value), 0, prop_value),
         prop_value = if_else(prop_value>3, 3, prop_value))

p3 = ggplot(afcd_taxa, aes(y = nutrient, x = broad_group, fill = prop_value))+
  geom_tile(colour = "white", size = 1, height = 1) +
  #geom_text(aes(label = pct(perc_value))) +
  #scale_fill_distiller() +
  scale_fill_gradient(low = "white", high = "red", breaks = c(0, 1.5, 3), labels = c("0", "1.5", ">3")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 3, barheight = 1)) +
  labs(x = "", y = "", title = "", fill = "Relative\nconcentration") +
  theme(text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 90),
        legend.position="top",
        #plot.margin = ggplot2::margin(t = 0.18, r = 0, b = 0.89, l = 0, "cm"),
        plot.title = element_text(hjust = 0.5)
  )
p3

p = ggarrange(p1, p3, ncol = 2, widths = c(7, 5))

p






p2 = ggplot(afcd_taxa, aes(x = nutrient, y = prop_value, color = broad_group)) +
  geom_errorbar(aes(ymin = 1, ymax = prop_value),
                position = position_dodge(width = .7)) +
  geom_point(position = position_dodge(width = .7), size = 3) +
  geom_hline(lty = 2, yintercept = 1) +
  guides(color = guide_legend(nrow=2,byrow=TRUE)) +
  labs(y = "Nutrient concentration\n(relative to median)", x = "", color = "") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position="top")
p2


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




