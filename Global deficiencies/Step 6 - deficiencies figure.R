## Load libraries
library(tidyverse)
library(ggpubr)
library(sf)

# Clear workspace
rm(list = ls())

indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

nutrient_key <- read.csv(file.path(indir,'ph_nutrient_key_all_foods.csv'), na.strings=c("","NA")) %>% 
  arrange(desc(nut_order))

global_deficiency_combined <- readRDS("Global deficiencies/data-raw/processed/global_deficiency_combined.Rds") %>% 
  select(iso3c, nutrient, inadequate_intake) %>% 
  group_by(nutrient) %>% 
  mutate(sev_med = median(inadequate_intake)) %>% 
  arrange(sev_med) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Vitamin B6" = "B6-Pyridoxine",
                           "Niacin" = "B3-Niacin",
                           "Thiamin" = "B1-Thiamin",
                           "Thiamine" = "B1-Thiamin",
                           "Riboflavin" = "B2-Riboflavin",
                           "Vitamin B12" = "B12-Cobalamine",
                           "Folate" = "B9-Folate",
                           "Fiber" = "Dietary fiber")) %>% 
  filter(!nutrient %in% c("Protein", "Carbohydrates"))

def_nut = data.frame(PH_nutrient = unique(global_deficiency_combined$nutrient),
                     here = "yes")

nut_key = nutrient_key %>% 
  select(PH_nutrient) %>% 
  left_join(def_nut) %>% 
  filter(is.na(here))

df = data.frame(iso3c = NA,
                nutrient = c(nut_key$PH_nutrient),
                inadequate_intake = NA,
                sev_med = NA)

nut_categories = nutrient_key %>% 
  rename(nutrient = PH_nutrient) %>% 
  select(nutrient, type)

global_deficiency_combined = rbind(df, global_deficiency_combined) %>%
  left_join(nut_categories)

nutrient_vec = unique(nutrient_key$PH_nutrient)

#global_deficiency_combined$nutrient = factor(global_deficiency_combined$nutrient, 
#                                             levels = nutrient_vec)

nut_cat = nutrient_key %>%
  rename(nutrient = PH_nutrient) %>%
  select(nutrient, type) %>% 
  distinct(nutrient, .keep_all = T) %>% 
  mutate(col_nut = case_when(type == "Protein" ~ "deepskyblue",
                             type == "Vitamins" ~ "tan1",
                             type == "Minerals" ~ "plum3",
                             type == "Carbohydrates" ~ "olivedrab3",
                             type == "Fats" ~ "khaki3")) %>% 
  filter()

col_vec = c(nut_cat$col_nut[1:19], nut_cat$col_nut[21:41])

###Inadequate Intake
nut_vector = unique(global_deficiency_combined$nutrient)
global_deficiency_combined$nutrient = factor(global_deficiency_combined$nutrient, levels = nut_vector)

p3 = ggplot(data = global_deficiency_combined, aes(y = inadequate_intake, x = nutrient)) +
  #geom_violin(width=1, size=.2, alpha=.3) +
  geom_point(size=1, alpha=.7) +
  geom_boxplot(alpha = 0.3) +
  guides(fill = "none", color = "none") +
  labs(x = "", y = "Inadequate\nintake (%)") +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))
p3


ggsave(pp, filename = "Figures/Figure 3 - deficiencies.jpeg", 
       height = 7, 
       width = 12)

ggsave(p3, filename = "Figures/Figure 3 - deficiencies.pdf", 
       height = 5, 
       width = 8, dpi=600, device=cairo_pdf)

ggsave(pp, filename = "Figures/Figure 2 - v1.jpeg", 
       height = 8, 
       width = 13.5)

ggsave(pp, filename = "Figures/Figure 2 - v1.pdf", 
       height = 8, 
       width = 13.5, dpi=600, device=cairo_pdf)

# Build maps
unique_nut = unique(global_deficiency_combined$nutrient)
for(n in 1:length(unique_nut)){
  assign(paste("p", n, sep="_"), ASF_map(nut = unique_nut[n]))
}

# Merge maps
g <- gridExtra::grid.arrange(p1, p2,
                             p3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename = "Figures/Nutrient supply relative to ASF - coastal.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Nutrient supply relative to ASF - coastal.jpeg", 
       height = 6.2, 
       width = 10)