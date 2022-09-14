## Load libraries
library(tidyverse)
library(ggpubr)

# Clear workspace
rm(list = ls())

global_deficiency_combined <- readRDS("Global deficiencies/data-raw/processed/global_deficiency_combined.Rds") %>% 
  select(iso3c, nutrient, inadequate_intake) %>% 
  group_by(nutrient) %>% 
  mutate(sev_med = median(inadequate_intake)) %>% 
  arrange(sev_med)

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

