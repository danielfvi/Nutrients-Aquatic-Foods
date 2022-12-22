## Load libraries
library(tidyverse)
library(ggpubr)
library(sf)

# Clear workspace
rm(list = ls())

indir <- "DRI-aquatic-foods/data-raw/raw"
outdir <- "DRI-aquatic-foods/data-raw/processed"

anemia <- read_csv("Global deficiencies/data-raw/raw/anemia-in-women-15-49-years.csv") %>% 
  rename(country = `Region Name`,
         iso3c = `Region Alpha-3 Code`,
         year = `Start Year`) %>% 
  select(country, iso3c, year, Value) %>% 
  filter(year == 2016)

stunting <- read_csv("Global deficiencies/data-raw/raw/stunting-in-children-under-5-years.csv") %>% 
  group_by(`Region Alpha-3 Code`) %>% 
  mutate(year = max(`Start Year`)) %>% 
  ungroup() %>% 
  mutate(is_recent = if_else(`Start Year` == year, 1, 0)) %>% 
  filter(is_recent == 1) %>% 
  rename(country = `Region Name`,
         iso3c = `Region Alpha-3 Code`) %>% 
  select(iso3c, Value)

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



#Load world map
world <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Extract French Guiana
fguiana <-world %>% 
  sf::st_cast(to="POLYGON") %>% 
  filter(gu_a3=="FRA") %>% 
  mutate(id=1:n()) %>% 
  select(id) %>% 
  filter(id==1)

#World centroids
world_lg <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% 
  mutate(area_sqkm=sf::st_area(.)/(1000*1000)) %>%
  mutate(area_sqkm=as.numeric(area_sqkm)) %>% 
  sf::st_centroid() %>% 
  select(continent, subunit, su_a3, area_sqkm) %>% 
  rename(country=subunit, iso3=su_a3) 

# Small nation centroids
world_tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass = "sf") %>% 
  select(continent, subunit, su_a3) %>% 
  rename(country=subunit, iso3=su_a3) %>% 
  mutate(area_sqkm=10)

# Merge centroids
world_centers <- bind_rows(world_lg, world_tiny)


# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=5),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

####Anemia
# Format data
  dta_sf <- world %>% 
    left_join(anemia, by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(anemia, by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(Value)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  
  plot1 <- ggplot(dta_sf) +
    geom_sf(mapping=aes(fill=Value), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt, mapping=aes(fill=Value), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Prevalence of anemia (%)",
                         breaks=c(10, 35, 60), labels=c(10, 35, 60),
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 3, barheight = 0.8)) +
    #labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(axis.text = element_blank(),
          legend.position="top",
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 13),
          plot.margin = ggplot2::margin(t = 0.2, r = 0.5, b = 0, l = 0.2, "cm"))
#plot1

####Stunting
# Format data
dta_sf <- world %>% 
  left_join(stunting, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_pt <- world_centers %>% 
  left_join(stunting, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(Value)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")


plot2 <- ggplot(dta_sf) +
  geom_sf(mapping=aes(fill=Value), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=Value), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Prevalence of stunting (%)",
                       breaks=c(10, 35, 60), labels=c(10, 35, 60),
                       low="navy", high="darkblue", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 3, barheight = 0.8)) +
  #labs(title = nut)+
  # Theme
  theme_bw() + base_theme +
  theme(axis.text = element_blank(),
        legend.position="bottom",
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 13),
        plot.margin = ggplot2::margin(t = 0, r = 0.5, b = 0.2, l = 0.2, "cm"))
#plot2


###Inadequate Intake
nut_vector = unique(global_deficiency_combined$nutrient)
global_deficiency_combined$nutrient = factor(global_deficiency_combined$nutrient, levels = nut_vector)

p3 = ggplot(data = global_deficiency_combined, aes(x = inadequate_intake, y = nutrient)) +
  #geom_violin(width=1, size=.2, alpha=.3) +
  geom_point(size=1, alpha=.7) +
  geom_boxplot(alpha = 0.3) +
  guides(fill = "none", color = "none") +
  labs(y = "", x = "Inadequate\nintake (%)") +
  theme_bw() +
  theme(text = element_text(size = 16),
        plot.margin = ggplot2::margin(t = 1.95, r = 0, b = 0.2, l = 0, "cm"))
#p3


p = ggarrange(plot1, plot2, ncol = 1, labels = c("", ""), label.x = 0.03, label.y = c(0.75, 1))
pp = ggarrange(p3, p, widths = c(2.5, 5), heights = c(3, 6), labels = c("", ""), label.x = 0.5, label.y = 0.93)
pp

ggsave(pp, filename = "Figures/Figure 2 - v2.jpeg", 
       height = 7, 
       width = 12)

ggsave(pp, filename = "Figures/Figure 2 - v2.pdf", 
       height = 7, 
       width = 12, dpi=600, device=cairo_pdf)

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