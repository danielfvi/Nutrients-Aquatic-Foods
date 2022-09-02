## Load libraries
library(tidyverse)
library(ggpubr)
library(sf)

# Clear workspace
rm(list = ls())

global_deficiency_combined <- readRDS("Global deficiencies/data-raw/processed/global_deficiency_combined.Rds") %>% 
  select(iso3c, nutrient, inadequate_intake)

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

# # Set breaks and labels
# breaks_list <- list("DHA+EPA"=c(0, 25, 50),
#                     "Vitamin B12"=c(0, 5, 10),
#                     "Iron"=c(0, 5, 10),
#                     "Zinc"=c(0, 2.5, 5),
#                     "Calcium"=c(0, 2.5, 5),
#                     "Vitamin A"=c(0, 5, 10))
# 
# labels_list <- list("DHA+EPA"=c("0", "25", ">50"),
#                     "Vitamin B12"=c("0", "5", ">10"),
#                     "Iron"=c("0", "5", ">10"),
#                     "Zinc"=c("0", "2.5", ">5"),
#                     "Calcium"=c("0", "2.5", ">5"),
#                     "Vitamin A"=c("0", "5", ">10"))

# percent ASF
ASF_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(global_deficiency_combined %>% filter(nutrient == nut), by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(global_deficiency_combined %>% filter(nutrient == nut), by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(inadequate_intake)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  # if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
  #   legend.pos = "left"
  # }else{
  #   legend.pos = "right"
  # }
  
  # Get breaks and labels
  # breaks <- breaks_list[[nut]]
  # labels <- labels_list[[nut]]
  
  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=inadequate_intake), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=inadequate_intake), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Inadequate\nintake (%)",
                         #breaks=breaks, labels=labels,
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(axis.text = element_blank(),
          #legend.position=legend.pos,
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 13))
  return(plot1)
}

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