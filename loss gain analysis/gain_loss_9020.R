



## NOTE ==== The Original Raster resolution is in 60m x 60m, do not change to other resolution for area
##              calculation as it will cause error in area.

library(raster)
library(sf)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(tmap)
library(mapview)
library(ggspatial)
library(terra)
library(ggrepel)



source("rasterdf.R")

##Load the district shapefiles
site <- st_read("all_districts/all_districts_site_43N.shp")

site <- cbind(site, st_coordinates(st_centroid(site)))

ggplot(site) +
  geom_sf(fill = NA, lwd = 0.4) +
  geom_point(aes(x=X, y=Y), color = NA) +
  geom_text_repel(aes(x=X, y=Y, label = district), 
                  size = 4, point.padding = 0.2,
                  nudge_x = .15,
                  nudge_y = .5,
                  segment.curvature = -1e-20,
                  arrow = arrow(length = unit(0.015, "npc")))


## Load the raster data set - built up raster for all the years
raw_ras <- list.files("raster/", pattern = "*.tif", full.names = TRUE)


## Stack the rasters
raw_stack <- raster::stack(raw_ras[c(1,7)])

## Check if the projection of site and 
st_crs(raw_stack) == st_crs(site)


## Rename the raster
names(raw_stack) <- c(1990L, 2020L)

## Increase the raster resolution fro 60m to 180m - Original was 60m x 60m - Do not change to 120m as it causes change in area
#raw_stack <- raster::aggregate(raw_stack, fact = 2, fun = "mean")


lc_y90 <- raw_stack[[1]]
lc_y20 <- raw_stack[[2]]


## AGRICULTURE ====
agriLoss <- lc_y90 == 4 & lc_y20 != 4
agriGain <- lc_y90 != 4 & lc_y20 == 4

agriChange <- 1 + agriLoss + agriGain*2

agriChange_df <- as.data.frame(agriChange, xy=TRUE) %>% drop_na()

agriChange_df %>% 
ggplot() +
  geom_raster(aes(x=x, y=y, fill = as.character(layer))) +
  geom_sf(data = site, fill = NA, lwd = 0.4) +
  scale_fill_manual(values = c("grey90", "red", "green"),
                    labels = c("No Change", "Loss", "Gain"))


## Exchange between BuiltUp and Agri =====

agriToBuilt <- lc_y90 == 4 & lc_y20 == 3
builtToAgri <- lc_y90 == 3 & lc_y20 == 4


agriToBuilt_map <- as.data.frame(agriToBuilt, xy=TRUE) %>% drop_na()

agriToBuilt_map %>% filter(layer == "TRUE") %>% 
  ggplot() +
  geom_tile(aes(x=x, y=y, fill = as.character(layer)), show.legend = F) +
  geom_sf(data = site, fill = NA, lwd = 0.4) +
  scale_fill_manual(values = c("red")) +
  xlab("") + ylab("") + guides(fill = guide_legend(byrow = TRUE)) +
  
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm"),
    line_width = 0.5,
    height = unit(0.3, "cm")
  ) +

  annotation_north_arrow(
    location = "br", which_north = "true",
    height = unit(2, "cm"),
    width = unit(2, "cm"),
    pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_width = 1,
      line_col = "black",
      fill = c("white", "black"),
      text_col = "black",
      text_family = "",
      text_face = NULL,
      text_size = 13,
      text_angle = 0
    )) +
  
  theme_void() +
  theme(legend.text = element_text(size = 20),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "NONE")


ggsave("output/agri_to_built.png", height = 8, width = 11, units = 'in', dpi= 200)




## Calculate the area in km from agri to built and built to agri
agriToBuilt_df <- freq(agriToBuilt, usenames = TRUE) %>% as.data.frame() %>% drop_na() %>% 
  filter(value == 1) %>% mutate(area_km = (count*60*60)/1e6)

builtToAgri_df <- freq(builtToAgri, usenames = TRUE) %>% as.data.frame() %>% drop_na() %>% 
  filter(value == 1) %>% mutate(area_km = (count*60*60)/1e6)

## Net Change Between Agriculture and Built Up
net_change_agri_built <- builtToAgri_df[1,3] - agriToBuilt_df[1,3]






raster::crosstab(lc_y90, lc_y20)


as.table(raster::crosstab(lc_y90, lc_y20))

