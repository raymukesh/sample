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



##Load the district shapefiles
site <- st_read("all_districts/all_districts_site_43N.shp") %>% rowid_to_column("ID")

## Load the raster data set - built up raster for all the years
raw_ras <- list.files("raster/", pattern = "*.tif", full.names = TRUE)


## Stack the rasters
lc_90 <- raster("raster/final_1990_v4.tif")

## Check if the projection of site and 
st_crs(raw_stack) == st_crs(site)

lc_90_ras <- raster::rasterize(site, lc_90)

lc_90_df <-  as.data.frame(as.table(crosstab(lc_90_ras, lc_90)*((60*60)/1e6))) %>% 
  pivot_wider(names_from = "final_1990_v4", values_from = "Freq") %>% mutate(layer = as.integer(layer)) %>% 
  rename("water" = "1", "veg" = "2", "built" = "3", "crop" = "4", "barren" = "5")

site_new <- site %>% left_join(lc_90_df, by = c("ID"= "layer")) %>% mutate(built_per = built/dt_area_km*100)

colnames(site_new)

ggplot() +
  geom_sf(data = site_new,  aes(fill = built_per), lwd = 0.5) +
  scale_fill_gradientn(colors = hcl.colors(10, palette = "RdYlBu"))
  scale_fill_distiller(palette = "YlOrBr")
  scale_fill_continuous(colors = hcl.colors(10, palette = "RdYlBu"))




