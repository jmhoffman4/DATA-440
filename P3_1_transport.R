rm(list=ls(all=TRUE))

# install.packages("raster", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("maptools", dependencies = TRUE)
# install.packages("spatstat", dependencies = TRUE)
# install.packages("units", dependencies = TRUE)

library(raster)
library(sf)
library(tidyverse)
library(maptools)
library(spatstat)
library(units)

setwd("~/JMHFall2020/DATA440/R/Togo/Data")

##########################################
### Import and subset spatial polygons ###
##########################################

tgo_raster_pop19 <- raster("worldpop/tgo_ppp_2020_constrained.tif")

tgo_int <- read_sf("gadm36_TGO_shp/gadm36_TGO_0.shp")

tgo_adm1_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_1.shp")
tgo_adm2_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_2.shp")

maritime <- tgo_adm1_sf %>%
  filter(NAME_1 == "Maritime")

###############################
### Import and crop rasters ###
###############################

mari_pop15 <- crop(tgo_raster_pop19, maritime)
mari_pop15 <- mask(mari_pop15, maritime)

# confirm projection

plot(mari_pop15)
plot(st_geometry(maritime), add = TRUE)

### load synthetic housholds and persons

load("tgo_synpop.RData")

st_geometry(tgo_synpop) <- tgo_synpop$geometry


### load de facto settlement boundaries

load("all_polys_bomi.RData")

# filter out

all_polys_subdiv <- all_polys_subdiv %>%
  filter(area < 1.50e+03)

# plot & analyze

subdiv_cntr_pts <-  all_polys_subdiv %>% 
  st_centroid() %>% 
  st_cast("MULTIPOINT")

ggplot() +
  geom_sf(data = maritime,
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.15) +
  geom_sf(data = tgo_synpop,
          size = .01,
          alpha = .01) +
  scale_color_gradient(low = "yellow", high = "red") +
  xlab("longitude") + ylab("latitude") +
  ggtitle("Urbanized Areas throughout the county of Bomi, Liberia")

st_voronoi(st_geometry(subdiv_cntr_pts), bomi)

ggsave("maritime.png", plot, width = 10, height = 10, dpi = 300)
