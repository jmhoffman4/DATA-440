rm(list=ls(all=TRUE))

# install.packages("tidyverse", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)
#install.packages("raster", dependencies = TRUE)
#install.packages("doParallel", dependencies = TRUE)
#install.packages("snow", dependencies = TRUE)
library(raster)
library(tidyverse)
library(sf)
library(doParallel)
library(snow)
setwd("~/Fall2020/DATA440/R/8_28_Extracting/Data")

pse_int  <- read_sf("gadm36_PSE_shp/gadm36_PSE_0.shp")
pse_adm1  <- read_sf("gadm36_PSE_shp/gadm36_PSE_1.shp")
pse_adm2  <- read_sf("gadm36_PSE_shp/gadm36_PSE_2.shp")

ggplot() +
  geom_sf(data = pse_adm2,
          size = 1,
          color = "purple",
          fill = "gray",
          alpha = 0) +
  geom_sf(data = pse_adm1,
          size = 1,
          color = "gray50",
          alpha = .5) +
  geom_sf(data = pse_int,
          size = 1,
          alpha = .50) +
  geom_sf_text(data = pse_adm2,
               aes(label = "label 1"),
               size = 1) +
  geom_sf_text(data = pse_adm1,
               aes(label = "variable_name"),
               size = 1)
#ggsave("palestine.png")

pse_raster_pop19 <- raster("world_pop/pse_ppp_2019_UNadj.tif")

plot(pse_raster_pop19)
plot(st_geometry(pse_adm2), add = TRUE)

#detectCores()
#ncores <- 3
#beginCluster(ncores)
#pop_vals_adm2 <- raster::extract(pse_raster_pop19, pse_adm2, df = TRUE)
#endCluster()
#save(pop_vals_adm2, file = "pop_vals_adm2.RData")

#beginCluster(ncores)
#pop_vals_adm1 <- raster::extract(pse_raster_pop19, pse_adm1, df = TRUE)
#endCluster()
#save(pop_vals_adm1, file = "pop_vals_adm1.RData")


load("pop_vals_adm1.RData")
load("pop_vals_adm2.RData")

totals_adm2 <- pop_vals_adm2 %>%
  group_by(ID) %>%
  summarize(pse_ppp= sum(pse_ppp_2019_UNadj , na.rm = TRUE))

totals_adm1 <- pop_vals_adm1 %>%
  group_by(ID) %>%
  summarize(pse_ppp= sum(pse_ppp_2019_UNadj , na.rm = TRUE))

#sum(totals_adm2$pse_ppp)
pse_adm1 <- pse_adm1 %>%
  add_column(pse_pop = totals_adm1$pse_ppp)

pse_adm2 <- pse_adm2 %>%
  add_column(pse_pop = totals_adm2$pse_ppp)

ggplot(pse_adm1) +
  geom_sf(aes(fill = pse_pop)) +
  geom_sf_text(aes(label = pse_adm1$NAME_1),
               color = "black",
               size = 3) +
  scale_fill_gradient(low = "lightskyblue1" , high = "orangered4")

ggsave("pse_pop19.png")

##Stretch 1
ggplot(pse_adm2) +
  geom_sf(aes(fill = log(pse_pop))) +
  geom_sf_text(aes(label = pse_adm2$NAME_2),
               color = "black",
               size = 3) +
  scale_fill_gradient(low = "lightskyblue1" , high = "orangered4")

ggsave("pse_pop19Stretch1.png")
##Stretch 2
ggplot(pse_adm2) +
  geom_sf(data = pse_adm1, aes(fill = log(pse_pop)),
          size = 2, alpha = 0) +
  geom_sf(data = pse_adm2, aes(fill = log(pse_pop)),
          size = .75, alpha = 1) +
  geom_sf_text(data = pse_adm1, aes(label = pse_adm1$NAME_1),
               color = "black",
               size = 6, nudge_y = .75) +
  geom_sf_text(aes(label = pse_adm2$NAME_2),
               color = "black",
               size = 2) +
  scale_fill_gradient2(low = "blue" , mid = "yellow", high = "red", midpoint = 12.5)

ggsave("pse_pop19Stretch2.png")
