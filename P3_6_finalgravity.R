rm(list=ls())

# install.packages("devtools")
# install.packages("gravity", dependencies = TRUE)
#install.packages("gifski", dependencies = TRUE)
#install.packages("png", dependencies = TRUE)
library(gifski)
library(png)
# library(jsonlite)
library(tidyverse)
library(dplyr)
library(sf)
library(rmapshaper)
# library(imputeTS)
library(gganimate)
# library(stplanr)
#devtools::install_github("itsleeds/od")
library(gravity)
library(raster)
library(units)

setwd("~/JMHFall2020/DATA440/R/Togo/Data")
# api <- "https://www.worldpop.org/rest/data/"
# x <- as.data.frame(fromJSON(api))

###################
### Import Data ###
###################


tgo_adm1_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_1.shp")
tgo_adm2_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_2.shp")


tgo1_simp <- ms_simplify(tgo_adm1_sf)

# center points
# cpts <- read_sf("LBR_5yrs_InternalMigFlows_2010/LBR_AdminUnit_Centroids/LBR_AdminUnit_Centroids.shp")
adm1_cpts <- st_centroid(tgo1_simp)

# Import migratory flows
flows <- read_csv("Africa_1km_Internal_Migration_Flows/TGO_5yrs_InternalMigFlows_2010/TGO_5yrs_InternalMigFlows_2010.csv")


# Night time lights
tgo_ntl15 <- raster("worldpop/tgo_viirs_100m_2016.tif")

############################################
### Create Origin-Destination Data Frame ###
############################################

# names of origin and destination counties

adm1_cpts <- adm1_cpts[ ,c(4,11)]
adm1_cpts$NAME_1 <- gsub(" ","_",adm1_cpts$NAME_1)

nms_o <- adm1_cpts$NAME_1
nms_d <- adm1_cpts$NAME_1

names(nms_o) <- "origin_county"
names(nms_d) <- "dest_county"

odm <- expand_grid(nms_o, nms_d)


odm <- odm %>%
  filter(nms_o != nms_d)

odm <- odm %>% 
  rename(origin_county  = nms_o,
         dest_county  = nms_d)

# distances of origin and destination counties

o <- st_geometry(adm1_cpts)
d <- st_geometry(adm1_cpts)

dist <- st_distance(o,d) %>%
  set_units(km) %>%
  as.data.frame()

dist <- dist %>%
  rownames_to_column(var = "origin") %>%
  pivot_longer(names_to = "destination",
               values_to = "distance",
               cols = V1:V5)

dist$distance <- as.numeric(dist$distance)

dist <- dist %>%
  filter(distance > 0)

odm <- odm %>%  
  add_column(distance = dist$distance) %>%
  add_column(migration = flows$PrdMIG)
# add night-time lights

#tgo_ntl_vals1 <- raster::extract(tgo_ntl15, tgo_adm1_sf, df = TRUE)
#save(tgo_ntl_vals1, file = "tgo_ntl.RData")
load("tgo_ntl.RData")

ntl_ttls <- tgo_ntl_vals1 %>%
  group_by(ID) %>%
  summarize_all(sum, na.rm = TRUE)

ntl_o <- ntl_ttls$tgo_viirs_100m_2016
ntl_d <- ntl_ttls$tgo_viirs_100m_2016

od_ntl <- expand_grid(ntl_o, ntl_d)

od_ntl <- od_ntl %>%
  filter(ntl_o != ntl_d)

odm <- odm %>% 
  add_column(ntl = od_ntl$ntl_d)

# add origin and destination centerpoints, union od points as multipoint

cpts_o <- rep(o, each = 15)
cpts_d <- rep(o, 15)

cpts <- cbind.data.frame(cpts_o,cpts_d)

names(cpts) <- c("origin_cpt", "dest_cpt")

cpts <- cpts %>%
  filter(origin_cpt != dest_cpt)


odpts <- st_union(o,d) %>%
  st_as_sf()

odpts <- odpts[st_geometry_type(odpts) == "MULTIPOINT", ]

names(odpts) <- "od_pts"
cpts <- cpts[1:20,]
odm <- odm %>% 
  add_column(origin_cpt = cpts$origin_cpt,
             dest_cpt = cpts$dest_cpt,
             od_pts = odpts$od_pts)





# Summarize origin in/out-migration flows

origin_flows_sums <- flows %>%
  group_by(NODEI) %>%
  summarise(sum(PrdMIG))

names(origin_flows_sums) <- c("county", "outmigration")

# Summarize destination in-migration flows

destination_flows_sums <- flows %>%
  group_by(NODEJ) %>%
  summarise(sum(PrdMIG))

names(destination_flows_sums) <- c("county", "inmigration")

tgo1_simp <- tgo1_simp %>% 
  add_column(outmigration = origin_flows_sums$outmigration,
             inmigration = destination_flows_sums$inmigration)

ggplot() +
  geom_sf(data = tgo1_simp, aes(fill = outmigration))

ggplot() +
  geom_sf(data = tgo1_simp, aes(fill = inmigration))

# create pie chart by adm1 with % origin/destination flows

# create od matrix
odm <- pivot_wider(data = flows, id_cols = NODEI, names_from = NODEJ, values_from = PrdMIG) #instead of spread() or pivot_longer instead of gather()
odm <- odm[ ,-1]
odm <- odm[ ,c(1:5)] #alternatively obj[ ,c(16,2:15)]


#####################################################
### create vector paths to/from all center points ###
#####################################################

#adm1_cpts <- adm1_cpts[ ,c(4,11)]
adm1_cpts$NAME_1 <- gsub(" ","_",adm1_cpts$NAME_1)

o <- st_geometry(adm1_cpts)
d <- st_geometry(adm1_cpts)

# od_combos <- expand_grid(o,d) #st_union accomplishes the same thing

# create for 1 to 2:15
pt <- st_union(o[1], d[2:5])

ln <- st_cast(pt, "LINESTRING") %>%
  st_as_sf()

# create for all

pts_all <- st_union(o, d) %>%
  st_as_sf()

mpts_all <- pts_all[st_geometry_type(pts_all) != "POINT", ]

lns_all <- st_cast(mpts_all, "LINESTRING") %>%
  st_as_sf()


# add migration values

mari_origin <- subset(origin_flows_sums)
ln_x <- add_column(ln, migration = mari_origin$outmigration[1:4])

# produce line plot with lines as weights

ggplot() +
  geom_sf(data = tgo1_simp) +
  geom_sf(data = origin_flows_sums, aes(size = outmigration)) +
  geom_sf(data = lns_all)

#### next annimate

library(lwgeom)

#ln[1,]

p <- st_line_sample(st_transform(lns_all, 32629), 2) %>%
  st_cast("POINT") %>%
  st_as_sf()

p <- lns_all %>%
   st_transform(32629) %>%
   st_startpoint() %>%
   st_endpoint() %>%
   st_cast("POINT") %>%
   st_as_sf()

p$id <- rep(1:10, each = 2)
p$time <- seq(from = 0, to = 1, by = 1)

p <- p %>% st_transform(4251)

p$long <- st_coordinates(p)[,1]
p$lat <- st_coordinates(p)[,2]

ggplot() +
  geom_sf(data = tgo1_simp) +
  geom_sf(data = origin_flows_sums, aes(size = outmigration)) +
  geom_sf(data = adm1_cpts) +
  geom_sf(data = lns_all, alpha = .2) +
  geom_sf(data = p, size = .2)

a <- ggplot() +
  geom_sf(data = tgo1_simp) +
  geom_sf(data = adm1_cpts) +
  geom_sf(data = lns_all, alpha =.1) +
  geom_point(data = p, size = .1, aes(x = long, y = lat))
a
##

anim = a + 
  transition_reveal(along = time)+
  ease_aes('linear')+
  ggtitle("Time: {frame_along}")

gganimate::animate(anim, renderer = gifski_renderer(),nframes = 24, fps = 6)
print(anim)
anim_save("output.gif", animation = anim)

# CDR data

a <- read_sf("/Users/tyfrazier/Desktop/Spatial_Data/Settlements/settspops/setts-pops.shp")

ggplot() +
  geom_sf(data = a)
