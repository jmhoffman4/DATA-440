rm(list=ls(all=TRUE))
setwd("~/Fall2020/DATA440/R/Palestine/Data")

library(ggpubr)
library(ggmap)
library(raster)
library(sf)
library(tidyverse)
library(rgdal)
library(maptools)
library(spatstat)
library(units)
library(rayshader)
library(rayrender)
library(doParallel)
library(snow)
library(scales)

pse_int  <- read_sf("gadm36_PSE_shp/gadm36_PSE_0.shp")

pse_adm1  <- read_sf("gadm36_PSE_shp/gadm36_PSE_1.shp")

pse_pop19 <- raster("world_pop/pse_ppp_2019.tif")

pse_adm2  <- read_sf("gadm36_pse_shp/gadm36_PSE_2.shp")

pse_topo <- raster("lulc/pse_srtm_topo_100m.tif")

pse_roads  <- read_sf("HDX/pse_trs_roads_osm.shp")

jericho <- pse_adm2 %>%
  filter(NAME_2 == "Jericho")
jerusalem <- pse_adm2 %>%
  filter(NAME_2 == "Jerusalem")

combined_adm2s <- rbind(jericho, jerusalem)

jer_roads <- st_crop(pse_roads, combined_adm2s)

#plot(st_geometry(combined_adm2s))

jer_pop19 <- crop(pse_pop19, combined_adm2s)
jer_pop19 <- mask(jer_pop19, combined_adm2s)


pop <- floor(cellStats(jer_pop19, 'sum'))

#1a Geometric Bar Plots#


pse_raster_pop19 <- raster("world_pop/pse_ppp_2019_UNadj.tif")

plot(pse_raster_pop19)
plot(st_geometry(combined_adm2s), add = TRUE)

#detectCores()
#ncores <- 3
#beginCluster(ncores)
#pop_vals_adm2 <- raster::extract(pse_raster_pop19, combined_adm2s, df = TRUE)
#endCluster()
#save(pop_vals_adm2, file = "pop_vals_adm2.RData")

load("pop_vals_adm2.RData")

totals_adm2 <- pop_vals_adm2 %>%
  group_by(ID) %>%
  summarize(pse_ppp= sum(pse_ppp_2019_UNadj , na.rm = TRUE))

sum(totals_adm2$pse_ppp)

combined_adm2s <- combined_adm2s %>%
  add_column(pse_pop = totals_adm2$pse_ppp)


combined_adm2s <- combined_adm2s %>%
  mutate(area = sf::st_area(combined_adm2s) %>% 
           units::set_units(km^2)) %>%
  mutate(density = pse_pop /area )



combined_adm2s %>%
  ggplot(aes(x=NAME_2, y=pse_pop)) +
  geom_bar(stat="identity", color="pink", fill = "purple", width=1) +
  coord_flip() +
  xlab("NAME") + ylab("POP")

combined_adm2s %>%
  mutate(NAME_2 = fct_reorder(NAME_2, pse_pop)) %>%
  ggplot(aes(x=NAME_2, y=pse_pop)) +
  geom_bar(stat="identity", color="purple", fill = 'lavender' , width=.5) +
  coord_flip() +
  xlab("NAMES") + ylab("POP") +
  geom_text(aes(label= scales::percent(pse_pop/sum(pse_pop))),
            position = position_stack(vjust = 0.5),
            color = "black", size=2.0)


pse_bplt <- combined_adm2s %>%
  mutate(NAME_1 = fct_reorder(NAME_2, pse_pop)) %>%
  ggplot(aes(x=NAME_2, y=pse_pop, fill = pse_pop)) +
  geom_bar(stat="identity", color="purple", width=.5) +
  coord_flip() +
  xlab("Administrative Areas") + ylab("POP") +
  geom_text(aes(label=scales::percent(pse_pop/sum(pse_pop))),
            position = position_stack(vjust = 0.5),
            color = "black", size=2.0) +
  scale_fill_gradient(low = "lightskyblue3", high = "hotpink") +
  ggtitle("Population & share of Population (in %)")

pse_bplt

pse_spatial <- ggplot(combined_adm2s) +
  geom_sf(aes(fill = pse_pop)) +
  geom_sf_text(aes(label = NAME_2),
               color = "black",
               size = 3) +
  geom_sf_text(aes(label=round(density, 2)),
               color="black", size=2, nudge_y = -.1) +
  scale_fill_gradient(low = "lightskyblue1" , high = "hotpink") +
  ggtitle("Population & Density (in persons/km^2")
jer_jer <- ggarrange(pse_spatial, pse_bplt, nrow = 1, widths = c(2.25,2))
jer_jer <- annotate_figure(jer_jer, top = text_grob("Jericho & Jerusalem, Palestine", color = "black", face = "bold", size = 12))
jer_jer

#ggsave("jer_jer.png", width = 20, height = 10, dpi = 200)
###########################1b De FActo Plot ##################################3



st_write(combined_adm2s, "jer.shp", delete_dsn=TRUE)
jer_with_mtools <- readShapeSpatial("jer.shp")

win <- as(jer_with_mtools, "owin")
plot(win)

jer_ppp <- rpoint(pop, f = as.im(jer_pop19), win = win)


#png("jer_1.png", width = 2000, height = 2000)
#plot(win, main = NULL) 
#plot(jer_ppp, cex = 0.15, add = TRUE)
#dev.off()

#bw_1 <- bw.ppl(raf_ppp)
#save(bw_1, file = "bw_1.RData") 

load("bw_1.RData")  



subdiv_dense <- density.ppp(jer_ppp,sigma = bw_1)


image(subdiv_dense)
Dsg <- as(subdiv_dense, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
Dcl <- contourLines(Dim, levels = 1000000)  # create contour object
SLDF <- ContourLines2SLDF(Dcl, CRS("+proj=longlat +datum=WGS84 +no_defs"))

sf_multiline_obj <- st_as_sf(SLDF, sf)
inside_polys <- st_polygonize(sf_multiline_obj)
combined_polys <- st_polygonize(sf_multiline_obj)
outside_lines <- st_difference(sf_multiline_obj, inside_polys)

z <- st_buffer(outside_lines, 0.001)
zz <- st_difference(combined_adm2s, z)
zzz <- st_cast(zz, "POLYGON")

zzz$area <- as.numeric(st_area(zzz))

subpolys <- zzz #%>%
  #filter(area < 40000000)


subpolys_extract <- raster::extract(jer_pop19, subpolys, df = TRUE)

subpolys_totals <- subpolys_extract %>%
  group_by(ID) %>%
  summarize(pop19 = sum(pse_ppp_2019, na.rm = TRUE))

subpolys <- subpolys %>%
  add_column(pop19 = subpolys_totals$pop19)

png("subpolys.png", width = 1200, height = 1200)
plot(subdiv_dense, main = NULL)
plot(st_geometry(subpolys), add = TRUE)
dev.off()

subpolys_filtered <- subpolys #%>%
  #filter(pop19 > 25)

png("subpolys_filtered.png", width = 1200, height = 1200)
plot(subdiv_dense, main = NULL)
plot(st_geometry(subpolys_filtered), add = TRUE)
dev.off()

inside_polys <- st_collection_extract(inside_polys, "POLYGON")

ips_extract <- raster::extract(jer_pop19, inside_polys, df = TRUE)

ips_totals <- ips_extract %>%
  group_by(ID) %>%
  summarize(pop19 = sum(pse_ppp_2019, na.rm = TRUE))

inside_polys <- inside_polys %>%
  add_column(pop19 = ips_totals$pop19)

inside_polys_filtered <- inside_polys %>%
  filter(pop19 > 10)

uas <- st_union(inside_polys_filtered, subpolys_filtered)

urban_areas <- st_cast(uas, "POLYGON")

urban_areas[ ,1:19] <- NULL

uas_extract <- raster::extract(jer_pop19, urban_areas, df = TRUE)

uas_totals <- uas_extract %>%
  group_by(ID) %>%
  summarize(pop19 = sum(pse_ppp_2019, na.rm = TRUE))

urban_areas <- urban_areas %>%
  add_column(pop19 = uas_totals$pop19)
urban_areas

urban_areas <- urban_areas %>% unique()
ggplot() +
  geom_sf(data = combined_adm2s,
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.15) +
  geom_sf(data = urban_areas,
          fill = "lightblue",
          size = 0.45,
          alpha = 0.5)
#ggsave("urban_areas.png")

urban_areas_2 <- urban_areas %>%
  mutate(area = st_area(urban_areas) %>%
           set_units(km^2)) %>%
  mutate(density = as.numeric(pop19 / area))

ua_cntr_pts <-  urban_areas_2 %>% 
  st_centroid() %>% 
  st_cast("MULTIPOINT")

combined_topo <- crop(pse_topo, combined_adm2s)


class(combined_adm2s)
combined_matrix <- raster_to_matrix(combined_topo)

combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix)) %>%
  plot_map()

ambientshadows <- ambient_shade(combined_matrix)

table(jer_roads$fclass)

primary <- jer_roads %>%
  filter(fclass ==  1)

secondary <- jer_roads %>%
  filter(fclass == 2)

tertiary <- jer_roads %>%
  filter(fclass == 3)

pse_hcf <- read_sf("HDX/hotosm_pse_health_facilities_points.shp")
table(pse_hcf$amenity)

pse_hcf <- st_crop(pse_hcf, combined_adm2s)
hospitals <- pse_hcf %>%
  filter(amenity == "hospital")

clinics <- pse_hcf %>%
  filter(amenity == "clinic")

other_hcfs <- pse_hcf %>%
  filter(amenity == "doctors" | amenity == "dentist" | amenity == "pharmacy")

ggplot() +
  geom_sf(data = combined_adm2s,
          size = 5.0,
          linetype = "11",
          color = "gold",
          alpha = 0) +
  geom_sf(data = combined_polys,
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.5) +
  geom_sf(data = urban_areas_2,
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.15) +
  geom_sf(data = primary,
          size = .5,
          color = "blue") +
  geom_sf(data = secondary,
          size = .35,
          color = "purple") +
  geom_sf(data = tertiary,
          size = .2,
          color = "lightblue") +
  geom_sf(data = hospitals,
          size = 4,
          color = "red") +
  geom_sf(data = clinics,
          size = 2,
          color = "orange")+
  geom_sf(data = other_hcfs,
          size = 1,
          color = "pink") +
  geom_sf(data = ua_cntr_pts,
          aes(size = pop19,
              color = density),
          show.legend = "point") +
  scale_color_gradient(low = "yellow", high = "red")+
  theme_void() + theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y=NULL, title=NULL)

png("road_hcf.png", width = 1200, height = 1200, bg = "transparent")  

ggplot() +
  geom_sf(data = combined_adm2s, 
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.15) +
  geom_sf(data = urban_areas_2,
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.15) +
  geom_sf(data = primary,
          size = .6,
          color = "navyblue") +
  geom_sf(data = secondary,
          size = .45,
          color = "purple") +
  geom_sf(data = tertiary,
          size = .3,
          color = "blue") +
  geom_sf(data = hospitals,
          size = 2,
          color = "red") +
  geom_sf(data = clinics,
          size = 1,
          color = "orange")+
  geom_sf(data = other_hcfs,
          size = .5,
          color = "pink") +
  geom_sf(data = ua_cntr_pts,
          aes(size = pop19,
              color = density),
          show.legend = "point") +
  scale_color_gradient(low = "lightskyblue3", high = "hotpink") +
  ggtitle("Access to Health Care Services throughout Jericho & Jerusalem, Palestine") + 
  xlab("longitude") + ylab("latitude")

ggsave("defactofinal.png")  
##################TOPOGRAPHY Stretch, INCOMPLETE

overlay_img <- png::readPNG("road_hcf.png")
dev.off()

combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix), color = "lightblue") %>%
  add_shadow(ray_shade(combined_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(combined_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  add_overlay(overlay_img, alphalayer = 0.95) %>%
  plot_3d(combined_matrix, zscale = 20,windowsize = c(1000,1000), 
          phi = 40, theta = 135, zoom = 0.55, 
          background = "grey30", shadowcolor = "grey5", 
          soliddepth = -50, shadowdepth = -100)
render_label(combined_matrix, "Ganta", textcolor ="white", linecolor = "white", 
             x = 250, y = 575, z = 1000, textsize = 2.5, linewidth = 4, zscale = 10)
render_snapshot(title_text = "Jericho & Jerusalem, Palestinian Territory", 
                title_size = 50,
                title_color = "grey90")

#rgl::rgl.clear()

