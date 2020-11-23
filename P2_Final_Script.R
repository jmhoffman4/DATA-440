############################OCT2
rm(list=ls(all=TRUE))
setwd("~/JMHFall2020/DATA440/R/Togo/Data")
# install.packages("tidyverse", dependencies = TRUE)
#install.packages("sf", dependencies = TRUE)
# install.packages("ggsn", dependencies = TRUE)
library(tidyverse)
library(sf)
library(ggsn)
### Import Administrative Subdivisions ###
tgo_int  <- read_sf("tgo_adm_1m_gaul_20191003_shp/tgo_admbnda_adm0_1m_gaul_20191003.shp")
tgo_adm1_sf  <- read_sf("tgo_adm_1m_gaul_20191003_shp/tgo_admbnda_adm1_1m_gaul_20191003.shp")
tgo_adm2_sf  <- read_sf("tgo_adm_1m_gaul_20191003_shp/tgo_admbnda_adm2_1m_gaul_20191003.shp")

### Import DHS Cluster Locations ###
tgo_dhs_clust  <- read_sf("TG_2013-14_DHS_10192020_1619_153462/TGGE62FL/TGGE62FL.shp")
### Import DHS Cluster Locations ###
hh13_all <- haven::read_dta("TG_2013-14_DHS_10192020_1619_153462/TGHR61DT/TGHR61FL.DTA")

hh13 <- haven::read_dta("TG_2013-14_DHS_10192020_1619_153462/TGHR61DT/TGHR61FL.DTA")[ ,c(3,11)]

hh13[] <- lapply(hh13, function(x){ attributes(x) <- NULL; x })
### Transform Projections ###

tgo_int <- tgo_int %>% st_transform(32629)
tgo_adm1_sf <- tgo_adm1_sf %>% st_transform(32629)
tgo_adm2_sf <- tgo_adm2_sf %>% st_transform(32629)

#lbr_dhs_clust <- lbr_dhs_clust %>% st_transform(4251)
tgo_dhs_clust <- tgo_dhs_clust %>% st_transform(32629)

### Data Modifications ###

tgo_dhs_clust_join <- left_join(tgo_dhs_clust, hh13, by = c("DHSCLUST" = "hv001"))

tgo_dhs_clust_join_rural <- tgo_dhs_clust_join %>%
  filter(URBAN_RURA == "R")

tgo_dhs_clust_join_rural <- rownames_to_column(tgo_dhs_clust_join_rural, var = "id")

rural_10 <- sample_frac(tgo_dhs_clust_join_rural, 0.01, replace = FALSE)
id <- as.numeric(rural_10$id)
rural_5 <- tgo_dhs_clust_join_rural[-id, ]

urban_2 <- tgo_dhs_clust_join %>%
  filter(URBAN_RURA == "U")

### Create Buffers and Sample Points

rural_10_buffs <- distinct(rural_10, .keep_all = TRUE) %>%
  st_buffer(dist = 10000) %>%
  st_intersection(tgo_adm1_sf) %>%
  filter(ADM1FIPSNA == "Maritime Region")

rural_10_pts <- st_sample(rural_10_buffs, table(rural_10$DHSCLUST))

rural_5_buffs <- distinct(rural_5, .keep_all = TRUE) %>%
  st_buffer(dist = 5000) %>%
  st_intersection(tgo_adm1_sf) %>%
  filter(ADM1FIPSNA == "Maritime Region")

rural_5_pts <- st_sample(rural_5_buffs, table(rural_5$DHSCLUST))

urban_2_buffs <- distinct(urban_2, .keep_all = TRUE) %>%
  st_buffer(dist = 2000) %>%
  st_intersection(tgo_adm1_sf) %>%
  filter(ADM1FIPSNA == "Maritime Region" )

urban_2_pts <- st_sample(urban_2_buffs, table(urban_2$DHSCLUST))

rural_10_pts <- st_sf(rural_10_pts)
#st_geometry(rural_10_pts) <- "geometry"


rural_5_pts <- st_sf(rural_5_pts)
#st_geometry(rural_5_pts) <-  "geometry"


rural_5_pts <- st_sf(rural_5_pts)
#st_geometry(rural_5_pts) <- "geometry"


st_geometry(rural_10_pts) <- "points"
st_geometry(rural_5_pts) <- "points"

pts <- rbind(rural_10_pts, rural_5_pts)

plot(st_geometry(pts), cex = .25)

#plot(st_geometry(rural_10), add = TRUE, cex = .25)

plot(st_geometry(tgo_adm1_sf), add = TRUE)

plot(st_geometry(rural_10_pts), add = TRUE, cex = .25)
plot(st_geometry(rural_5_pts), add = TRUE, cex = .25)
plot(st_geometry(urban_2_pts), add = TRUE, cex = .25)

plot(st_geometry(rural_10_buffs), add = TRUE)
plot(st_geometry(rural_5_buffs), add = TRUE)
plot(st_geometry(urban_2_buffs), add = TRUE)
######################################################OCT5
rm(list=ls(all=TRUE))

setwd("~/JMHFall2020/DATA440/R/Togo/Data")

#install.packages("foreign", dependencies = TRUE)
#install.packages("reshape", dependencies = TRUE)
#install.packages("gdata", dependencies = TRUE)
#install.packages("VIM", dependencies = TRUE)

#install.packages("raster", dependencies = TRUE)
#install.packages("sf", dependencies = TRUE)
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("maptools", dependencies = TRUE)
#install.packages("spatstat", dependencies = TRUE)
#install.packages("units", dependencies = TRUE)

library(foreign)
library(reshape)
library(gdata)
library(VIM)

library(tidyverse)
library(haven)

library(raster)
library(sf)
library(tidyverse)
library(maptools)
library(spatstat)
library(units)

persons <- read_dta("TG_2013-14_DHS_10192020_1619_153462/TGIR61DT/TGIR61FL.DTA")
households <- read_dta("TG_2013-14_DHS_10192020_1619_153462/TGHR61DT/TGHR61FL.DTA")

weights <- households$hv005
size <- households$hv009
sex <- households[ ,368:413]
age <- households[ ,414:459]

hhs <- cbind.data.frame(weights, size, sex, age)

# What is the population of your selected country
# How many households in your selected country
# What is the population of your selected subdivision
# How many households in your selected subdivision

Maritime <- subset(households, hv024 == 3)
sum(households$hv009)
sum(Maritime$hv009)
table(Maritime$hv009) #maybe we expand the sample


########################
### set our location ###
########################

tgo_raster_pop19 <- raster("worldpop/tgo_ppp_2020_constrained.tif")

tgo_int <- read_sf("gadm36_TGO_shp/gadm36_TGO_0.shp")

tgo_adm1_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_1.shp")
tgo_adm2_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_2.shp")

zio <- tgo_adm2_sf %>%
  filter(NAME_2 == "Zio")

zio_pop19 <- crop(tgo_raster_pop19, zio)
zio_pop19 <- mask(zio_pop19, zio)

pop <- floor(cellStats(zio_pop19, 'sum'))

pop <- pop/100
houses <- ceiling(pop / 4.6)

png("zio_pop19.png", width = 800, height = 800)
plot(zio_pop19, main = NULL)
plot(st_geometry(zio), add = TRUE)
dev.off()

st_write(tgo_int, "tgo.shp", delete_dsn=TRUE)
st_write(zio, "zio.shp", delete_dsn=TRUE)

tgo_mt <- readShapeSpatial("tgo.shp")
zio_mt <- readShapeSpatial("zio.shp")

#win <- as(tgo_mt, "owin")
win <- as(zio_mt, "owin")

tgo_houses <- rpoint(houses, f = as.im(tgo_raster_pop19), win = win)
zio_houses <-rpoint(houses, f = as.im(zio_pop19), win = win)

#png("zio_pipo.png", width = 2000, height = 2000)
#plot(win, main = NULL)
#plot(zio_houses, cex = .15)
#dev.off()
###############################OCT7

#png("tgo_hhs.png", width = 500, height = 500)
#plot(win, main = NULL)
#plot(tgo_houses, cex = .15, add = TRUE)
#dev.off()

#png("zio_hhs.png", width = 500, height = 500)
#plot(win, main = NULL)
#plot(zio_houses, cex = .15, add = TRUE)
#dev.off()

#coordinates(zio_houses)
sf_obj_tgo <- sf::st_as_sf(tgo_houses)
sf_obj_tgo <- sf_obj_tgo[-1,]

st_write(sf_obj_tgo, "sf_obj_tgo.shp")
sf_obj_tgo  <- read_sf("sf_obj_tgo.shp")


hh_pts <- sf_obj %>% st_set_crs(st_crs(tgo))


sf_obj <- sf::st_as_sf(zio_houses)
sf_obj <- sf_obj[-1,]

st_write(sf_obj, "sf_obj.shp")
sf_obj  <- read_sf("sf_obj.shp")

hh_pts <- sf_obj %>% st_set_crs(st_crs(zio))


ggplot() +
  geom_sf(data = zio) +
  geom_sf(data = hh_pts,
          size = .1,
          alpha = .5)

png("zio_hhs.png", width = 2000, height = 2000)
#plot(win, main = NULL)
#plot(zio_houses, cex = .15)
#dev.off()
######################OCT 9########3
rm(list=ls(all=TRUE))
setwd("~/JMHFall2020/DATA440/R/Togo/Data")

# install.packages("foreign", dependencies = TRUE)
# install.packages("reshape", dependencies = TRUE)
# install.packages("gdata", dependencies = TRUE)
# install.packages("VIM", dependencies = TRUE)
# install.packages("expss", dependencies = TRUE)
library(expss)
library(foreign)
library(reshape)
library(gdata)
library(VIM)
library(tidyverse)
library(haven)
library(VIM)
library(raster)
library(sf)
library(maptools)
library(spatstat)
library(units)

persons <- read_dta("TG_2013-14_DHS_10192020_1619_153462/TGIR61DT/TGIR61FL.DTA")
households <- read_dta("TG_2013-14_DHS_10192020_1619_153462/TGHR61DT/TGHR61FL.DTA")

unit <- households$hv004
weights <- households$hv005
location <- as_factor(households$shregion)
size <- households$hv009
sex <- households[ ,368:413]
age <- households[ ,414:459]

#regions <- c("Centre", "Kara", "Maritime", "Plateaux", "Savanes", "n/a")
#location <- factor(location, levels = 1:6, labels = regions)
hhs <- cbind.data.frame(unit, weights, location, size, sex, age)

sum(hhs$weights)

maritime <- subset(households, shregion == 3)
sum(households$hv005)
sum(maritime$hv005)
table(maritime$hv009) #maybe we expand the sample

miss <- aggr(hhs)

table(hhs$size)


a <- aggr(hhs[ ,1:5])
plot(a, numbers = TRUE, prop = TRUE)

########################
### set our location ###
########################

tgo_raster_pop19 <- raster("worldpop/tgo_ppp_2020_constrained.tif")

tgo_int <- read_sf("gadm36_TGO_shp/gadm36_TGO_0.shp")

tgo_adm1_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_1.shp")
tgo_adm2_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_2.shp")

zio <- tgo_adm2_sf %>%
  filter(NAME_2 == "Zio")

zio_pop19 <- crop(tgo_raster_pop19, zio)
zio_pop19 <- mask(zio_pop19, zio)

pop <- floor(cellStats(zio_pop19, 'sum'))
pop_tgo <- floor(cellStats(tgo_raster_pop19, 'sum'))


houses_zio <- ceiling(pop /4.6)
houses_tgo <- nrow(hhs)


# distribute at adm0

#st_write(tgo_int, "tgo0.shp", delete_dsn=TRUE)
tgo0_mt <- readShapeSpatial("tgo0.shp")
win <- as(tgo0_mt, "owin")

houselocs_tgo <- rpoint(houses_tgo, f = as.im(tgo_raster_pop19), win = win)

# plot using baseR
#plot(st_geometry(tgo_int))
#plot(houselocs_tgo, add = TRUE, cex = .1)
#png("tgo_house_plot.png", width = 2000, height = 2000)
#dev.off()
# using ggplot
pts <- cbind.data.frame(x = houselocs_tgo$x, y = houselocs_tgo$y)

hh_pts = st_as_sf(pts, coords = c("x", "y"), 
                  crs = st_crs(tgo_int))

ggplot() +
  geom_sf(data = tgo_int) +
  geom_sf(data = hh_pts,
          size = .1,
          alpha = .5)
#ggsave(filename = "tgo_households_rpoint.png")
dev.off()
# distribute at adm1

st_write(tgo_adm1_sf, "tgo1.shp", delete_dsn=TRUE)
tgo1_mt <- readShapeSpatial("tgo1.shp")
win <- as(tgo1_mt, "owin")

table(hhs$location)

# works
tgo1_dhslocs <- st_sample(tgo_adm1_sf, table(hhs$location))
# doesn't work
# lbr1_dhslocs <- st_sample(lbr_adm1, table(hhs$location), type = as.im(lbr_pop15))

ggplot() +
  geom_sf(data = tgo_adm1_sf) +
  geom_sf(data = tgo1_dhslocs,
          size = .1,
          alpha = .5)
#ggsave(filename = "tgo_households_adm1.png")

# using spatstat

setwd("~/JMHFall2020/DATA440/R/Togo/Data")

x <- 1
while(x < 7){
  st_write(tgo_adm1_sf[x, ], paste(x,".shp",sep =""), delete_dsn=TRUE)
  x <- x + 1
}

files <- list.files(pattern="*.shp$")

counties <- letters[1:6]
###############################################
x <- 1
while(x < 7){
  assign(counties[x], readShapeSpatial(files[x]))
  x <- x + 1
}

winA <- as(a, "owin")  

plot(tgo_raster_pop19)
plot(winA, add = TRUE)

Apts <- rpoint(table(hhs$location)[1], f = as.im(zio_pop19), win = winA)

Acoords <- cbind.data.frame(x = Apts$x, y = Apts$y)

sfA = st_as_sf(Acoords, coords = c("x", "y"), 
               crs = st_crs(tgo_adm1_sf))

ggplot() +
  geom_sf(data = tgo_adm1_sf[1]) +
  geom_sf(data = sfA,
          size = .1,
          alpha = .5)

# join DHS data

hhs_maritime <- hhs[which(location == "maritime (sans lomé)"), ]


sfA$id <- 1:nrow(sfA)
hhs_maritime$id <- 1:nrow(hhs_maritime)

maritime_joined <- left_join(sfA, hhs_maritime, by = c("id" = "id"))

ggplot() +
  geom_sf(data = tgo_adm1_sf) +
  geom_sf(data = maritime_joined,
          size = .1,
          alpha = .5)


####################################
### Spatial polygon calculations ###
####################################

maritime <- tgo_adm1_sf %>%
  filter(NAME_1 == "Maritime")
###########################
### Raster calculations ###
###########################

maritime_pop19 <- crop(tgo_raster_pop19, maritime)
maritime_pop19 <- mask(maritime_pop19, maritime)
# confirm projection
plot(maritime_pop19)
plot(st_geometry(maritime), add = TRUE)

##########################
### distribute at adm0 ###
##########################

# calculate average household size

tgo_hhs_n <- floor(cellStats(tgo_raster_pop19, 'sum') / mean(hhs$size))


tgo0_mt <- readShapeSpatial("tgo0.shp")
win <- as(tgo0_mt, "owin")

hhs_pts <- rpoint(tgo_hhs_n, f = as.im(tgo_raster_pop19), win = win)

pts <- cbind.data.frame(x = hhs_pts$x, y = hhs_pts$y)

hhs_locs = st_as_sf(pts, coords = c("x", "y"),
                    crs = st_crs(tgo_int))

# generate households

hhs_samp <- slice_sample(hhs, n = tgo_hhs_n, replace = TRUE)[ ,c(2,4:96)]

hhs_locs$id <- 1:nrow(hhs_locs)
hhs_samp$id <- 1:nrow(hhs_samp)

togo_joined <- left_join(hhs_locs, hhs_samp, by = c("id" = "id"))

# calculate weighted error
abs((nrow(togo_joined) - sum(togo_joined$weights)) / nrow(togo_joined))

#  plot

plot <- ggplot() +
  geom_sf(data = tgo_int) +
  geom_sf(data = togo_joined,
          size = .01,
          alpha = .05)

#ggsave("togo_joined.png", plot, width = 10, height = 10, dpi = 300)

###################################
### distribute at selected adm1 ###
###################################

# calculate average household size
#4.6?
# bomi_hhs_n <- floor(cellStats(bomi_pop15, 'sum') / mean(hhs$size))

maritime_hhs_n <- floor(cellStats(maritime_pop19, 'sum') / mean(subset(hhs, location == "maritime (sans lomé)")$size))

st_write(maritime, "maritime.shp", delete_dsn=TRUE)
maritime_mt <- readShapeSpatial("maritime.shp")
win <- as(maritime_mt, "owin")

hhs_adm1_pts <- rpoint(maritime_hhs_n, f = as.im(maritime_pop19), win = win)

adm1_pts <- cbind.data.frame(x = hhs_adm1_pts$x, y = hhs_adm1_pts$y)

adm1_locs = st_as_sf(adm1_pts, coords = c("x", "y"), 
                     crs = st_crs(tgo_int))

# analyze the data
maritime_analyze <- subset(hhs, location == "maritime (sans lomé)")
sum(maritime_analyze$weights)

nrow(maritime_analyze)
table(hhs$size)
table(maritime_analyze$size)

ggplot() +
  geom_density(data = hhs,
               aes(x = size), fill = "gold")  +
  geom_density(data = hhs[which(location == "maritime (sans lomé)"), ],
               aes(x = size), colour = "green")

adm1_sampP <- slice_sample(hhs, n = maritime_hhs_n, replace = TRUE)
adm1_sampP1 <- slice_sample(hhs[which(location == "maritime (sans lomé)"), ], n = maritime_hhs_n, replace = TRUE)

ggplot() +
  geom_density(data = adm1_sampP,
               aes(x = size), fill = "gold")  +
  geom_density(data = adm1_sampP1,
               aes(x = size), colour = "green") 

# generate households

maritime_samp <- slice_sample(hhs, n = maritime_hhs_n, replace = TRUE)[ ,c(2,4:96)]

adm1_locs$id <- 1:nrow(adm1_locs)
maritime_samp$id <- 1:nrow(maritime_samp)

maritime_joined <- left_join(adm1_locs, maritime_samp, by = c("id" = "id"))
# calculate weighted error
abs((nrow(maritime_joined) - sum(maritime_joined$weights)) / nrow(maritime_joined))

#  plot

plot <- ggplot() +
  geom_sf(data = maritime) +
  geom_sf(data = maritime_joined,
          size = .01,
          alpha = .05)

#ggsave("maritime.png", plot, width = 10, height = 10, dpi = 300)

# disaggregate to persons
#########################################
rm(list=ls(all=TRUE))

setwd("~/JMHFall2020/DATA440/R/Togo/Data")

# install.packages("raster", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("maptools", dependencies = TRUE)
# install.packages("spatstat", dependencies = TRUE)
# install.packages("units", dependencies = TRUE)
# install.packages("VIM", dependencies = TRUE)

library(tidyverse)
library(haven)
library(sf)
library(raster)
library(spatstat)
library(maptools)
library(VIM)
library(units)

########################################
### Import and modify household data ###
########################################

#persons <- read_dta("TG_2013-14_DHS_10192020_1619_153462/TGIR61DT/TGIR61FL.DTA")
households <- read_dta("TG_2013-14_DHS_10192020_1619_153462/TGHR61DT/TGHR61FL.DTA")

hhid <- households$hhid #check length(unique(hhid))
unit <- households$hv004
weights <- households$hv005
location <- as_factor(households$shregion)
size <- households$hv009
sex <- households[ ,368:413]
age <- households[ ,414:459]
edu <- households[ ,460:505]
wealth <- households$hv270
hhs <- cbind.data.frame(hhid, unit, weights, location, size, sex, age, edu, wealth)

# script to simplify observations as needed
# table(hhs$location)
# hhs$new <- gsub("Bomi|Montserrado", "Greater Monrovia", hhs$location)
# table(hhs$new)

##########################################
### Import and subset spatial polygons ###
##########################################

tgo_raster_pop19 <- raster("worldpop/tgo_ppp_2020_constrained.tif")

tgo_int <- read_sf("gadm36_TGO_shp/gadm36_TGO_0.shp")

tgo_adm1_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_1.shp")
tgo_adm2_sf  <- read_sf("gadm36_TGO_shp/gadm36_TGO_2.shp")

maritime <- tgo_adm2_sf %>%
  filter(NAME_1 == "Maritime")

maritime_pop19 <- crop(tgo_raster_pop19, maritime)
maritime_pop19 <- mask(maritime_pop19, maritime)

pop <- floor(cellStats(maritime_pop19, 'sum'))
pop_tgo <- floor(cellStats(tgo_raster_pop19, 'sum'))


# confirm projection

plot(maritime_pop19)
plot(st_geometry(maritime), add = TRUE)

###############################
### Expand survey data from ###
###  households to persons  ###
###############################
col <- c(1,2,3,4,5,99,100)
gender_pivot <- hhs %>% 
  gather(key = "pnmbr", value = "gender", colnames(hhs)[6:51], na.rm = TRUE) # revise after adding new variable
gender_pivot <- gender_pivot[ 1:46505,col]

age_pivot <- hhs %>%
  gather(key = "pnmbr", value = "age", colnames(hhs)[52:97], na.rm = TRUE)
age_pivot <- age_pivot[1:46505 ,col]

edu_pivot <- hhs %>%
  gather(key = "pnmbr", value = "edu", colnames(hhs)[98:143], na.rm = TRUE)
edu_pivot <- edu_pivot[ 1:46505,col]

pns <- cbind.data.frame(gender_pivot, age = age_pivot$age, education = edu_pivot$edu)

# check household level error
sum(hhs$weights)
nrow(hhs)

# check person level error
sum(pns$weights) 
nrow(pns)
nrow(pns) / cellStats(tgo_raster_pop19, 'sum') # person sample proportion

# Does not appear to have accurately incorporated person-level weights (household size) as part of 
# household survey design (secondary sampling unit).  Ideally, sum of person-level weights should equal 
# number of person level observations (rows)

pns_numeric <- pns
pns_numeric$location <- as.numeric(pns_numeric$location)
write.csv(pns_numeric[ ,c(4:9)], file = "pns.csv")

############################
### Spatially locate all ###
###  households at adm0  ###
############################

# calculate average household size

tgo_hhs_n <- floor(cellStats(tgo_raster_pop19, 'sum') / mean(hhs$size))

st_write(tgo_int, "tgo0.shp", delete_dsn=TRUE)
tgo0_mt <- readShapeSpatial("tgo0.shp")
win <- as(tgo0_mt, "owin")

hhs_pts <- rpoint(tgo_hhs_n, f = as.im(tgo_raster_pop19), win = win) # randomly generate points
# ideal method is to use DHS coordinates with point process model

pts <- cbind.data.frame(x = hhs_pts$x, y = hhs_pts$y)

hhs_locs = st_as_sf(pts, coords = c("x", "y"), crs = st_crs(tgo_int))

# random sample from generate households

hhs_pop <- slice_sample(hhs, n = tgo_hhs_n, replace = TRUE) # randomly expand households from survey to population 
# keep all columns, check weight_by argument

sum(hhs_pop$weights) #check error
nrow(hhs_pop) # check n rows
nrow(hhs_locs) # confirm

tgo_hhs_locs <- cbind.data.frame(hhs_pop, hhs_locs)

# calculate weighted error
abs((nrow(tgo_hhs_locs) - sum(tgo_hhs_locs$weights)) / nrow(tgo_hhs_locs))

####################################
### Expand households to persons ###
###      with adm0 locations     ###
####################################

#gender_pivot <- tgo_hhs_locs %>% 
# gather(key = "pnmbr", value = "gender", colnames(hhs)[6:58], na.rm = TRUE) # revise after adding new variable
#gender_pivot <- gender_pivot[ ,-6:-111]

#age_pivot <- lbr_hhs_locs %>%
#  gather(key = "pnmbr", value = "age", colnames(hhs)[59:111], na.rm = TRUE)
#age_pivot <- age_pivot[ ,-6:-111]

#edu_pivot <- lbr_hhs_locs %>%
#  gather(key = "pnmbr", value = "edu", colnames(hhs)[112:164], na.rm = TRUE)
#edu_pivot <- edu_pivot[ ,-6:-111]

tgo_pns <- cbind.data.frame(gender_pivot,age = age_pivot$age) #sort variables for use/validation, remove wealth

sum(tgo_hhs_locs$weights)
nrow(tgo_hhs_locs)

sum(tgo_pns$weights) 
nrow(tgo_pns)
nrow(tgo_pns) / cellStats(tgo_raster_pop19, 'sum') # compare DHS-based, generated synthetic person total proportion to ML/EO output


hhs_samp <- slice_sample(hhs, n = tgo_hhs_n, replace = TRUE)[ ,c(2,4:96)]

hhs_locs$id <- 1:nrow(hhs_locs)
hhs_samp$id <- 1:nrow(hhs_samp)

togo_joined <- left_join(hhs_locs, hhs_samp, by = c("id" = "id"))

### plot ###

plot <- ggplot() +
  geom_sf(data = tgo_int) +
  geom_sf(data = togo_joined,
          size = .01,
          alpha = .05)

#ggsave("togo_2.png", plot, width = 10, height = 10, dpi = 300)

############################
### Spatially locate all ###
###  households at adm1  ###
############################

maritime_hhs <- subset(hhs, location == "maritime (sans lomé)")


# calculate average household size

maritime_hhs_n <- floor(cellStats(maritime_pop19, 'sum') / mean(maritime_hhs$size))

st_write(maritime, "maritime.shp", delete_dsn=TRUE)
maritime_mt <- readShapeSpatial("maritime.shp")
win <- as(maritime_mt, "owin")

hhs_adm1_pts <- rpoint(maritime_hhs_n, f = as.im(maritime_pop19), win = win)

adm1_pts <- cbind.data.frame(x = hhs_adm1_pts$x, y = hhs_adm1_pts$y)

adm1_locs = st_as_sf(adm1_pts, coords = c("x", "y"), crs = st_crs(tgo_adm1_sf))

# analyze the data
sum(maritime_hhs$weights)
nrow(maritime_hhs)

table(hhs$size)
table(maritime_hhs$size)

ggplot() +
  geom_density(data = hhs,
               aes(x = size), fill = "gold")  +
  geom_density(data = maritime_hhs,
               aes(x = size), colour = "green")

adm1_sampP <- slice_sample(hhs, n = maritime_hhs_n, replace = TRUE)
adm1_sampP1 <- slice_sample(maritime_hhs, n = maritime_hhs_n, replace = TRUE)

ggplot() +
  geom_density(data = adm1_sampP,
               aes(x = size), fill = "gold")  +
  geom_density(data = adm1_sampP1,
               aes(x = size), colour = "green") 

# generate households

maritime_hhs_pop <- slice_sample(hhs, n = maritime_hhs_n, replace = TRUE)

sum(maritime_hhs_pop$weights) #check error
nrow(maritime_hhs_pop) # check n rows
nrow(adm1_locs) # confirm

maritime_hhs_locs <- cbind.data.frame(maritime_hhs_pop, adm1_locs)

# calculate weighted error
abs((nrow(maritime_hhs_locs) - sum(maritime_hhs_locs$weights)) / nrow(maritime_hhs_locs))



x <- maritime_joined %>% 
  gather(key = "pnmbr", value = "gender", colnames(maritime_joined)[4:56], na.rm = TRUE)

x <- x[ -4:-56]

y <- maritime_joined %>%
  gather(key = "pnmbr", value = "age", colnames(maritime_joined)[57:109], na.rm = TRUE)

y <- y[ ,-4:-56]

z <- cbind.data.frame(x,y)

a <- floor(cellStats(maritime_pop19, 'sum'))
b <- nrow(z)

(a - b) / a

length(unique(z$id))
# multinomial to predict


# validate
