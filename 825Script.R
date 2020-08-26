rm(list=ls(all=TRUE))
#install.packages("zeallot")
#install.packages("psych")
#install.packages("tidyverse", dependencies = TRUE)

#install.packages("sf", dependencies = TRUE)

library(tidyverse)

library(sf)

setwd("~/Fall2020/DATA440/R/Data")

lbr_int  <- read_sf("gadm36_LBR_shp/gadm36_LBR_0.shp")

st_geometry(lbr_int)

ggplot() +
  geom_sf(data = lbr_int,
          size = 1,
          color = "gold",
          fill = "purple",
          alpha =1) +
  geom_sf_text(data = lbr_int,
               aes(label = "Liberia"),
               size = 10,
               color = "pink")

##
rm(list=ls(all=TRUE))
lbr_int  <- read_sf("gadm36_LBR_shp/gadm36_LBR_0.shp")
lbr_adm1  <- read_sf("gadm36_LBR_shp/gadm36_LBR_1.shp")
lbr_adm2 <- read_sf("gadm36_LBR_shp/gadm36_LBR_2.shp")
ggplot() +
  geom_sf(data = lbr_adm2,
          size = .8,
          color = "purple",
          #fill = "color",
          alpha = 0) +
  geom_sf(data = lbr_adm1,
          size = 1,
          color = "gray50",
          fill = "pink1",
          alpha = 0.65) +
  geom_sf(data = lbr_int,
          size = 2,
          color = "black",
          fill = "purple",
          alpha = 0) +
  geom_sf_text(data = lbr_adm2,
               aes(label = NAME_2),
               size = 1) +
  geom_sf_text(data = lbr_adm1,
               aes(label = NAME_1),
               size = 2.5,
               color = "black") +
  geom_sf_label(data = lbr_int,
                aes(label = "Liberia"),
                size = 12,
                nudge_x = .93,
                nudge_y = 1.3,
                color = "black")

ggsave("liberia.png")
#STRETCH 1 
rm(list=ls(all=TRUE))

# install.packages("tidyverse", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)

library(tidyverse)
library(sf)

setwd("~/Fall2020/DATA440/R/Data")

lbr_int  <- add_command_here("gadm36_LBR_shp/add_file_name_here.shp")
lbr_adm1  <- add_command_here("gadm36_LBR_shp/add_file_name_here.shp")
lbr_adm2  <- add_command_here("gadm36_LBR_shp/add_file_name_here.shp")

ggplot() +
  geom_sf(data = adm2_object,
          size = value,
          color = "color",
          fill = "color",
          alpha = value) +
  geom_sf(data = adm1_object,
          size = value,
          color = "gray50",
          alpha = value) +
  geom_sf(data = int_object,
          size = value,
          alpha = value) +
  geom_sf_text(data = adm2_object,
               aes(label = variable_name),
               size = value) +
  geom_sf_text(data = adm1_object,
               aes(label = variable_name),
               size = value)

ggsave("liberia.png")
