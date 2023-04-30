### yasmine hentati
### study area maps


# load packages
library(mapview)
library(lme4)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)
library(sf)
library(viridis)
library(here)
library(tidycensus)
# remotes::install_github("walkerke/crsuggest") 
library(crsuggest)
library(tidyr)
library(terra)
# install.packages("spatialEco")
library(spatialEco)
library(readr)
library(ggfortify)
library(rgeos)
library(usmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
theme_set(theme_bw())
library(maps)

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

#### load in camera locations

all_sites <- read.csv(here("data", "counts_cleaned.csv"), stringsAsFactors = FALSE)

# create new column for utm zone 
data_10_WA <- all_sites %>% subset(City == "tawa") # add seattle later 
data_10_WA$utmZone <- "10"


# transform into UTM - WA
utm1 <- data.frame(x=data_10_WA$Long, y=data_10_WA$Lat) 
coordinates(utm1) <- ~x+y 
proj4string(utm1) <- CRS("+proj=longlat +datum=WGS84") 
utm2 <- spTransform(utm1,CRS("+init=epsg:32610"))

data_10_WA$utmEast <- utm2$x
data_10_WA$utmNorth <- utm2$y

# turn coordinates into spatial points
points_WA <- data_10_WA %>% distinct(Site, .keep_all = TRUE) %>%
  st_as_sf(coords = c("utmEast", "utmNorth"), crs = 
             32610)
points_WA


# keep only 1 unique site row for each 

points_WA <- points_WA %>% distinct(Site, .keep_all = TRUE)
points_WA$Site

#### load in regional map for inset 
?usmap

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)


# load county names to label map library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
















#### load in state census tract shapefile for study site maps 

wa_map <- st_read(here("data", "tract20.shp")) %>% 
  na.omit()

wa_map <- wa_map %>% st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -122.7, xmax = -121.32, ymin = 47.13, ymax = 48.5))


# suggest top CRS 
suggest_top_crs(wa_map) # 6597 is projected CRS 

head(wa_map)

wa_map <- wa_map %>% dplyr::filter(substr(GEOID20, 1, 5) 
                                            %in% c("53033", "53053"))

mapview(wa_map)
# re-project and crop 
tractsKP <- st_transform(tractsKP, crs=4326)
tractsKP_crop <- st_crop(tractsKP, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))


# add water
water <- st_read(here("data", 
                      "DNR_Hydrography_-_Water_Bodies_-_Forest_Practices_Regulation.shp")) 

sf_use_s2(FALSE)
water <- water %>% st_transform(st_crs(wa_map)) %>%
  st_crop(wa_map)