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
library(tools)
devtools::install_github("ropensci/rnaturalearthhires")

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

#### load in camera locations
points_tawa <- st_read(here("data", "wa_camera_points.shp"))
points_tawa <- st_as_sf(points_tawa)

#### load in regional map for inset using rnaturalearth and maps

states_all <- ne_states(
  country = c("canada", "united states of america"),
  returnclass = "sf"
)

states_all <- states_all %>%
  filter(name_en == "Washington" | 
           name_en == "British Columbia")


# bring in county borders 
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))


# keep only WA counties 
counties <- subset(counties, grepl("washington", counties$ID))

# let's put everything in the same proj 
counties <- counties %>% st_transform(st_crs(states_all))
points_tawa <- points_tawa  %>% st_transform(st_crs(states_all))
st_crs(states_all) == st_crs(counties) 

# make the big map
(washington <- ggplot(data = states_all) +
    geom_sf(fill = "antiquewhite1") +
  #   geom_sf(data = water, fill = NA, color = gray(.5)) + 
     geom_sf(data = counties, fill = NA, color = gray(.5)) + 
#      geom_sf(data = points_tawa, size = 1, shape = 23, fill = "darkred") +
#     annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
   #           color = "grey22", size = 4.5) +
    coord_sf(xlim = c(-127, -120), ylim = c(46.5, 49), expand = FALSE) +
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                          size = 0.5), panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA)))


# add cities 
# wacities <- data.frame(state = rep("Washington", 5), city = c("Miami", 
 #                                                           "Tampa", "Orlando", "Jacksonville", "Sarasota"), lat = c(25.7616798, 
  #                                                                                                                   27.950575, 28.5383355, 30.3321838, 27.3364347), lng = c(-80.1917902, 


# make study site map 
(siteA <- ggplot(data = states_all) +
   geom_sf(fill = "antiquewhite1") +
    geom_sf(data = water, fill = "aliceblue", color = gray(.5)) + 
   geom_sf(data = points_tawa, size = 2, shape = 23, fill = "darkred") +
   coord_sf(xlim = c(-122.8,-122.30), ylim= c(47.18,47.5), expand = FALSE) + 
   annotate("text", x = -122.65, y = 47.4, label= "Site A", size = 6) + 
   theme_void() + 
   theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                         size = 0.5), panel.background = element_rect(fill = "aliceblue"), 
         panel.border = element_rect(fill = NA)))



#### load in state census tract shapefile for study site maps 

wa_map <- st_read(here("data", "tract20.shp")) %>% 
  na.omit()

wa_map <- wa_map %>% st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -127, xmax = -123, ymin = 46, ymax = 48.5))


# suggest top CRS 
suggest_top_crs(wa_map) # 6597 is projected CRS 

head(wa_map)

wa_map <- wa_map %>% dplyr::filter(substr(GEOID20, 1, 5),
                                             %in% c("53033", "53053"))

mapview(wa_map)
# re-project and crop 
tractsKP <- st_transform(tractsKP, crs=4326)
tractsKP_crop <- st_crop(tractsKP, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))


# add water
water <- st_read(here("data", 
                      "DNR_Hydrography_-_Water_Bodies_-_Forest_Practices_Regulation.shp")) 

sf_use_s2(FALSE)
water <- water %>% st_transform(st_crs(counties)) %>%
  st_crop(counties)
