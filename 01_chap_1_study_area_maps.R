### yasmine hentati
### study area maps


# load packages
library(mapview)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(sf)
library(sp)
library(viridis)
library(here)
# if needed: remotes::install_github("walkerke/crsuggest") 
library(crsuggest)
library(terra)
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
library(rmapshaper)
library(cowplot)
library(ggrepel) 


#### load in camera locations
points_tawa <- st_read(here("data", "wa_camera_points.shp"))
points_tawa <- st_as_sf(points_tawa)

points_sewa <- st_read(here("data", "points_sewa.shp"))
points_sewa <- st_as_sf(points_sewa)


#### load in state county and census tract shapefile for study site maps 

wa_map <- st_read(here("data", "tl_2016_53_cousub.shp")) 
wa_tract <- st_read(here("data", "tract20.shp"))

wa_map <- wa_map %>%  st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))

wa_tract <- wa_tract %>%  st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))

# add water
water <- st_read(here("data", 
                      "DNR_Hydrography_-_Water_Bodies_-_Forest_Practices_Regulation.shp")) 

sf_use_s2(FALSE)

# crop to our map area 
water <-  water %>% st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))



# let's put everything in the same proj 

points_tawa <- points_tawa  %>% st_transform(st_crs(wa_map))
points_sewa <- points_sewa  %>% st_transform(st_crs(wa_map))
st_crs(water) == st_crs(wa_map)

# clip counties shapefile to water 

wa_crop <- ms_erase(wa_map, water)
mapview(wa_crop) # for now we'll be lazy and manually erase the lines in the 
# water (that line up with canadian borders) later

# clip water 


# make the big map
(washington <- ggplot(data = wa_crop) +
    geom_sf(fill = "antiquewhite1") +
 #      geom_sf(data = water, fill = "alice blue", color = gray(.2)) + 
#      geom_sf(data = points_tawa, size = 1, shape = 23, fill = "darkred") +
#     annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
   #           color = "grey22", size = 4.5) +
    coord_sf(xlim = c(-125.5, -121), ylim = c(46.6, 48.5), expand = FALSE) +
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA)) + 

    # add bounding boxes for study areas
  geom_rect(
    xmin = -122.6,
    ymin = 47.17,
    xmax = -122.3,
    ymax = 47.35,
    fill = NA, 
    colour = "red",
    size = 1
  ))



# add cities 
# wacities <- data.frame(state = rep("Washington", 5), 
    #                   city = c("Seattle", "Tacoma", 
    #                            "Olympia", "Port Angeles", "Everett"), 
    #                   lat = c(25.7616798, 


#ggplot(data = world) +
#  geom_sf() +
#  geom_sf(data = counties, fill = NA, color = gray(.5)) +
#  geom_sf(data = flcities) +
#  geom_label_repel(data = flcities, aes(x = lng, y = lat, label = city), 
#                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
#                                                                                 -0.25, 0.5, 0.5, -0.5)) +
 # coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# clip tract shapefile to water 

tract_crop <- ms_erase(wa_tract, water)


# make study site map - tacoma 
(TAWA <- ggplot(data = tract_crop) +
   geom_sf(fill = "antiquewhite1") +
 #    geom_sf(data = water, fill = "aliceblue", color = gray(.5)) + 
   geom_sf(data = points_tawa, size = 4, shape = 23, fill = "darkred") +
   coord_sf(xlim = c(-122.6,-122.30), ylim= c(47.17,47.35), expand = FALSE) + 
   annotate("text", x = -122.48, y = 47.32, label= "Tacoma", size = 6) + 
 #    xlab("Longitude")+ ylab("Latitude") + 
    theme_void() + 
   theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "aliceblue"), 
         panel.border = element_rect(fill = NA)))

# make study site map - seattle 
(SEWA <- ggplot(data = tract_crop) +
    geom_sf(fill = "antiquewhite1") +
    #    geom_sf(data = water, fill = "aliceblue", color = gray(.5)) + 
    geom_sf(data = points_sewa, size = 4, shape = 23, fill = "darkred") +
    coord_sf(xlim = c(-121.85,-122.54), ylim= c(47.36,47.77), expand = FALSE) + 
    annotate("text", x = -122.44, y = 47.6, label= "Seattle", size = 6) + 
    #    xlab("Longitude")+ ylab("Latitude") + 
    theme_void() + 
    theme(panel.grid.major = element_blank(), panel.background = element_rect(fill = "aliceblue"), 
          panel.border = element_rect(fill = NA)))



#############################################################################################
### GRAVEYARD - RIP  ### 

#### load in regional map for inset using rnaturalearth and maps

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

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



# suggest top CRS 
suggest_top_crs(wa_map) # 6597 is projected CRS 

head(wa_map)

wa_map <- wa_map %>% dplyr::filter(substr(GEOID20, 1, 5),
                                   %in% c("53033", "53053"))

mapview(wa_map)
# re-project and crop 
tractsKP <- st_transform(tractsKP, crs=4326)
tractsKP_crop <- st_crop(tractsKP, c(xmin= -121.7, ymin = 46.7, xmax = -122.8, ymax = 47.8))
