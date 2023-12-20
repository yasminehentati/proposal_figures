#### carcass study area maps 
## yasmine hentati

library(pacman)
pacman::p_load(readr, ggplot2, tidyr, dplyr, mapview, lme4, magrittr, lintr, sf, 
               raster, viridis, cowplot, markdown, st, here, tidycensus,
               crsuggest, terra, spatialEco, readr, ggfortify, rgeos, usmap,
               rnaturalearth, rnaturalearthdata, maps, tools, stringr,
               rmapshaper, cowplot, ggrepel, ggspatial, extrafont)


# read in necropsy data
necdata <- read_csv(here("data", "All_Carcasses_5-23-23.csv"))
head(necdata) 


loc_data <- necdata[,c("Individual ID", "Species", "Latitude", "Longitude")] %>% na.omit()

# turn into spatial data
carc_pts <- st_as_sf(loc_data, coords = c("Longitude", "Latitude"), crs = 
                         4326)
mapview(carc_pts)

# save shapefile
st_write(carc_pts, here("data", "points_carcasses.shp"),    append = FALSE)




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

carc_pts <- carc_pts  %>% st_transform(st_crs(wa_map))
st_crs(water) == st_crs(wa_map)

# clip counties shapefile to water 

wa_crop <- ms_erase(wa_map, water)


# add cities 
# wacities <- data.frame(state = rep("Washington", 5), 
#                    city = c("Seattle", "Tacoma", 
#                             "Olympia", "Port Angeles", "Everett"), 
#                    lat = c(25.7616798, 

?scale_color_manual

### prepare for map

# jitter carcass points 
carc_points_jittered <- st_jitter(carc_pts, 0.04)
mapview(carc_points_jittered)


p1 <- ggplot(carc_pts) + 
  geom_sf() + 
  ggtitle('Original')

p2 <- ggplot(carc_points_jittered) + 
  geom_sf() + 
  ggtitle('Jittered')

plot_grid(p1,p2)

# see how they look together
p3 <- ggplot() + 
  geom_sf(data = carc_pts, color = 'red') + 
  geom_sf(data = carc_points_jittered, color = 'black') + 
  ggtitle('Both')

p3

# add cities 
wacities <- data.frame(state = rep("Washington", 3), 
                    city = c("Seattle", "Tacoma", "Everett"), 
                    lat = c(47.582813, 47.255169, 47.980286),
                    lng = c(-122.336582, -122.450305, -122.210186))

# make into spatial
wacities_pts <- st_as_sf(wacities, coords = c("lng", "lat"), crs = 
                       4326)

(washington <- ggplot(data = wa_crop) +
  geom_sf(fill = "#CDC5B4", color = NA) +
  geom_sf(data = carc_points_jittered, size = 3, shape = 21, aes(fill=Species)) + 
  scale_fill_manual(
    values = c("Coyote" = "#CE8147",
               "Raccoon" = "#561D25",
               "River otter" = "#7F7F7F"),
                     labels = c("Coyote", "Raccoon", "River otter")) +
  #     annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
  #           color = "grey22", size = 4.5) +
       geom_sf(data = wacities_pts) +
       geom_text_repel(data = wacities, aes(x = lng, y = lat, label = city), 
                      fontface = "bold", 
                      nudge_x = c(0.2, 0.05, 0.1), 
                      nudge_y = c(-0.1, -0.05, -0.1)) +
  coord_sf(xlim = c(-123, -121.75), ylim = c(47, 48.52), expand = FALSE) +
  xlab("Longitude")+ ylab("Latitude")+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#dcf0f5"), 
        panel.border = element_rect(fill = NA)) +     
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering))


# zoomed in map of seattle area carcasses
(seattle <- ggplot(data = wa_crop) +
    geom_sf(fill = "#CDC5B4", color = NA) +
    geom_sf(data = carc_pts, size = 3, shape = 21, aes(fill=Species)) + 
    scale_fill_manual(
      values = c("Coyote" = "#CE8147",
                 "Raccoon" = "#561D25",
                 "River otter" = "#7F7F7F"),
      labels = c("Coyote", "Raccoon", "River otter")) +
    #     annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
    #           color = "grey22", size = 4.5) +
    coord_sf(xlim = c(-121.85,-122.54), ylim= c(47.36,47.77), expand = FALSE) +
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "#dcf0f5"), 
          panel.border = element_rect(fill = NA)) +     
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering))



########
# Coyote only maps

carc_pts_coy <- carc_pts %>% filter(Species == "Coyote")
head(carc_pts_coy)

carc_pts_jitter_coy <- carc_points_jittered %>% filter(Species == "Coyote")
head(carc_pts_jitter_coy)

(washington <- ggplot(data = wa_crop) +
    geom_sf(fill = "#CDC5B4", color = NA) +
    geom_sf(data = carc_pts_jitter_coy, size = 3, shape = 21, aes(fill=Species)) + 
    scale_fill_manual(
      values = c("Coyote" = "#CE8147"),
      labels = c("Coyote")) +
    #     annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
    #           color = "grey22", size = 4.5) +
    geom_sf(data = wacities_pts) +
    geom_text_repel(data = wacities, aes(x = lng, y = lat, label = city), 
                    fontface = "bold", 
                    nudge_x = c(0.2, 0.05, 0.1), 
                    nudge_y = c(-0.1, -0.05, -0.1)) +
    coord_sf(xlim = c(-123, -121.75), ylim = c(47, 48.52), expand = FALSE) +
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "#dcf0f5"), 
          panel.border = element_rect(fill = NA)) +     
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering))



### with env health 


(washington2<-ggplot() +
    #  geom_sf(fill = "#CDC5B4", color = "white", lwd = 0.2) +
    geom_sf(data = env_WA_sp, 
      aes(fill = Rank), 
      lwd = 0,
      colour = "white") +
    scale_fill_gradientn(
      colors = c("#9DBF9E", "#FCB97D", "#A84268"),
      na.value = "grey80",
      limits = c(1, 10),
      name = "Environmental health rankings") +
    # carcass points 
    geom_sf(data = carc_pts_jitter_coy, size = 3, shape = 21, fill = "grey22") + #, aes(fill = Species)) + 
#      scale_color_manual(
  #      values = c("Coyote" = "#CE8147"),
  #      labels = c("Coyote")) +
         annotate(geom = "text", x = -85.5, y = 27.5, label = "Gulf of Mexico", 
               color = "grey22", size = 4.5) +
    geom_sf(data = wacities_pts) +
    geom_text_repel(data = wacities, aes(x = lng, y = lat, label = city), 
                    fontface = "bold", 
                    nudge_x = c(0.2, 0.05, 0.1), 
                    nudge_y = c(-0.1, -0.05, -0.1)) +
    coord_sf(xlim = c(-123, -121.75), ylim = c(47, 48.52), expand = FALSE) +
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "#dcf0f5"), 
          panel.border = element_rect(fill = NA)) +     
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering))


##### read in coyote scat points 
coy_scats <- read_csv(here("data", "all_scats_5-27-23.csv")) 

# clean data
coy_scats2 <- coy_scats %>% 
  drop_na(Latitude) %>% drop_na(Longitude) %>% # remove NAs in points  
  group_by(Site) %>% 
  filter(n() >= 5) %>% # remove sites with less than 10 scats 
  filter(!grepl('Other', Site)) # remove anything classified as "other"

summary <- coy_scats2 %>%                         
  group_by(Site) %>% 
  summarise(n = n()) 



# check class of lat long columns
sapply(coy_scats2$Latitude, class)

# change to numeric 
coy_scats2$Latitude <- as.numeric(as.character(coy_scats2$Latitude))
coy_scats2$Longitude <- as.numeric(as.character(coy_scats2$Longitude))

# drop NAs agai because introduced by coercion for some reason
coy_scats2 <- coy_scats2%>% 
  drop_na(Latitude) %>% drop_na(Longitude)


# turn into sf 
coy_scats2 <- st_as_sf(coy_scats2, coords = c("Longitude", "Latitude"), crs = 
                    4326)

# keep only one piont for each site  
scats_sites <- coy_scats2 %>% distinct(Site, .keep_all = TRUE)

# different cities than last time  
wacities2 <- data.frame(state = rep("Washington", 4), 
                       city = c("Seattle", "Bellevue", "Renton", "Shoreline"), 
                       lat = c(47.582813, 47.6101497, 47.479627, 47.7559659),
                       lng = c(-122.336582, -122.2015159, -122.2079218, -122.3456972))
# make into spatial
wacities2_pts <- st_as_sf(wacities2, coords = c("lng", "lat"), crs = 
                           4326)

head(env_WA_sp)

#### Plot 
(seattle <- ggplot(data = env_WA_sp) +
   #  geom_sf(fill = "#CDC5B4", color = "white", lwd = 0.2) +
    geom_sf(
      aes(fill = Rank), 
      lwd = 0,
      colour = "white") +
    scale_fill_gradientn(
      colors = c("#9DBF9E", "#FCB97D", "#A84268"),
      na.value = "grey80",
      limits = c(1, 10),
      name = "Environmental health rankings") +
    # carcass points 
    geom_sf(data = carc_pts_jitter_coy, size = 3, shape = 21, aes(fill = Species)) + 
    scale_fill_manual(
      values = c("Coyote" = "#CE8147"),
      labels = c("Coyote")) +
    
    # scat sites 
    geom_sf(data = scats_sites, size = 3, shape = 8, aes(fill = "darkred")) + 
    
    # cities 
    geom_sf(data = wacities2_pts) +
    geom_label(data = wacities2, aes(x = lng, y = lat, label = city), 
                    fontface = "bold") + 
   #  geom_text_repel(data = wacities2, aes(x = lng, y = lat, label = city), 
   #                  fontface = "bold", 
   #                  nudge_x = c(-0.02, 0.05, 0.02, 0.02), # Seattle Bellevue Renton Shoreline
  #                   nudge_y = c(-0.01, -0.01, 0.02, -0.02)) +
     
    # map boundaries 
    coord_sf(xlim = c(-122.12,-122.45), ylim= c(47.45,47.76), expand = FALSE) +
    
    # water bodies 
    annotate(geom = "text", 
             x = c(-122.38,  -122.23, -122.09), 
             y = c(47.61,  47.675, 47.62), 
             label= c("Elliott Bay", "Lake \nWashington",
                      "Lake \nSammamish"),
             fontface = "italic",  color = "grey22",  size = 3.3) + 
    annotation_scale(location = "bl", width_hint = 0.4) +
    
    # theme stuff 
    xlab("Longitude")+ ylab("Latitude")+
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "#dcf0f5"), 
          panel.border = element_rect(fill = NA)) +     
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering))


