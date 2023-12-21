#### otter spraints 



# library(pacman)
pacman::p_load(ggplot2, tidyr, dplyr, mapview, lme4, magrittr, lintr, sf, 
               raster, viridis, cowplot, markdown, sf, here, tidycensus,
               crsuggest, terra, spatialEco, readr, ggfortify, rgeos, usmap,
               rnaturalearth, rnaturalearthdata, maps, tools, stringr,
               rmapshaper, cowplot, ggrepel, ggspatial, extrafont, 
               janitor, rstatix, flextable)


# load in points 

scats <- read_csv(here("data", "Otter_scat_data.csv")) %>% drop_na(any_of("Scat ID"))

# keep only otters, drop NAs 
# scats <- scats %>% # drop_na(Latitude) %>% drop_na(Longitude) %>% 
#   filter(Species == "River otter")


summary <- scats %>%                         
  group_by(Site) %>% 
  summarise(n = n())



# check class of lat long columns
sapply(scats$Latitude, class)

# change to numeric 
scats$Latitude <- as.numeric(scats$Latitude)
scats$Longitude <- as.numeric(scats$Longitude)



# turn into sf 
scats <- st_as_sf(scats, coords = c("Longitude", "Latitude"), crs = 
                         4326)

# keep only one piont for each latrine 
scats_sites <- scats %>% distinct(Site, .keep_all = TRUE)


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


# clip counties shapefile to water 
wa_crop <- ms_erase(wa_map, water)

# clip tract shapefile to water 
tract_crop <- ms_erase(wa_tract, water)

# read in env health data 
env_data <- read_csv(here("data", "env_health_ranks_wa.csv"))

env_WA_sp <- merge(tract_crop, env_data, by.x = "GEOID20",
                   by.y = "GEOID", all.x = TRUE) 

# crop to only king, kitsap and pierce counties 
env_WA_sp <- env_WA_sp %>% dplyr::filter(substr(GEOID20, 1, 5) 
                                         %in% c("53053", "53033",
                                                "53035"))


# plot latrines - plain 
(seattle <- ggplot(data = env_WA_sp) +
    geom_sf(fill = "#CDC5B4", color = "white", lwd = 0.2) +
    geom_sf(data = scats_sites, size = 4, shape = 8, fill = "darkred",
            color = "darkred") + 
    
    # city names 
 #    geom_sf(data = wacities_pts) +
  #   geom_text_repel(data = wacities, aes(x = lng, y = lat, label = city), 
   #                  fontface = "bold", family = "Futura-Bold",
   #                  nudge_x = c(0.2, 0.05, 0.1), 
   #                  nudge_y = c(0.1, -0.05, -0.1)) +

    # bounding coordinates 
    coord_sf(xlim = c(-122.1,-122.54), ylim= c(47.36,47.77), expand = FALSE) +
    
    # water bodies 
    annotate(geom = "text", 
             x = c(-122.38, -122.43, -122.23, -122.44), 
             y = c(47.61, 47.685, 47.675, 47.55), 
                label= c("Elliott Bay", "Shilshole \nBay", "Lake \nWashington",
                         "Duwamish/ \nGreen River"),
             fontface = "italic",  color = "grey22",  size = 3.3) + 
     annotate(
       geom = "segment", x = -122.41, y = 47.55, xend = -122.32, yend = 47.53, 
       color = "grey22", size = 0.3,
         arrow = arrow(length = unit(1, "mm"))) +
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




#### descriptive tables 

# change date to date 
scats$Date <- as.Date(scats$Date, "%m/%d/%Y")

# add month column

scats[,"month"] <- format(scats$Date, "%m")

)

scats %>% tabyl(Site)
scats %>% tabyl(month)


scats_wide <- scats %>% pivot_longer(cols = c("Date"), names_to = "Site",values_to = "Scat ID")
scats %>%                      
  count(Site, month) %>%     # group and tabulate counts by two columns
  ggplot()+                       # pass new data frame to ggplot
  geom_col(                     # create bar plot
    mapping = aes(   
      x = outcome,              # map outcome to x-axis
      fill = month,           # map age_cat to the fill
      y = ))                   # map the counts column `n` to the height
