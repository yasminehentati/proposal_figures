################


### get seattle camera data

library(here)
library(readr)
library(dplyr)
library(sf)
library(mapview)

sewa_pts <- read_csv(here("data", "SEWA_metadata.csv"))
head(sewa_pts)
?unique

# keep only one row for each site 
sewa_sites <- sewa_pts %>% distinct(station, .keep_all = TRUE)


source("mason_site_collapse_code/qaqc_sites.R")
source("mason_site_collapse_code/long_to_zone.R")
source("mason_site_collapse_code/fix_site_names.R")

head(sewa_sites)

# make data spatial
sewa_sites <- st_as_sf(sewa_sites, coords = c("lat", "lon"), crs = 
                        4326)

head(sewa_sites)
mapview(sewa_sites)

# save shapefile
st_write(sewa_sites, here("data", "points_sewa.shp"),    append = FALSE)


