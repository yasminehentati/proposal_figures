### iuwc 2023


## load packages 
pacman::p_load(readr, ggplot2, tidyr, dplyr, mapview, lme4, magrittr, lintr, sf, 
               raster, viridis, cowplot, markdown, st, here, tidycensus,
               crsuggest, terra, spatialEco, readr, ggfortify, rgeos, usmap,
               rnaturalearth, rnaturalearthdata, maps, tools, stringr,
               rmapshaper, cowplot, ggrepel, ggspatial, extrafont, RColorBrewer,
               vegan, PropCIs, DescTools, raster, proj4, rgdal, corrplot, lme4,
               forcats, melt)

# read in parasite counts 
counts <- read_csv(here("data", "Worm_counts - ALL(2).csv"))  

# read in carcass locations

carc_pts <- read_csv(here("data", "Necropsy_Dissection_Data.csv"))
glimpse(carc_pts)

# keep only coyotes drop NA 
carc_pts <- carc_pts %>%  drop_na(Weight) %>% 
  filter(Species == "Coyote")
glimpse(carc_pts)

# join to counts of parasites 
parasites <- left_join(counts, carc_pts, by = "UCP_ID")

# make weight into kg instead of g 

parasites$Weight <- (parasites$Weight)/1000
carc_pts$Weight <- (carc_pts$Weight)/1000

######## 
# summary stats -- carcasses with data

# percent male and female 
sex <- parasites %>%                                   
  group_by(Sex) %>%
  summarise(Percentage=n()/nrow(.))

head(parasites)

########################
# calculate prevalence 

# make all columns greater than 1 into 1s 
binary <- parasites[,3:8] %>% mutate_if(is.numeric, ~1 * (. > 0))
binary

# for taenia 

BinomCI(sum(binary$Taenia), nrow(binary),
        conf.level = 0.95,
        method = "clopper-pearson")

# for ancylo 

BinomCI(sum(binary$All_Hookworm), nrow(binary),
        conf.level = 0.95,
        method = "clopper-pearson")

# for echino

BinomCI(20, 20,
         conf.level = 0.95,
         method = "clopper-pearson")

BinomCI(sum(binary$Echinococcus), nrow(binary),
        conf.level = 0.95,
        method = "clopper-pearson")

# for toxocara

BinomCI(sum(binary$T_canis), nrow(binary),
        conf.level = 0.95,
        method = "clopper-pearson")



# mean intensity 
colnames(parasites)
# taenia 
mean(parasites[,3])

# hw
mean(parasites[, 7])

# toxocara

colMeans(parasites[sapply(parasites, is.numeric)], na.rm=TRUE)


### mean infracommunity species richness 

################
# build models to predict infection of each parasite 

# create new df 
colnames(parasites)

alldat <- parasites[,c("UCP_ID", "Sex", "Age", "Weight", "Taenia",
                       "Echinococcus", "All_Hookworm", "T_canis")]

## spleen mass over total mass
parasites$Spleen_mass <- as.numeric(parasites$Spleen_mass)
alldat$SpleenRatio <- (parasites$Spleen_mass)/(parasites$Weight) 

parasites$All_Hookworm

# correlation test 
cor(parasites$All_Hookworm, alldat$SpleenRatio)

## body length minus tail length
parasites$Total_length_mm <- as.numeric(parasites$Total_length_mm)
parasites$Tail_mm <- as.numeric(parasites$Tail_mm)
alldat$BodyOnly <- parasites$Total_length_mm - parasites$Tail_mm


head(alldat)


taenia <- lm(Taenia ~ SpleenRatio + Age, data = alldat)
summary(taenia)

hookworm <- lm(All_Hookworm ~ SpleenRatio + Age, data = alldat)
summary(hookworm)

echino <- lm(Echinococcus~ SpleenRatio + Age, data = alldat)
summary(echino)

toxo <- lm(T_canis ~ SpleenRatio + Age, data = alldat)
summary(echino)
## 
###############################################

#####  general carcass report 
 

### sex ratios 


sex_all <- carc_pts %>%                                   
  group_by(Sex) %>%
  summarise(Percentage=n()/nrow(.))

sex_all

county_all <- carc_pts %>% 
  group_by(County) %>% 
  summarise(Samples=n())

county_all
24+12+3+7+18
24/64
# change g to kg
carc_pts$Weight <- (carc_pts$Weight)/1000





#### PLOTS 
weights <- ggplot(carc_pts, aes(x = Sex, y = Weight, fill = Sex)) + 
  geom_boxplot() + 
#   stat_summary(fun=mean, geom="point", shape=23, size=4) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_brewer(palette="Greens") + 
  xlab("Sex")+ ylab("Weights (kg)") + 
  theme_classic()

weights

#  stacked bar chart 


### remove unknown 

carc_pts <- carc_pts %>%   
  filter(Age != "Unknown")


ggplot(carc_pts %>% count(County, Age),    
       aes(County, n, fill=Age)) +
  geom_bar(stat="identity") +   
  scale_fill_brewer(palette="Dark2") + 
  xlab("County")+ ylab("Number of coyotes") + 
  scale_y_continuous(breaks=c(5,10,15,20,25)) + 
  theme_classic()
  

###############################################################################

parasites2 <- parasites[,c(2,3,7,8)]

colnames(parasites2)[3] <- "Ancylostomatidae"
colnames(parasites2)[4] <- "Toxocara canis"
colnames(parasites2)[2]<- "Taenia spp."

head(parasites2)
parasites2 <- melt(parasites2, id.vars = "UCP_ID", variable.name = "Parasite",
     value.name="Intensity")
head(parasites2)

# plot abundance 

parasites2$Parasite <- factor(parasites2$Parasite, levels=c("Toxocara canis",
                                                            "Ancylostomatidae",
                                                            "Taenia spp."))

ggplot(parasites2, aes(fill=Parasite, 
                       y=Intensity, 
                       x=reorder(UCP_ID, -Intensity))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Set2") + 
  xlab("Coyote ID")+ ylab("Parasite intensity") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  



ggplot(parasites %>% count(County, Age),    
       aes(County, n, fill=Age)) +
  geom_bar(stat="identity") +   
  scale_fill_brewer(palette="Blues") + 
  xlab("County")+ ylab("Number of coyotes") + 
  theme_classic()



######################################################################### 
#                    spatial metrics 
# drop NAs in long lat 
carc_pts <- carc_pts %>%  drop_na(Latitude) %>% drop_na(Longitude) 

carc_pts <- st_as_sf(carc_pts, coords = c("Longitude", "Latitude"), crs = 
                           4326)


###### housing density

## read in housing density  data - starting with WA 

wa_housing <- st_read(here("data", "wa_blk10_Census_change_1990_2010_PLA2.shp"))

# filter to only counties of interest 
wa_housing <- wa_housing %>% dplyr::filter(substr(BLK10, 1, 5) 
                                           %in% c("53033", "53053", # king pierce 
                                                  "53061", "53035", # snoho kitsap
                                                  "53029", "53057")) # island skagit

# we only need 2010 housing data - select relevant info

wa_housing <- wa_housing %>% dplyr::select(BLK10, WATER10, POP10, 
                                           HU10, HUDEN10, HHUDEN10,
                                           PUBFLAG:geometry)
colnames(wa_housing)


##########

#### impervious surface
## impervious surface calculation 


# read in water
water <- st_read(here("data", 
                      "DNR_Hydrography_-_Water_Bodies_-_Forest_Practices_Regulation.shp")) 



# crop to our map area 
water <-  water %>% st_transform(crs = 4326) %>% 
  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))

head(water)

# load impervious cover raster map
imp_map <- raster(here("data", "NLCD_imp", 
                       "nlcd_2019_impervious_descriptor_l48_20210604.img"))


# imp_map <-  imp_map %>% projectRaster(crs = crs(water)) %>%
 #  st_crop(c(xmin = -125.5, xmax = -121, ymin = 46.5, ymax = 49))

sf_use_s2(FALSE)
head(water)
water <- st_transform(water, st_crs(imp_map))

head(water)

# clip water 
# water2 <- st_as_sf(water)

# impclipped <- raster::mask(imp_map, water)

# again reproject  points to match raster
# these are just the points not the buffers
carc_pts <- st_transform(carc_pts, st_crs(imp_map))

# imp buffer: extract the mean impervious cover around each point using 1000 m radius buffer
imp_buff <- raster::extract(imp_map, carc_pts, fun=mean, buffer= 1000, df=TRUE)
imp_buff

carc_pts$imp_buff <- imp_buff$nlcd_2019_impervious_descriptor_l48_20210604

# housing density buffer: need to rasterize 

wa_hous_rast <- rasterize(wa_housing, imp_map,
field = "HUDEN10")

# transform points to same proj as raster
points_WA<- st_transform(points_WA, st_crs(wa_hous_rast))

# calculate housing density average w/ 1000m buffer
hu_den <- raster::extract(wa_hous_rast, points_WA, fun=mean, buffer= 1000, df=TRUE)
hu_den

points_WA$huden2010 <- hu_den$layer

##### 



########### calculate average abundance

colnames(carc_pts)


