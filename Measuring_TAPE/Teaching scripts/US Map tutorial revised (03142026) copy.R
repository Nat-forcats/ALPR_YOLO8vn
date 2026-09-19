### US MAP PLOTTING ###
# Updated to include AK, HI, PR and hopefully GU and VI? #


## BASIC OLD MAP ## 

# Install and load required libraries
install.packages("usmap")
# install.packages("ggplot2")
library(usmap)
library(ggplot2)

# Plot a basic US map with territories
plot_usmap() +
  labs(title = "US Map with Territories",
       subtitle = "Using the usmap package")

# Plot with custom fills (e.g., using built-in state population data)
plot_usmap(data = countypop, values = "pop_2015", include = c("AK", "HI", "PR", "GU", "VI")) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_void()


### This shows US nicely and alaska and hawai'i, but I don't actually see PR, GU or VI




##### NEW APPROACH ######
#  https://learning.nceas.ucsb.edu/2023-08-usgs/session_09.html

# Libraries
library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(stringr)
library(ggrepel)
library(patchwork)


# Load map from Tigris
## read data
# cb = T + 20m resol, removes American Samoa, Maraina Islands, Guam and Virgin Islands
us_states <- states(cb = TRUE, 
                    resolution = "20m")

## shift geometries for easy plotting
us_states_shift <- us_states %>% 
  shift_geometry()


# Load ecoregions - Continental US
## read data
ecoregions_cont_us <- read_sf("data/Aggr_Ecoregions_2015.shp")

unique(ecoregions_cont_us$WSA9_NAME)
st_crs(ecoregions_cont_us) #Albers, NAD83

# Loading ecoregion for Alaska (example for loading others?)
## Read data
ecoregion_ak_l3 <- read_sf("data/ak_eco_l3.shp")
st_crs(ecoregion_ak_l3) # EPSG",3338, Alaska Albers
plot(ecoregion_ak_l3$geometry)


# Loading major US cities
## read
us_cities <- read_sf("data/USA_Major_Cities.shp")
st_crs(us_cities) #WGS 84 / Pseudo-Mercator
plot(us_cities$geometry)

## transform to desired crs
us_cities_nad83 <- st_transform(us_cities,
                                crs = st_crs(us_states))
st_crs(us_cities_nad83)
plot(us_cities_nad83$geometry)

## shift geometries for easy plotting
us_cities_shift <- us_cities_nad83 %>% 
  shift_geometry()

plot(us_cities_shift$geometry)



# Loading states by ecoregion
## read data
us_state_casc <- read_csv("data/state_by_casc_region.csv")

## merge polygons to get CASC region areas
casc_shp <- us_states %>% 
  left_join(us_state_casc, by= c("GEOID", "STUSPS", "NAME")) %>% 
  group_by(casc_region) %>% 
  summarise(geometry = st_union(geometry))

plot(casc_shp$geometry)

## shift geometries for easy plotting
casc_shift <- casc_shp %>% 
  shift_geometry()

plot(casc_shift$geometry)




### CLEANING Ecoregion data

## Rename columns to a generic name
ecoregions_cont_us_clean <- ecoregions_cont_us %>% 
  rename(code = WSA9,
         name = WSA9_NAME)

## Select two main colums from Alaska ecoregion file and rename to match continental file + transforming to the smae CRS than continental file.
## note: choosing L1 because it is the mos broad ecoregion (4 ecoregions for alaska)
ecoregion_ak_clean <- ecoregion_ak_l3 %>% 
  select(code = NA_L1CODE,
         name = NA_L1NAME) %>% 
  mutate(name = str_to_sentence(name)) %>% 
  st_transform(st_crs(ecoregions_cont_us_clean))

##checking outcomes
unique(ecoregion_ak_clean$name)
st_crs(ecoregion_ak_clean)
plot(ecoregion_ak_clean$geometry)


### BIND ecoregion data to one file
ecoregions_all <- bind_rows(ecoregions_cont_us_clean, 
                            ecoregion_ak_clean)


# shift geom
ecoregions_shift <- ecoregions_all %>% 
  shift_geometry()

plot(ecoregions_shift$geometry)


#### PLOTTING #####
us_plot <-  ggplot()+
  geom_sf(data = ecoregions_shift,
          aes(fill = name))+
  geom_sf(data = us_states_shift,
          fill = NA,
          color = "darkgray")+
  geom_sf(data = us_cities_shift)+
  geom_sf(data = casc_shift,
          color = "red",
          fill = NA)+
  theme_void()+
  scale_fill_viridis_d()

us_plot



### CUSTOMIZE MAP ###

finalized_plot <-  ggplot()+
  geom_sf(data = ecoregions_shift,
          aes(fill = name),
          alpha = 0.5)+
  geom_sf(data = us_states_shift,
          fill = NA,
          color = "darkgray")+
  geom_sf(data = us_cities_shift,
          alpha = 0.3)+
  geom_sf(data = casc_shift,
          color = "black",
          size = 6,
          fill = NA)+
  #an alternative to label your can use geom_sf_text, plots just the text not the rectagle aroud it.
  geom_sf_label(data = casc_shift[c(1:7), ],
                aes(label = casc_region),
                size = 3)+
  geom_label_repel(
    data = casc_shift[c(8:10), ],
    aes(label = casc_region, 
        geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    min.segment.length = 2)+
  # colour = "magenta",
  # segment.colour = "magenta")+
  theme_void()+
  scale_fill_viridis_d(name = "Ecoregion")+
  labs(title = "Cities across Ecoregions in the US",
       subtitle = "Map is devided into CASC regions")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


finalized_plot



### Page has a few more options for plotting but it mostly moves around HI and AK on the map