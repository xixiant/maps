library(tidyverse)
library(sf)
library(rgdal)
library(spdep)
library(pacman)
p_load(gridExtra,
       tidycensus)


# references --------------------------------------------------------------

# https://www.robert-hickman.eu/post/getis-ord-heatmaps-tutorial/

# you can also create hexbins in QGIS if you start to distrust the time it takes to create miniscule bins
# which you should not do with a large area
# it will hang
# and do nothing
# https://www.gislounge.com/using-qgis-create-hexbin-map-gisp-registrations/


# if you choose a reasonable bin size you can do it in R:
# https://strimas.com/post/hexagonal-grids/


# wanted to find the column numbers - this was handy
custom_glimpse <- function(df) {
  data.frame(
    col_name = colnames(df),
    col_index = 1:ncol(df),
    col_class = sapply(df, class),
    row.names = NULL
  )
}

# un comment if you need the column numbers
# custom_glimpse(evse_df)


# get data ----------------------------------------------------------------

setwd("C:/Users/aheller/Dropbox/CS/4 - Past projects/2021/LaMetro - 405")

# this woudl be something to improve - leave large files in a temp_dir
# downloads into a temp file
# temp_dir <- tempdir()

# shapefile data of 405 - 3 mi buffer
corridor <- read_sf(dsn = "405_3_mi_buffer.shp")

# evse data from afdc
evse_data <- read.csv("alt_fuel_stations_2021-12-07.csv")


# trimming down to L1 for this, creating repeated rows for each object
# https://stackoverflow.com/questions/2894775/repeat-each-row-of-data-frame-the-number-of-times-specified-in-a-column
# ran into a dataytpe error - this was helpful: https://www.statology.org/r-list-object-cannot-be-coerced-to-type-double/
evse_data_by_type <- evse_data %>%
  filter(evse_data[1] == "ELEC") %>%
  select(c(18,19,20,25,26)) # %>%                   # 18 is L1, 19 is L2, 20 is DCFC
  # filter(!is.na(.[1])) %>%
  # slice(rep(seq_len(n()), as.numeric(unlist(.[1])))) %>% 
  # select(-1)


county_area <- get_acs(
  geography="county",           # county
  variables = "B01003_001",
  year = 2019,
  output = "tidy",
  county = 037,                 # LA
  state = "CA",
  key = census_api_key,
  moe_level = "95",
  survey = "acs5",
  geometry = TRUE
)



# plotting ----------------------------------------------------------------

# convert data to simple features
evse <- evse_data_by_type %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs("EPSG:4269")) %>%
  #transform projection to match the boundary data
  st_transform(crs = st_crs(county_area)) %>%
  #remove evse outside of the limits of the corridor shapefile
  .[unlist(st_intersects(county_area, .)),]

# copy to csv
write.csv(evse, "evse_county2.csv")

head(evse)

# plot the evse locations (option 1)
p1 <- ggplot(evse) +
  geom_sf(data = corridor, fill = "grey") +
  geom_sf(colour = "blue", alpha = 0.2) +
  ggtitle("EVSE on 405") +
  theme_void()

plot(p1)


# bind the coordinates as numeric
evse <- evse %>%
  cbind(., st_coordinates(evse))


# plot the evse as a density map
p2 <- ggplot() +
  geom_sf(data = corridor) +
  stat_density_2d(data = evse, aes(X, Y)) +
  ggtitle("EVSE on 405") +
  theme_void()

plot(p2)


# generate grid -----------------------------------------------------------


# generate a grid of points separated hexagonally
# no way to do this purely in sf yet afaik
hex_points <- spsample(as_Spatial(corridor), type = "hexagonal", cellsize = 5000.0)


# generate hexagon polygons from these points
hex_polygons <- HexPoints2SpatialPolygons(hex_points, dx = 5000.0) %>%
  st_as_sf(crs = st_crs(corridor)) %>%
  #clip to the corridor shapefile
  st_intersection(., corridor)



# gridded maps ------------------------------------------------------------


# find how many evse are within each hexagon
hex_polygons$planning_no <- lengths(st_intersects(hex_polygons, evse))

# plot the number of evse per bin
p6 <- ggplot(hex_polygons) +
  geom_sf(aes(fill = planning_no), color = "gray21") +
  scale_fill_viridis_c(option = "magma", "# plugs") +
  theme_void()
#  ggtitle("Binned EVSE")

plot(p6)


arrange <- grid.arrange(p3,p5,p6, nrow = 1)

ggsave("arranged_plot3.png", arrange,  width = 7, height = 4, units='in', dpi = 400)


# getis-ord binned map ----------------------------------------------------


#find the centroid of each hexagon and convert to a matrix of points
hex_points <- do.call(rbind, st_geometry(st_centroid(hex_polygons))) %>%
  unlist() %>%
  as.matrix.data.frame()

#use a k-nearest-neighbour algorithm to find which shape neighbour which
#super easy for the hexagons analytically obvs but important for e.g. using the ward boundaries instead
neighbouring_hexes <- knn2nb(knearneigh(hex_points, k = 6), 
                             row.names = rownames(hex_points)) %>%
  include.self()

#calculate the local G for a given variable (#bus stops) using the neihbours found previously
localGvalues <- localG(x = as.numeric(hex_polygons$planning_no),
                       listw = nb2listw(neighbouring_hexes, style = "B"),
                       zero.policy = TRUE)

#bind this back to the sf as a numeric variable column
hex_polygons$smooth_planning_no <- as.numeric(localGvalues)

# plot the statistic
# +ve indicates more than would be expected
p4 <- ggplot(hex_polygons) +
  geom_sf(aes(fill = smooth_planning_no)) +
  scale_fill_viridis_c(option = "magma", name = "Gi* Statistic") +
  theme_void() +
  ggtitle("Getis-Ord Binned EVSE on 405")

plot(p4)

spplot(hex_polygons)

# leaving off: going to create three separate datasets for each charger type, create images