library(pacman)
p_load(tidyverse,
       sf,
       rgdal)


# references --------------------------------------------------------------

# https://www.robert-hickman.eu/post/getis-ord-heatmaps-tutorial/
# GADM provides maps and spatial data for all countries and their subdivisions:
# https://gadm.org/index.html

# data --------------------------------------------------------------------

# get the shapefile data of london from GADM
# downloads into a temp file
# leaving off here - download and save the file like normal

# fails
gadm_url <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_GBR_2_sf.rds"
temp_dir <- tempdir()

# fails
london <- download.file(gadm_url, destfile = file.path(temp_dir, "london_shapefile.rds"), 
              mode = "wb", quiet = TRUE)



str(london)

#get the bus stop data
#https://data.london.gov.uk/dataset/tfl-bus-stop-locations-and-routes
bus <- read.csv("https://data.london.gov.uk/download/tfl-bus-stop-locations-and-routes/673033eb-59a6-4903-84bc-d24495661707/bus-stops-10-06-15.csv",
                stringsAsFactors = FALSE) %>%
  filter(!is.na(Location_Easting)) %>%
  #convert to simple features
  st_as_sf(coords = c("Location_Easting", "Location_Northing"), crs = st_crs("+init=epsg:27700")) # %>%
  #transform projection to match the boundary data
#  st_transform(crs = st_crs(london)) %>%
  #remove bus stops outside of the limits of the london shapefile
#  .[unlist(st_intersects(london, .)),]

str(bus)

#plot the bus stop locations
p1 <- ggplot(bus) +
  geom_sf(data = london, fill = "grey") +
  geom_sf(colour = "blue", alpha = 0.2) +
  ggtitle("Bus Stops in London") +
  theme_void()

plot(p1)



#bind the coordinates as numeric
bus <- bus %>%
  cbind(., st_coordinates(bus))

#plot the bus stops as a density map
p2 <- ggplot() +
  geom_sf(data = london) +
  stat_density_2d(data = bus, aes(X, Y)) +
  ggtitle("Bus Stops in London") +
  theme_void()

plot(p2)



#merge the london wards into one boundary file
london_union <- london %>%
  group_by("group") %>%
  summarise()

#generate a grid of points separated hexagonally
#no way to do this purely in sf yet afaik
hex_points <- spsample(as_Spatial(london_union), type = "hexagonal", cellsize = 0.01)

#generate hexgaon polygons from these points
hex_polygons <- HexPoints2SpatialPolygons(hex_points) %>%
  st_as_sf(crs = st_crs(london_union)) %>%
  #clip to the london shapefile
  st_intersection(., london_union)




#find how many bus stops are within each hexagon
hex_polygons$planning_no <- lengths(st_intersects(hex_polygons, bus))

#plot the number of bus stops per bin
p2 <- ggplot(hex_polygons) +
  geom_sf(aes(fill = planning_no)) +
  scale_fill_viridis_c(option = "magma", "# Bus Stops") +
  theme_void() +
  ggtitle("Binned London Bus Stops")

plot(p2)



ibrary(spdep)

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

#plot the statistic
#+ve indicates more than would be expected
p3 <- ggplot(hex_polygons) +
  geom_sf(aes(fill = smooth_planning_no)) +
  scale_fill_viridis_c(option = "magma", name = "Gi* Statistic") +
  theme_void() +
  ggtitle("Getis-Ord Binned London Bus Stops Statistic")

plot(p3)