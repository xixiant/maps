library(pacman)
p_load(tidycensus,
       tidyverse,
       sf,
       mapview,
       crsuggest,
       ggrepel,
       osmdata,
       data.table,
       viridis,
       extrafont)

# references --------------------------------------------------------------

# https://gist.github.com/walkerke/c6331bff465a2abf5c3c25b3dbd45afd
# https://github.com/tashapiro/30DayMapChallenge/blob/main/staten_island/staten_island_pizza.R
# https://schmidtynotes.com/r/gis/ggplot/2019/11/30/hex-plots-and-apis-in-r.html
# https://www.bls.gov/cew/classifications/areas/county-msa-csa-crosswalk.htm

# data --------------------------------------------------------------------


# list of Boston counties in MA (five digit FIPS)
boston <- c("25009", "25017", "25021", "25023", "25025")


# tidycensus doesn't want the state code - drop the first two characters
boston_counties <- str_sub(boston, 3,5)


# census pull
call <- map_dfr(boston, ~get_acs(
    geography = "block group",
    variable = "B03002_003",
    summary_var = "B03002_001",
    year = 2019,
    output = "tidy",
    state = "MA",                       # state
    county = boston_counties,
    key = census_api_key,
    moe_level = "95",
    survey = "acs5",
    cache = TRUE,
    geometry = TRUE
  ))


# wrangle
POC <- call  %>%
  group_by(GEOID) %>%
  summarise(
    POC = 1 - (sum( estimate [variable == "B03002_003"]) / sum ( summary_est [variable == "B03002_003"]))
  )


# Get an appropriate CRS
boston_crs <- suggest_top_crs(POC, units = "m")


# Generate a 1km hexgrid over the census data
grid <- POC %>%
  st_transform(boston_crs) %>%
  st_make_grid(cellsize = 1000, square = FALSE)



# Use areal interpolation to transfer block group 
# values to the grid
boston_interpolated <- POC %>%
  st_transform(boston_crs) %>%
  select("POC") %>%
  na.omit() %>%
  st_interpolate_aw(
    to = grid,
    extensive = FALSE
  )


# avoid using a flatgeobuf file format, which did not work
mapview::mapviewOptions(fgb = FALSE)


# Take a look at the results
mapview(boston_interpolated, zcol = "POC", 
        layer.name = "Percent people of color,<br/>2015-2019 ACS")