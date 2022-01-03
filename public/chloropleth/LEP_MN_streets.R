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

# data --------------------------------------------------------------------


# bounding box for Minneapolis (St. Paul?)
bbx <- getbb("Minneapolis")

# let's try st_intersection next

# gather main streets
streets <- bbx %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", 
                  value=c("motorway",
                          "trunk",
                          "primary",
                         "secondary",
                         "tertiary",
                         "motorway_link",
                         "trunk_link",
                         "primary_link",
                         "secondary_link",
                         "tertiary_link"
                  )) %>%
  osmdata_sf()

# looking at those lines
ggplot() +
  geom_sf(data = streets$osm_lines,
          aes(color = highway),
          size = .4,
          alpha = .65)+
  theme_void()

# list of Minneapolis counties in MN
minn <- c("27003", "27019", "27037", "27053", "27123")


minn_counties <- str_sub(minn, 3,5)

# census data 
minn_call <- map_dfr(minn_counties, ~get_acs(
    geography = "block group",           
    variable = "B03002_003",
    summary_var = "B03002_001",
    year = 2019,
    output = "tidy",
    state = "MN",                       # state
    county = .x,
    key = census_api_key,
    moe_level = "95",
    survey = "acs5",
    cache = TRUE,
    geometry = TRUE
  ))

head(minn_call)



# wrangle
# need to double check my estimates and summary estimates against the table entries
minn_LEP <- minn_call  %>%
  group_by(GEOID) %>%
  summarise(
    LEP_over_5 = sum (estimate [
      variable == 'B16004_007' |
        variable == 'B16004_008' |
        variable == 'B16004_012' |
        variable == 'B16004_013' |
        variable == 'B16004_017' |
        variable == 'B16004_018' |
        variable == 'B16004_022' |
        variable == 'B16004_023' |
        variable == 'B16004_029' |
        variable == 'B16004_030' |
        variable == 'B16004_034' |
        variable == 'B16004_035' |
        variable == 'B16004_039' |
        variable == 'B16004_040' |
        variable == 'B16004_044' |
        variable == 'B16004_045'], 
      na.rm = TRUE) /
      sum ( estimate [ variable == "B16004_001"])
  )


# Get an appropriate CRS
minn_crs <- suggest_top_crs(minn_LEP, units = "m")


# Generate a 1km hexgrid over the census data
minn_grid <- minn_LEP %>%
  st_transform(minn_crs) %>%
  st_make_grid(cellsize = 1000, square = FALSE)




# Use areal interpolation to transfer block group 
# values to the grid
minn_interpolated <- minn_LEP %>%
  st_crop(xmin = min(bbx[1,]),
          xmax = max(bbx[1,]),
          ymin = min(bbx[2,]),
          ymax = max(bbx[2,])) %>%
  st_transform(minn_crs) %>%
  select("LEP_over_5") %>%
  na.omit() %>%
  st_interpolate_aw(
    to = minn_grid,
    extensive = FALSE
  )


mapview::mapviewOptions(fgb = FALSE)


# Take a look at the results
mapview(minn_interpolated, zcol = "LEP_over_5", 
        layer.name = "LEP over 5,<br/>2015-2019 ACS")


# plot --------------------------------------------------------------------


# pulling in specific fonts, setting formatting used in plot
font_add("Corbel", "C://Windows//Fonts//corbel.ttf")              # Update to correct file path
font_add("Impact", "C://Windows//Fonts//impact.ttf")              # Update to correct file path

font_color<-'#211c1d'
background<-"white"


# formatting
col_road <- '#FFFFFF'
col_background <- '#16324F'
col_labs  <- '#46B3FF'





#  geom_sf(aes(fill = income), color = "#FFFFFF")+

# the thing
# I think I'll need to change the aes to the hex value - look at anohter example of hex mapped using ggplot
ggplot() +
    geom_sf(data = minn_interpolated,
          aes(fill = LEP_over_5),
          color = "#FFFFFF",
          alpha = 0.95) +
  scale_fill_viridis() +
  geom_sf(data = streets$osm_lines,
          aes(color = highway),
          size = .1,
          color = col_road,
          alpha = 0.8) +
  # scale_fill_gradientn(colours = magma(10),
  #                      labels = c("0km", "100km", "200km", "300km", "400km", "500km"),
  #                      guide = guide_colourbar(
  #                        title.position = "top",
  #                        direction = "horizontal",
  #                        barwidth = 17,
  #                        barheight = 0.5,
  #                        frame.colour = "black"
  #                      )) # +
  guides(fill =
           guide_legend(title.position = "top",
                        title.hjust =0.5,
                        label.position = "bottom",
                        nrow = 1)) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = col_background),
    text = element_text(
      family="Gill Sans",
      color="white"),
    plot.margin = unit(c(0, 2, 0.2, 2), "cm"),
    plot.title =element_text(
      family = "Corbel",
      color="white",
      size= 20,
      hjust=0.99,
      vjust= -10),
    plot.subtitle = element_text(
      size=14,
      hjust=0.92,
      vjust=-97.5),
    plot.caption=element_text(
      size=10,
      margin=margin(t=10,b=5)),
    legend.position = c(0.1,1)
  ) +
  labs(title = "Limited English Proficiency in Minneapolis",
       caption = "Data from ACS 2019 & OpenStreetMap | Chart @hellville",
       fill = "Concentration of LEP population")


