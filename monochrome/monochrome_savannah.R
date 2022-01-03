library(pacman)
p_load(osmdata,
       tidyverse,
       sf,
       tigris,
       extrafont)

##### monochrome of Savannah and Hilton Head
##### #geocompr #rstats #greatmigration #monochrome

# next time ---------------------------------------------------------------


# inelegant execution to get GA and SC data - check out do.call, see about combining the function
# switched up colors at the end because something else is being shaded black and not finding what it is
# look for some other fonts already loaded to machine by specifying a path this way: font_import(paths = c("c:/path/to/folder/with/fonts/", prompt = F)

# references --------------------------------------------------------------


# https://www.google.com/maps/@32.0286735,-81.1206937,11.62z?authuser=1
# http://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/

# more on how to deal with time on osm queries:
# https://rdrr.io/cran/osmdata/man/opq.htm

# color picker
# https://www.w3schools.com/colors/colors_picker.asp?colorhex=343c47

# color brewer for mapping
# https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

# nice labels for OSM
# https://github.com/gkaramanis/30DayMapChallenge/blob/0638c30cdb4f4f7b01db6e9bd9e59284e0b04f5a/2021/05-osm/05-osm-speed.R

# good presentation on using annotate in ggplot
# https://evamaerey.github.io/ggplot2_grammar_guide/annotate.html#1

# get data ----------------------------------------------------------------


# not using this now, but note that there are preset options for setting bounding box for the data
# bbx <- getbb("Savannah, GA")

# and now defining my own bounding box
min_lat <- 31.8223
max_lon <- -80.6187
max_lat <- 32.3419
min_lon <- -81.4744

bbx <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))

colnames(bbx) <- c("min","max")


# must change the timeout value or osm will timeout before returning values for custom bounding boxes
# using pre-defined datasets also seems to help but looks like it requires good knowledge
# of the osm data structures
# https://rdrr.io/cran/osmdata/man/opq.html


# collecting the osm data for Savannah using my custom bounding box
highways <- bbx %>%
        opq(timeout = 200) %>%                             # must change the timeout value or osm will timeout before returning values for custom boundary boxes
        add_osm_feature(key = "highway",                   # more on opq: https://rdrr.io/cran/osmdata/man/opq.html 
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



# new query for even smaller data - up timeout
streets <- bbx %>%
        opq(timeout = 500) %>%                        # can add the memsize argument to exceed limit - note memsize  is expressed in bytes
        add_osm_feature(key = "highway", 
                        value = c("residential",
                                  "living_street",
                                  "service",
                                  "unclassified",
                                  "pedestrian",
                                  "footway",
                                  "track",
                                  "path")) %>%
        osmdata_sf()



# land data
counties_GA <- counties(state = "GA",
                        cb = T,
                        class = "sf")

counties_SC <- counties(state = "SC",
                        cb = T,
                        class = "sf")


# function for GA water
get_water1 <- function(county_GEOID){
        area_water("GA", county_GEOID, class = "sf")
}

water_GA <- do.call(rbind, 
                 lapply(counties_GA$COUNTYFP,get_water))


# function for SC water
get_water2 <- function(county_GEOID){
        area_water("SC", county_GEOID, class = "sf")
}

water_SC <- do.call(rbind, 
                 lapply(counties_SC$COUNTYFP,get_water2))


# wrangling ---------------------------------------------------------------


# combining those objects (fix this later)
counties_all <- rbind(counties_SC, counties_GA)
water <- rbind(water_GA, water_SC)


# trimming to only include elements in the bounding box
water <- st_crop(water,
                 xmin=min_lon,xmax=max_lon,
                 ymin=min_lat,ymax=max_lat)


# removing the water fill from the land features
st_erase <- function(x, y) {
        st_difference(x, st_union(y))
}

counties_all <- st_erase(counties_all,water)


# formatting --------------------------------------------------------------


# this wound up being my favorite color picker: https://www.w3schools.com/colors/colors_picker.asp?colorhex=343c47

# loadfonts()

land_color <- "#000000"                     # darkest peach reveals some black color - not sure what element is being mapped, skipping for now
road_color <- "#ffeee6"                     # light peach
water_color <- "#ffeee6"                    # also light peach
h1_font <- "Georgia"
caption_font <- "Corbel"
font_color <- "#ff884d"


# plot --------------------------------------------------------------------

final_map <- 
        ggplot() + 
        # land
        geom_sf(data = counties_all,
                inherit.aes = FALSE,
                lwd = 0.0,
                fill = land_color) +
        # streets
        geom_sf(data = streets$osm_lines,
                inherit.aes = FALSE,
                color = road_color,
                size = .4,
                alpha = .65) +
        # highways
        geom_sf(data = highways$osm_lines,
                inherit.aes = FALSE,
                color = road_color,
                size = .6,
                alpha = .65) +
        coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
                 ylim = c(min(bbx[2,]), max(bbx[2,])),
                 expand = FALSE) +
        # annotations
        annotate("text",
                 x = bbx[1, 1] + .03,
                 y = bbx[2, 1] + 0.5,
                 hjust = "left",
                 vjust = "top",
                 label = "Savannah",
                 family = h1_font,
                 size = 26,
                 fontface = "bold",
                 color = font_color) +
        annotate("text",
                 x = bbx[1, 2] - .02,
                 y = bbx[2, 1] + 0.075, 
                 hjust = "right", 
                 vjust = "top", 
                 size = 7, 
                 family = caption_font, 
                 label = "Sources:\nOpenStreetMap, TIGER shapefiles\n@hellville", 
                 color = "#ff7733", 
                 lineheight = 0.9) +
        # themes
        theme_void(base_family = caption_font,
                   base_size = 14) +
        # theme(
        #         plot.background = element_rect(fill = "#ff9966", color = NA),
        #         plot.caption = element_text(hjust = 1, color = "grey80"),           # attaches to the labs caption
        #         plot.margin = margin(0, 0, 0, 0)
        # ) +
        # water
        theme(panel.background =
                      element_rect(fill = water_color))


final_map

ggsave(final_map, 
       filename = "Savannah.png",
       scale = 1, 
       width = 10, 
       height = 10, 
       units = "in",
       dpi = 200)

