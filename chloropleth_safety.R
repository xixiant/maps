library(pacman)
p_load(maps,
      usdata,
      extrafont,
      showtext,
      ggtext,
      ggrepel,
      readxl,
      sf,
      biscale,
      cowplot,
      tidyverse)

# References --------------------------------------------------------------

# https://github.com/tashapiro/30DayMapChallenge/blob/main/red-beef-map/red-beef-map.R
# https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
# https://slu-opengis.github.io/biscale/articles/biscale.html
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

# data --------------------------------------------------------------------

# set working directory to github file location
setwd("~/GitHub/maps")


# pull county data from the maps package
counties <- map_data(county)


# importing crash data as a tibble (did prep in Excel)
# https://oitco.hylandcloud.com/CDOTRMPop/docpop/docpop.aspx?clienttype=html&docid=8077600
crashes <- read_excel("CDOTRM_CD_Crash_Listing_-_2019.xlsx", sheet = 3)


# setting up the bivariate chloropleth
# for me https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html 
crashes_bucketed <- bi_class(crashes, x = Day, y = Night, style = "quantile", dim = 3)


# merging datasets (2020 updates to tibble package require updates to R and sf must be loaded)
crash_counties <- left_join(counties, crashes_bucketed, by=c("NAME"="Lookup_county"))


# formatting + fonts --------------------------------------------------------------


# fonts
# this github exchange was useful: https://github.com/wch/extrafont/issues/32
# font_import()


# see a list of the fonts available to you with the help of extrafont
# fonts()


# pulling in specific fonts, setting formatting used in plot
font_add("Corbel", "C://Windows//Fonts//corbel.ttf")              # Use the actual file path
font_add("Impact", "C://Windows//Fonts//impact.ttf")              # Use the actual file path formerly gill sans bold

chart_font<-"Corbel"
title_font<-"Impact"

font_color<-'#211c1d'
background<-"white"
annotation_color<-"#05803e"


# chart and chart elements ------------------------------------------------


# legend
legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "More daytime crashes",
                    ylab = "More lower light crashes",
                    size = 8)


# plot
map <- ggplot()+
  geom_sf(data = crash_counties,
          mapping = aes(fill = bi_class),
          color = "white",
          size = 0.1,
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme(
    base_family = chart_font,
    base_size = 18,
    bg_color = background,
    font_color = font_color
  ) +
  labs(
    title="Motorcycle & bicycle accident rates by time of day",
    subtitle="For counties of the Colorado Front Range",
    fill="Accidents",
    caption="Using 2019 Statewide Crash Data Listings from CDOT | @hellville"
  )


# legend + plot together at last - only visible upon ggsaving
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend,
            0,          # 0 = left side
            0,             # 0 = bottom
            0.25,           # width
            0.25)           # height


ggsave("crash_map.jpeg", width = 28.5, height = 15.6, units='cm')
