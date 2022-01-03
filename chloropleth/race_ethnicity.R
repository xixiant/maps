library(pacman)
p_load(extrafont,
      showtext,
      ggtext,
      ggrepel,
      readxl,
      sf,
      biscale,
      cowplot,
      tidyverse,
      tidycensus,
      data.table)



# Next time ---------------------------------------------------------------

# add a link to the data - add another label
# nice curved lines to highlight what's happening with the data (see timo's blog)


# Useful references -------------------------------------------------------

# https://github.com/tashapiro/30DayMapChallenge/blob/main/red-beef-map/red-beef-map.R
# https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html
# https://oitco.hylandcloud.com/CDOTRMPop/docpop/docpop.aspx?clienttype=html&docid=8077600
# https://slu-opengis.github.io/biscale/articles/biscale.html
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

# data --------------------------------------------------------------------

# set working directory to github file location
setwd("~/GitHub/maps/chloropleth")

race_income <- "Estimate	Summary_estimate
B17020_002	B17020_001
B03002_003	B03002_001"

race_income <- fread(race_income)

call_df <- map2_dfr(race_income$Estimate,race_income$Summary_estimate,  ~get_acs(
  geography="county",      # county
  variables = .x,
  summary_var = .y,
  year = 2019,
  output = "tidy",
  state = "VT",
  key = census_api_key,
  moe_level = "95",
  survey = "acs5",
  geometry = TRUE
))

call_neat <- call_df %>%
  group_by(GEOID) %>%
  dplyr::summarize(
    "poverty" = sum (estimate [variable == 'B17020_002' ]) / sum (summary_est [variable == 'B17020_002' ]),
    "POC" = 1 - (sum( estimate [variable == "B03002_003"]) / sum ( summary_est [variable == "B03002_003"]))
  )

# setting up the bivariate chloropleth
# https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html 
call_bucket <- bi_class(call_neat, x = "poverty", y = "POC", style = "quantile", dim = 3)


# formatting + fonts --------------------------------------------------------------


# fonts
# this github exchange was useful: https://github.com/wch/extrafont/issues/32
# font_import()


# see a list of the fonts available to you with the help of extrafont
# fonts()


# pulling in specific fonts, setting formatting used in plot
font_add("Corbel", "C://Windows//Fonts//corbel.ttf")              # Update to correct file path
font_add("Impact", "C://Windows//Fonts//impact.ttf")              # Update to correct file path

chart_font<-"Corbel"
title_font<-"Impact"                                              # not used

font_color<-'#211c1d'
background<-"white"


# chart and chart elements ------------------------------------------------


# legend
legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Below poverty",
                    ylab = "People of color",
                    size = 8)


# plot
map <- ggplot()+
  geom_sf(data = call_bucket,
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
    title="People of color & income levels by county within Vermont",
    caption="American Community Survey 5-Year Estimates 2015-2019"
  )


# legend + plot together
final_plot <- ggdraw() +
  draw_plot(map, 0, 0, .8, 1) +
  draw_plot(legend,
            0,              # 0 = left side
            .3,              # 0 = bottom
            0.25,           # width
            0.25)           # height


final_plot

# saving
ggsave("race_ethnicity.jpeg", width = 28.5, height = 15.6, units='cm', dpi = 400)
