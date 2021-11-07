library(pacman)
p_load(ggplot2,
      maps,
      usdata,
      dplyr,
      extrafont,
      showtext,
      ggtext,
      ggrepel,
      tibble)


# References --------------------------------------------------------------

# https://github.com/tashapiro/30DayMapChallenge/blob/main/red-beef-map/red-beef-map.R
# https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/


# data --------------------------------------------------------------------

counties <- map_data(county)



#merge world map with data on meat consumpsion
world_map_df<-left_join(world_map,df_2013, by=c("region"="Entity"))
summary(df_2013$kg_capita)

#colors from R Color Brewer



#fonts
font_import()
font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")              # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")    # Use the actual file path
chart_font<-"Gill Sans"
title_font<-"Gill Sans Bold"

#text for annotations
# leaving for reference
# text_label<-c("Top beef and buffalo meat consumption in Argentina (55.5kg), followed by Brazil (39.3kg)",
#               "United States ranked as 3rd highest conusmption (32.2kg)",
#               "Liberia lowest consumption (0.78kg)",
#               "India 2nd lowest consumption (0.81kg). Roughly 80% population practices Hinduism (prohibits beef).")
# text_lat<-c(-50,30,-10,-20)
# text_long<-c(-28,-150,-10,75)
# text_desc<-c("C1","C2","C3","C4")
# text_df<-data.frame(text_label,text_lat,text_long,text_desc)
# 
# #points
# point_desc<-c("ARG","IND","USA","LIB","BRZ")
# point_lat<-c(-36.416,20.593,40,7.4,-14.23)
# point_long<-c(-63.616,78.96,-110,-9.4,-51.92)
# point_df<-data.frame(point_desc,point_lat,point_long)

#x is long, #y is lat
#arrows to point between annotations and dots


#setcolors
#fill colors
colors<-c(
  '#fee5d9',
  '#fcbba1',
  '#fc9272',
  '#fb6a4a',
  '#ef3b2c',
  '#cb181d',
  '#99000d')
background<-'#1C1D21'
font_color<-"white"
annotation_color<-"#FF9D00"

ggplot()+
  geom_polygon(
    data=world_map_df,
    aes(x=long,y=lat,group=group, fill=bin),
    color=background, size=0.2
  )+
  guides(fill = 
           guide_legend(title.position = "top",
                        title.hjust =0.5,
                        nrow = 1))+
  geom_polygon(
    data=world_map_df%>%filter(is.na(kg_capita)),
    aes(x=long,y=lat,group=group),
    fill="grey",
    color=background, size=0.2
  )+
  scale_fill_manual(values=colors, na.translate = F)+
  geom_point(
    data=point_df,
    inherit.aes=F,
    aes(x=point_long,y=point_lat),
    color='#2E1F27'
  )+
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
               color = annotation_color)+
  geom_textbox(
    data=text_df%>%filter(text_desc %in% c("C1","C4")),
    aes(x=text_long,y=text_lat,label=text_label),
    size=2.5,
    width = unit(0.1, "npc"),
    fill=background,
    color="white",
    box.color=annotation_color
  )+
  geom_textbox(
    data=text_df%>%filter(text_desc %in% c("C3","C2")),
    aes(x=text_long,y=text_lat,label=text_label),
    size=2.5,
    width = unit(0.07, "npc"),
    fill=background,
    color="white",
    box.color=annotation_color
  )+
  coord_map(xlim=c(-180,180))+
  theme_void()+
  theme(
    plot.background=element_rect(fill=background),
    legend.position="top",
    text=element_text(color=font_color, family=chart_font),
    plot.title=element_text(family=title_font,hjust=.5,
                            vjust=5),
    plot.subtitle =element_text(hjust=.5, vjust=5),
    plot.margin = unit(c(0.8, 4.2, 0.8, 4.2), "cm")
  )+
  labs(
    title="Beef & Buffalo Meat Consumption Rates (2013)",
    subtitle="Consumption rate based on kg per capita",
    fill="Kg Per Capita",
    caption="Data from OurWorldInData.org | Chart @tanya_shapiro"
  )


ggsave("red_beef_map.jpeg", width = 28.5, height = 15.6, units='cm')