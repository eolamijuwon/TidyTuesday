## International Migration
CS2016_Immigra <- CS2016 %>%
  filter(Citizenship != "ZAF") %>%
  filter(Citizenship != "999") %>% 
  droplevels()



        immi_flows <- data.frame(table(CS2016_Immigra$Citizenship)) %>% 
          mutate(Var1 = as.character(Var1)) %>% 
          inner_join(country_codes,
                     by = c("Var1" = "Code")) %>% 
          mutate(Percentage = round(((Freq/sum(Freq))*100), digits = 2)) %>% 
          arrange(desc(Percentage)) %>% 
          head(n = 20) %>% 
          mutate(Country = as.character(Country)) %>% 
          mutate(Country = replace(Country, which(Country == "Congo"),
                                   "Congo [Republic]")) %>% 
          
          mutate(Country = replace(Country, which(Country == "Congo (Democratic Republic of the)"),
                                 "Congo [DRC]")) %>% 
          mutate(Country = replace(Country, which(Var1 == "GBR"),
                                   "United Kingdom")) %>% 
          inner_join(location_data, by  = c("Country" = "country")) %>% 
          dplyr::rename(lat_from = latitude,
                 long_from = longitude) %>% 
          mutate(lat_to = -30.559482,
                 long_to = 22.937506) %>% 
          mutate(Country = replace(Country, which(Country == "Swaziland"),
                                   "Eswatini")) %>% 
          mutate(Country = paste0(Country, " - {",  round(Percentage, digits = 1), "%}"))
        
        
        immi_flows$point <- with(immi_flows,
                                   ifelse(Percentage<1, 0.5,
                                          ifelse(Percentage>1 & Percentage<=3, 0.85,
                                                 ifelse(Percentage>3 & Percentage<=5, 1.2,
                                                        ifelse(Percentage>5 & Percentage <= 15, 1.55,
                                                               ifelse(Percentage>15 & Percentage<=35, 1.90,
                                                                      ifelse(Percentage>35, 2.25, 0)))))))


fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}

        routes = gcIntermediate(immi_flows[,c('long_from', 'lat_from')], 
                                immi_flows[,c('long_to', 'lat_to')], 200, 
                                breakAtDateLine = FALSE, addStartEnd = TRUE, sp=TRUE)
        fortifiedroutes = fortify.SpatialLinesDataFrame(routes) 
        
        #### merge to form great circles 
        routes_count = data.frame('count'=immi_flows$Percentage,
                                  'id'=1:nrow(immi_flows),
                                  'Countries'=immi_flows$Country,
                                  'point' = immi_flows$point)
        greatcircles = merge(fortifiedroutes, routes_count, all.x=T, by='id')
        
        
        
        
        
        worldmap = map_data ("world")


wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.05, colour = "black",
                     fill="#e7e6e3",
                     data=worldmap))



showtext_auto()
font_add("Roboto Condensed", regular = "RobotoCondensed-Regular.ttf",
         bold = "RobotoCondensed-Bold.ttf",
         bolditalic = "RobotoCondensed-BoldItalic.ttf",
         italic = "RobotoCondensed-Italic.ttf")
myFont <- "Roboto Condensed"




## Plot of International Migration Flows

ggplot() + 
  wrld +
  geom_line(aes(long,lat,group=id, color=Countries), 
            size=0.25, data= greatcircles) + 
  geom_point(data=immi_flows, 
             aes(x=long_from,
                 y=lat_from,
                 size = point), 
             fill = "white",
             alpha = 0.5) + 
  geom_point(data=immi_flows, 
             aes(x=long_from, y=lat_from, colour = Country), 
             size = 0.8, fill = NA,
             stroke = 0.5, 
             shape = 1, alpha = 0.7) + 
  theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.86, 0.52),
        legend.text = element_text(family =  myFont, size = 3.2,
                                   colour = "black"),
        legend.title = element_text(family =  myFont, size = 4,
                                    colour = "red", face = "bold", 
                                    lineheight = unit(0.9, "pt")),
        legend.background = element_rect(fill=NA),
        legend.key = element_rect(fill=NA,
                                  color = "black",
                                  size = 0.2),
        legend.key.size = unit(0.35, 'lines'),
        legend.justification=c("left", "center"),
        plot.margin=unit(c(0.3,0.3,0.3,0.3),"cm"),
        plot.background = element_rect("#acd7ec"),
                panel.grid = element_line("#acd7ec",
                                  size = 0.05),
        panel.background = element_rect("#acd7ec")) +
  guides(size = FALSE)+
  labs(colour = "Country of Citizenship")

ggsave("Immigration_ZA.pdf", dpi = 300, height = 3.2, width = 6)
