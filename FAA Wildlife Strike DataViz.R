
#Author: Emmanuel Olamijuwon
#Date: 23 Jul 2019


library(tidyverse)
library(lubridate)
library(extrafont)
library(showtext)
        showtext_auto()
        font_add_google("Oswald", "Oswald")
        font_paths() 
        font_files()
        font_add("Oswald", "Oswald-Light.ttf")
        myFont <- "Oswald"
library(ggmap)
library(maptools)
library(rgdal)
library(RColorBrewer)


bird_impacts <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-07-23/wildlife_impacts.csv")


bird_impacts %>% 
        mutate(
          month = incident_date %>% 
            month(label = TRUE) %>% 
            as.factor(),
          year = incident_date %>% year()) %>% 
        group_by(month, year) %>% 
        mutate(incidents = n()) %>% 
        ungroup() %>% 
        ggplot(aes(x = year, y = month)) + 
        geom_tile(aes(fill = incidents), 
                   colour = "black",
                  size = 1) + 
        scale_fill_viridis_c("Number of \nIncidents", option="magma") +
        scale_x_continuous(breaks = c(1990:2018)) +
        labs(y = "Month of the Year", x = "Year",
             title = "Total Number of Incidents (1990 - 2018)",
             subtitle = "The total number of wildlife strikes have been high between the months of May and Oct since since 1999",
             caption = "Data: FAA National Wildlife Strike Database 1990 - 2018 | DataViz: @eOlamijuwon") +
        theme_minimal(base_family = myFont) +
        theme(axis.text.y = element_text(size = 20,
                                         colour = "white"),
              axis.text.x = element_text(size = 20,
                angle = 90, hjust = 1,
                colour = "white"),
              axis.title.x = element_text(margin = unit(c(0.2, 0, 0, 0), "cm"),
                                          colour = "green"),
              axis.title.y = element_text(margin = unit(c(0.0, 0.2, 0, 0), "cm"),
                                          colour = "green"),
              plot.title = element_text(size = 35, face = "bold",
                                        color = "green", 
                                        lineheight = 1.3),
              plot.subtitle = element_text(size = 23,
                                           colour = "gray"),
              text = element_text(size = 20),
              legend.position = "right",
              legend.text = element_text(size = 20,
                                         colour = "white"),
              legend.title = element_text(size = 20,
                                          colour = "green", 
                                          lineheight = 1.3),
              plot.margin=unit(c(0.3,0.3,0.3,0.3),"cm"),
              plot.background = element_rect("black"),
              plot.caption = element_text(size = 18,
                                          color = "white"))
ggsave("Incidents.png", dpi = 300, height = 3.5, width = 8)



impacts_operator <- bird_impacts %>% 
              group_by(operator) %>% 
              mutate(total_operation = n()) %>% 
              ungroup() %>% 
              filter(damage == "D" | damage == "M" |
                       damage == "S" | damage == "M?") %>% 
              group_by(operator) %>% 
              mutate(damaged = n()) %>% 
              distinct(operator, .keep_all = TRUE) %>% 
              ungroup() %>% 
              mutate(std_damageRate = (damaged/total_operation)*100) %>% 
              ggplot(mapping = aes(x = reorder(operator, std_damageRate),
                                   y = std_damageRate)) +
              geom_col(fill = "black",
                       color = "green") + 
              coord_flip() +
              theme_minimal(base_family = myFont) +
              theme(axis.text.y = element_text(size = 23,
                                               colour = "white"),
                    axis.text.x = element_text(size = 23,
                                               angle = 90, hjust = 1,
                                               colour = "white"),
                    axis.title.x = element_text(margin = unit(c(0.2, 0, 0, 0), "cm"),
                                                colour = "green",
                                                size = 27),
                    axis.title.y = element_text(margin = unit(c(0.0, 0.2, 0, 0), "cm"),
                                                colour = "green",
                                                size = 27),
                    plot.title = element_text(size = 35, 
                                              color = "green", 
                                              lineheight = unit(0.4, "pt")),
                    plot.subtitle = element_text(size = 25,
                                                 color = "gray"),
                    legend.text = element_text(size = 20,
                                               colour = "white"),
                    legend.title = element_text(size = 20,
                                                colour = "green"),
                    plot.margin=unit(c(0.3,0.3,0.3,0.3),"cm"),
                    plot.background = element_rect(fill = "black",
                                                   colour = "black"),
                    panel.grid = element_line("black"),
                    panel.background = element_rect("black"),
                    plot.caption = element_text(size = 25,
                                                color = "white")) +
              labs(y = "Percentage of Airline Damaged", x = "Airline (Operator)",
                   title = "Percentage of Airlines \nDamaged by Wildlife \nStrikes Since 1990")

  
  bird_impacts %>% 
              filter(damage == "D" | damage == "M" |
                       damage == "S" | damage == "M?") %>% 
              filter(airport_id != "ZZZZ") %>% 
              group_by(airport_id) %>% 
              mutate(damaged = n()) %>% 
              distinct(airport_id, .keep_all = TRUE) %>% 
              ungroup() %>% 
              filter(rank(desc(damaged))<=25) %>%
              ggplot(mapping = aes(x = reorder(airport, damaged),
                                   y = damaged)) +
              geom_col(fill = "black",
                       color = "green") + 
              coord_flip() +
              theme_minimal(base_family = myFont) +
              theme(axis.text.y = element_text(size = 23,
                                               colour = "white"),
                    axis.text.x = element_text(size = 23,
                                               colour = "white"),
                    axis.title.x = element_text(margin = unit(c(0.2, 0, 0, 0), "cm"),
                                                colour = "green",
                                                size = 28),
                    axis.title.y = element_text(margin = unit(c(0.0, 0.2, 0, 0), "cm"),
                                                colour = "green",
                                                size = 28),
                    plot.title = element_text(size = 35, 
                                              color = "green", 
                                              lineheight = unit(0.4, "pt")),
                    plot.subtitle = element_text(size = 12),
                    legend.text = element_text(size = 20,
                                               colour = "white"),
                    legend.title = element_text(size = 20,
                                                colour = "green"),
                    plot.margin=unit(c(0.3,0.3,0.3,0.3),"cm"),
                    plot.background = element_rect(fill = "black",
                                                   colour = "black"),
                    panel.grid = element_line("black"),
                    panel.background = element_rect("black"),
                    plot.caption = element_text(size = 23,
                                                color = "white")) +
              labs(y = "Total number of incidents reported", x = "Airport",
                   title = "Top 25 Airports With Airlines Reported Damaged \nas a Result of Wildlife Strikes Since 1990",
                   caption = "Data: FAA National Wildlife Strike Database 1990 - 2018 | DataViz: @eOlamijuwon") +
                annotation_custom(grob = ggplotGrob(impacts_operator), 
                                  xmin = 1.5, xmax = 12, 
                                  ymin = 75, ymax = 210)
  
ggsave("Incidents_airport.png", dpi = 300, height = 9, width = 6.5)



