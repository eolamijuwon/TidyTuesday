

## Author   : Emmanuel Olamijuwon
## Twitter  : @eOlamijuwon
## Date     : 09/10/2019

## ======================================
## Visualizing Internal and International
## Migration Flows in South Africa

## Data access and description available online:
## South Africa - Community Survey 2016
## https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/611



# Load/Instal Libraries

load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

packages <- c("tidyverse", "patchwork", "circlize", 
              "chorddiag", "readxl", "maptools", "rgdal",
              "mosaic", "geosphere", "rgeos", "maps",
              "showtext")
load(packages)




# Load dataset from github
CS2016 <- readstata13::read.dta13("C:\\Users\\eOlamijuwon\\Downloads\\CS2016Person_F1.dta")
# CS2016 <- readstata13::read.dta13("./CS2016Person_F1.dta")

## Data for extracting Country names from codes
## based on information available in the data documentation
country_codes <- read_xlsx ("./Country Codes.xlsx")

## Geo-Location Data Obtained from
## https://developers.google.com/public-data/docs/canonical/countries_csv
location_data <- read_xlsx ("./location_data.xlsx")


CS2016_ZA <- CS2016 %>%
  filter(COB == 888) %>%
  filter(POB != "Outside south africa" &
           POB != "Do not know" &
           POB != "Unspecified") %>%
  filter(Usual_Province != "Outside South Africa" &
           Usual_Province != "Do not know" &
           Usual_Province != "Unspecified") %>%
  droplevels()



## Internal Migration FLows
m <- matrix(table(CS2016_ZA$POB, CS2016_ZA$Usual_Province),
            byrow = FALSE, nrow = 9, ncol = 9)

        provinces <- levels(CS2016_ZA$POB)
        data_2wayTable <- data.frame(m)
        
        
        # colnames(data)
        dimnames(data_2wayTable) <- list(origin = provinces,
                            destination = provinces)
        
        # 
        ## Code adapted from :
        ## https://www.data-to-viz.com/graph/chord.html
        
        # Reshape data to long format
        data_long <- data_2wayTable %>%
          rownames_to_column %>%
          gather(key = 'key', value = 'value', -rowname)
        
        # parameters
        circos.clear()
        circos.par(start.degree = 90, gap.degree = 4, 
                   track.margin = c(-0.1, 0.1), 
                   points.overflow.warning = FALSE)
        par(mar = rep(0, 4))
        
        # color palette
        mycolor <- c("#3F9A6F", "#99B841", "#DBB951",
                         "#8D662B", "#8322D6", "#980100",
                         "#01649B", "#787478", "#000000")
        
        # Base plot
        chordDiagram(
          x = data_long, 
          grid.col = mycolor,
          transparency = 0.25,
          directional = 1,
          direction.type = c("arrows", "diffHeight"), 
          diffHeight  = -0.04,
          annotationTrack = "grid", 
          annotationTrackHeight = c(0.05, 0.1),
          link.arr.type = "big.arrow", 
          link.sort = TRUE, 
          link.largest.ontop = TRUE)
        
        # Add text and axis
        circos.trackPlotRegion(
          track.index = 1, 
          bg.border = NA, 
          panel.fun = function(x, y) {
            
            xlim = get.cell.meta.data("xlim")
            sector.index = get.cell.meta.data("sector.index")
            
            # Add names to the sector. 
            circos.text(
              x = mean(xlim), 
              y = 3.2, 
              labels = sector.index, 
              facing = "bending", 
              cex = 0.8
            )
            
          }
        )
        

## International Migration
CS2016_Immigra <- CS2016 %>%
  filter(Citizenship != "ZAF") %>%
  filter(Citizenship != "999") %>% 
  droplevels()


immi_flows <- data.frame( table(CS2016_Immigra$Citizenship)) %>% 
  inner_join(country_codes,
             by = c("Var1" = "Code")) %>% 
  mutate(Percentage = round(((Freq/sum(Freq))*100), digits = 2)) %>% 
  arrange(desc(Percentage)) %>% 
  
  mutate(Country = replace(Country, which(Country == "Congo"),
                           "Congo [Republic]")) %>% 
  
  mutate(Country = replace(Country, which(Country == "Congo (Democratic Republic of the)"),
                         "Congo [DRC]")) %>% 
  mutate(Country = replace(Country, which(Var1 == "GBR"),
                           "United Kingdom")) %>% 
  head(n = 20) %>% 
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

routes = gcIntermediate(immi_flows[,c('long_from', 'lat_from')], immi_flows[,c('long_to', 'lat_to')], 200, breakAtDateLine = FALSE, addStartEnd = TRUE, sp=TRUE)
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
                     # "#DBB951", 
                     data=worldmap))



showtext_auto()
font_add("Roboto Condensed", regular = "RobotoCondensed-Regular.ttf",
         bold = "RobotoCondensed-Bold.ttf",
         bolditalic = "RobotoCondensed-BoldItalic.ttf",
         italic = "RobotoCondensed-Italic.ttf")
myFont <- "Roboto Condensed"





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
        # plot.title = element_text(family =  myFont, size = 45, face = "bold",
        #                           color = "green", 
        #                           lineheight = 1.3),
        # plot.subtitle = element_text(family =  myFont, size = 23,
        #                              colour = "gray"),
        text = element_text(family =  myFont, size = 6),
        legend.position = c(0.86, 0.52),
        legend.text = element_text(family =  myFont, size = 3.2,
                                   colour = "black"),
        legend.title = element_text(family =  myFont, size = 4,
                                    colour = "red", face = "bold", 
                                    lineheight = unit(0.9, "pt")),
        legend.background = element_rect(fill=NA),
        legend.key = element_rect(fill=NA,
                                  color = "black"),
        legend.key.size = unit(0.35, 'lines'),
        legend.justification=c("left", "center"),
        plot.margin=unit(c(0.3,0.3,0.3,0.3),"cm"),
        plot.background = element_rect("#acd7ec"),
        
        
        plot.caption = element_text(family =  cmr, size = 18,
                                    color = "white"),
        panel.grid = element_line("#acd7ec",
                                  size = 0.05),
        panel.background = element_rect("#acd7ec")) +
  guides(size = FALSE)+
  labs(colour = "Country of Citizenship")

ggsave("Immigration_ZA.pdf", dpi = 300, height = 3.2, width = 6)
