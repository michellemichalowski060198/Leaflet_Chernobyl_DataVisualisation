#Data source https://www.kaggle.com/lsind18/chernobyl-data-air-concentration
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(lubridate)
library(sp)
library(rgdal)


#data preparation 
Chernobyl <- read.csv("/Users/michellemichalowski/Downloads/CHERNAIR.csv")
Chernobyl <- Chernobyl %>%
                rename(country = PAYS,
                     country_code = Code,
                     city = Ville,
                     Longitude = X,
                     Latitude = Y,
                     Iodine_131 = I.131..Bq.m3., 
                     Caesium_134 = Cs.134..Bq.m3., 
                     Caesium_137 = Cs.137..Bq.m3.) %>%
                     mutate(Date = ymd(Date),
                            country = as.factor(country),
                            country_code = as.factor(country_code),
                            city = as.factor(city),
                            Iodine_131 = as.numeric(Iodine_131),
                            Caesium_134 = as.numeric(Caesium_134),
                            Caesium_137 = as.numeric(Caesium_137)) %>% 
                     select(-End.of.sampling, -Duration.h.min.)

apply(Chernobyl, 2, function(x) sum(is.na(x)))

Chernobyl_2 <- Chernobyl %>%
              group_by(city) %>%
              summarise(mean_Iodine_131 = mean(Iodine_131, na.rm=TRUE),
                        mean_Caesium_134 = mean(Caesium_134, na.rm=TRUE),
                        mean_Caesium_137 = mean(Caesium_137, na.rm=TRUE),
                        Longitude = max(Longitude),
                        Latitude = max(Latitude))

#in which cities was radiation measured?
leaflet(Chernobyl) %>% addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPulseMarkers(lng = ~Longitude, lat = ~Latitude,
                  icon = makePulseIcon(heartbeat = 0.2))

leaflet(Chernobyl) %>% addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(
    clusterOptions = markerClusterOptions()
  ) 


#how strong was the radiation in those cities?
leaflet(Chernobyl_2) %>% addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(lng = ~30, lat = ~51, popup = "Chernobyl") %>%
  addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~mean_Caesium_137,
             blur = 40, max = 0.05, radius = 17)

leaflet(Chernobyl_2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 10, lat = 51, zoom = 4) %>%
  addCircleMarkers(lng = Chernobyl_2$Longitude, lat = Chernobyl_2$Latitude, 
                   weight = Chernobyl_2$mean_Caesium_134*25)
  

#lets take one country and look into the radiation more in detail (In this case Germany)
download.file(url = 'http://biogeo.ucdavis.edu/data/diva/adm/DEU_adm.zip', 
              destfile = 'deu.zip')
unzip(zipfile = 'deu.zip')

germany <- readOGR('DEU_adm0.shp')

leaflet(germany) %>%
  addPolygons() %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 10 , lat = 51, zoom = 5.5) %>%
  addCircleMarkers(lng = Chernobyl_2$Longitude, lat = Chernobyl_2$Latitude, weight = Chernobyl_2$mean_Caesium_134*25)
  
#how was radiation spread over Europe?
library(devtools)
#install older version of tmap to get Europe map
install_version("tmap", version = "1.0", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(rgdal)
library(RColorBrewer)
library(tmap)
library(leaflet)

proj4string(Europe)

europe_wgs84 <- spTransform(Europe, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#data prep for the plot
Chernobyl_3 <- Chernobyl %>%
  group_by(city, Date) %>%
  summarise(mean_Iodine_131 = mean(Iodine_131, na.rm=TRUE),
            mean_Caesium_134 = mean(Caesium_134, na.rm=TRUE),
            mean_Caesium_137 = mean(Caesium_137, na.rm=TRUE),
            Longitude = max(Longitude),
            Latitude = max(Latitude)) %>%
  mutate(Date = substr(Date,0,7))


leaflet(Chernobyl_3) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 10 , lat = 51, zoom = 3.5) %>%
  addPolygons(data = europe_wgs84, color = brewer.pal(n = 8, name = "YlOrRd"),
              weight = Chernobyl_3$mean_Iodine_131,
              group = "1986-04",
              popup = paste0("Iodine_131: ", round(Chernobyl_3$mean_Iodine_131,2)),
              highlightOptions = highlightOptions(weight = 1,
                                                  color = "white",
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = europe_wgs84, color = brewer.pal(n = 8, name = "YlOrRd"),
              weight = Chernobyl_3$mean_Iodine_131,
              group = "1986-05",
              popup = paste0("Iodine_131: ", round(Chernobyl_3$mean_Iodine_131,2)),
              highlightOptions = highlightOptions(weight = 1,
                                                  color = "white",
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = europe_wgs84, color = brewer.pal(n = 8, name = "YlOrRd"),
              weight = Chernobyl_3$mean_Iodine_131,
              group = "1986-06",
              popup = paste0("Iodine_131: ", round(Chernobyl_3$mean_Iodine_131,2)),
              highlightOptions = highlightOptions(weight = 1,
                                                  color = "white",
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = europe_wgs84, color = brewer.pal(n = 8, name = "YlOrRd"),
              weight = Chernobyl_3$mean_Caesium_137,
              group = "1986-08",
              popup = paste0("Iodine_131: ", round(Chernobyl_3$mean_Iodine_131,2)),
              highlightOptions = highlightOptions(weight = 1,
                                                  color = "white",
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright", labels = c("< 0.5",
                                      ">= 0.5 & <= 1",
                                      "> 1"), 
            colors = brewer.pal(n = 3, name = "YlOrRd"),
            title = "Iodine_131 concentration",
            opacity = 0.5) %>%
  addLayersControl(baseGroups = c("1986-04", "1986-05","1986-06", "1986-08"),
                   options = layersControlOptions(collapsed = FALSE))

