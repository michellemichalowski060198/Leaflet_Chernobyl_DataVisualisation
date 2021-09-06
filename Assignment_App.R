
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
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

#remove rows with NAs
Chernobyl_2 <- na.omit(Chernobyl_2)



ui <- fluidPage(
    titlePanel("Leaflet in Shiny!"),
    sliderInput("range", "Iodine levels", min(Chernobyl_2$mean_Iodine_131, na.rm = TRUE), 
                max(Chernobyl_2$mean_Iodine_131, na.rm = TRUE),
                value = range(Chernobyl_2$mean_Iodine_131, na.rm = TRUE), step = 0.1
    ),
    leafletOutput("mymap")
)

server <- function(input, output, session) {
    filteredData <- reactive({
        Chernobyl_2[Chernobyl_2$mean_Iodine_131 >= input$range[1] & 
                        Chernobyl_2$mean_Iodine_131 <= input$range[2],]
    })
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            setView(lng = 10, lat = 51, zoom = 4) %>%
            addCircleMarkers(lng = filteredData()$Longitude, lat = filteredData()$Latitude)
    })
}

shinyApp(ui, server)
