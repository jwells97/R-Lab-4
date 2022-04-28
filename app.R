#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanelimg(src="filename", height = "xx%",
                  width = "yy%"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("rb1", "Choose a Location", c("Oxford" = "1", "Inlet Beach" = "2", "Charleston" = "3", "Nashville" = "4", "Birmingham" = "5", "Homewood" = "6", "Florence" = "7"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("cityPlot")
            
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    cityData <- reactiveValues()
    output$cityPlot <- renderPlot({
        leaflet(options = leafletOptions(zoomsnap = 0.1)) %>% 
        addTiles()
    })
    observe ({
        cityData$name <- c("Oxford", "Inlet Beach", "Charleston", "Nashville", "Birmingham", "Homewood", "Florence",)
        cityData$address <- c("719 N Lamar", "10711 E Hwy 30A", "456 Meeting St", "5304 Charlotte Ave", "5361 US-280", "1926 29th Avenue S", "315 N Court St")
        cityData$state <- c("MS", "FL", "SC", "TN", "AL", "AL", "AL")
        cityData$phone <- c("662.236.2666", "850.532.6952", "843.459.1800", "615.610.3403", "205.490.7568", "205.666.7099", "256.415.8545")
        cityData$zipcode <- c("38655", "32461", "29403", "37209", "35242", "35209", "35630")
        cityData$long <- c("-89.516107", "-86.010989", "-79.939229", "-86.851889", "-86.673662", "205.666.7099", "256.415.8545")
        cityData$lat <- c("34.375572", "30.2802131", "32.795024", "36.152353", "33.420685", "205.666.7099", "256.415.8545")
        
        if (input$rb1 == "1") {
            viewLong <- CentralData$long[1]
            viewLat <- CentralData$lat[1]
        }
        else if (input$rb1 == "2") {
            viewLong <- CentralData$long[2]
            viewLat <- CentralData$lat[2]
        }
        
        CentralLabel <- sprintf("<b>%s</b><br />%s<br />%s", 
                                CentralData$name, 
                                CentralData$address, 
                                CentralData$phone) %>%
            lapply(htmltools::HTML)
        
        proxy <- leafletProxy("CentralMap") %>% 
            clearMarkers()
        
        proxy %>%
            setView(lng = viewLong, lat = viewLat, zoom = 15) %>%
            addMarkers(lng = as.numeric(CentralData$long),
                       lat = as.numeric(CentralData$lat),
                       popup = CentralLabel,
                       label = CentralLabel)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
