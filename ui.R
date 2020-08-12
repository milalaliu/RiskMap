library(shiny)
library(leaflet)
library(DT)


# Define UI for application that draws a histogram
navbarPage('天災風險地圖', id="main",
                tabPanel("Map", leafletOutput("bbmap", height=1000)))


