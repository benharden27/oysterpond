#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(leaflet)
library(shinydashboard)
library(oysterpond)

site_locs <- op_dat[!duplicated(op_dat$site),c(2,28,29)]
op_dat$depth = as.character(op_dat$depth)
op_dat <- select(op_dat,date,site,depth,temp,salinity,do_pc,tdn,tdp,chl)
op_meta_dat <- tibble::tibble(var = names(op_dat),
                              name = c("Date","Site","Depth",
                                       "Temperature",
                                       "Salinity",
                                       "Oxygen",
                                       "Nitrogen",
                                       "Phosophorus",
                                       "Chlorophyll-a"),
                              unit = c("","","m","^oC","ppt","%","mg/L",
                                       "ug/L","ug/L"))

# Define UI for application that draws a histogram
ui <- dashboardPage(

   # Application title
   dashboardHeader(title = "Oyster Pond Data Explorer"),

   # Sidebar with a slider input for number of bins
   dashboardSidebar(
     fluidPage(h3(textOutput("site"))),
     uiOutput("vars"),
     uiOutput("depths")
   ),

   dashboardBody(
     fluidRow(
       box(leafletOutput("map"), width = 12)
     ),
     fluidRow(
       box(plotOutput("series"), width = 12)
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderLeaflet({
    m <- leaflet(site_locs,
                 options = leafletOptions(zoomControl = FALSE,
                                          minZoom = 15, maxZoom = 15)) %>%
      addTiles() %>%
      addCircleMarkers(label = ~site)
  })

  site <- eventReactive(input$map_marker_click, {
    loc = input$map_marker_click
    site_locs$site[which(site_locs$lon==loc$lng)]
  })

  output$site <- renderText({
    site()
  })

  output$series <- renderPlot({
    data <- op_dat[op_dat$site == site(), ]
    data <- subset(data, depth %in% input$depth)
    ggplot(data) +
      geom_point(aes_string(x="date",y=input$var,color="depth"),
                 na.rm = TRUE)
  })

  output$vars <- renderUI({
    selectInput("var","Choose a variable...",
                choices = find_notna(op_dat,op_meta_dat,site()),
                selected = "temp")
  })

  output$depths <- renderUI({
    data <- op_dat[op_dat$site==site(), ]
    checkboxGroupInput("depth","Choose Depth",unique(data$depth),selected = "0")
  })

}

# Run the application
shinyApp(ui,server)

