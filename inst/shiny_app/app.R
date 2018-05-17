#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinydashboard)
library(ggplot2)

op_dat <- readRDS("data/op_dat.rds")
site_locs <- op_dat[!duplicated(op_dat$site),c(2,28,29)]
op_dat$depth = as.character(op_dat$depth)

# Define UI for application that draws a histogram
ui <- dashboardPage(

   # Application title
   dashboardHeader(title = "Oyster Pond Data Explorer"),

   # Sidebar with a slider input for number of bins
   dashboardSidebar(
     selectInput("site","Choose a site...", choices = site_locs$site , selected = "op1"),
     selectInput("var","Choose a variable...", choices = names(op_dat)[3:26], selected = "temp"),
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
    m <- leaflet(op_dat)
    m <- addTiles(m)
    m <- addCircleMarkers(m,label = ~site)
  })

  observeEvent(input$map_marker_click, {
    loc = input$map_marker_click
    print(loc)
    site <- site_locs$site[which(site_locs$lon==loc$lng)]
    output$clickloc <- renderText({site})
  })

  output$series <- renderPlot({
    data <- op_dat[op_dat$site == input$site, ]
    data <- subset(data, depth %in% input$depth)
    print(data)
    ggplot(data) +
      geom_point(aes_string(x="date",y=input$var,color="depth"))
  })

  output$depths <- renderUI({
    data <- op_dat[op_dat$site==input$site, ]
    checkboxGroupInput("depth","Choose Depth",unique(data$depth),selected = "0")
  })

}

# Run the application
shinyApp(ui,server)

