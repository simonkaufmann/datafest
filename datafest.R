library(readxl)
Data_Fest <- read_excel("University/Data Fest.xlsx")


library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)


## app.R ##
library(shinydashboard)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("Date", "Time:", min = as.Date("2019-09-01", "%Y-%m-%d"), max = as.Date("2020-05-25", "%Y-%m-%d"),value=as.Date("2016-12-01"), timeFormat="%Y-%m-%d"),
                #selectInput("colors", "Color Scheme",
                            #rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                #),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

# Scotland, London, Wales, North England, Midlands, South
positions <- data.frame(lat = c(56.8648, 51.467339, 52.499, 54.6208, 53.16275, 51.013329), long = c(-4.388, -0.110214, -3.8987, -2.311, -1.2526, -2.80393), mag = c(40, 100, 50, 45, 33, 29))

getPositions <- function() {
  return(positions)
}

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    #quakes[positions$mag >= -1000 & positions$mag <= 1000,]
    #positions[1:length(positions)]
    getPositions()
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    #colorNumeric(input$colors, c(0,100))
    colorNumeric("Greens", c(0,100))
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(positions) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  # A comment
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~1000*mag, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = positions)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = c(0,100)
      )
    }
  })
}

shinyApp(ui, server)


















ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)

## Sidebar content
dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)
## Body content
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

getwd()
print(datafestdata.txt$London)
read.delim("datafestdata.txt", header=TRUE)

