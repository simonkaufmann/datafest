library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)

csvdata <- read.csv('data_fest.csv')

date <- csvdata$EndDate
london <- csvdata$LondonA
midlands <- csvdata$MidlandsA
north <- csvdata$NorthA
wales <- csvdata$WalesA
south <- csvdata$RestOfSouthA
scotland <- csvdata$ScotlandA

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("Date", "Time:", min = as.Date("2019-09-01", "%Y-%m-%d"), max = as.Date("2020-05-25", "%Y-%m-%d"),value=as.Date("2016-12-01"), timeFormat="%Y-%m-%d"),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

# Scotland, London, Wales, North England, Midlands, South
positions <- data.frame(lat = c(56.8648, 51.467339, 52.499, 54.6208, 53.16275, 51.013329), long = c(-4.388, -0.110214, -3.8987, -2.311, -1.2526, -2.80393), mag = c(40, 100, 50, 45, 33, 29))

getData <- function(selectedDate) {
  print("selected date")
  print(selectedDate)
  for (i in 2:length(date)) {
    
    end <- length(date)
    positions$mag <- c(scotland[end], london[end], wales[end], north[end], midlands[end], south[end])
    
    if (selectedDate < as.Date(date[i])) {
      print("i")
      print(i)
      print(date[i])
      positions$mag <- c(scotland[i-1], london[i-1], wales[i-1], north[i-1], midlands[i-1], south[i-1])
      break 
    }
  }
  print(positions$mag)
  return(positions)
}

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    getData(input$Date)
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

