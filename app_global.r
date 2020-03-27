library(shiny)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(lubridate)


df <- read.csv("covid_global.csv")
df$confirmed <- as.numeric(df$confirmed)
df$date <- ymd(df$date)

#dfsum <- df %>% group_by(Date) %>% summarize(Total_Cases = sum(confirmed))

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = "0%",
    left = "10%",
    right = "50%",
    bottom = "92.5%",
    h1("COVID-19 Confirmed Cases")#,
    #h3("(",textOutput("num_matching", inline = TRUE),"cases)")
    ),
  absolutePanel(top = 10, right = 10,
                sliderInput(inputId = "date", "Select a Date", min = as.Date("2020-03-10"), 
                            max = as.Date("2020-03-25"), value = as.Date("2020-03-10"), 
                            step = .1,
                            animate = animationOptions(interval = .1)
                ),
                checkboxInput("legend", "Show legend", TRUE)  
                )
)


# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #


server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  data <- reactive({
    
    plot <- df %>% dplyr::filter(date %in% input$date)
    
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(palette = "plasma", domain = df$confirmed)
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = df) %>% addTiles() 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addCircles(lng =  ~ long,lat =  ~ lat, radius = ~data()$confirmed*30, weight = 1, color = "#777777",
                 fillColor = ~pal(data()$confirmed), fillOpacity = 0.7, popup = ~paste(data()$confirmed)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = df)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~confirmed
      )
    }
  })
  
  #output$num_matching <-renderText({format(sum(data()$confirmed), big.mark = ",")})
  

}

shinyApp(ui, server)