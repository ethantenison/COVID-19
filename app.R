library(shiny)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(lubridate)
library(shinydashboard)
library(V8)
library(rintrojs)
library(shinyjs)
library(dplyr)
library(readr)
library(tidyr)
library(leaflet.extras)
library(shinyWidgets)


df <- read.csv("states.csv")
df$value <- as.numeric(df$value)
df$date <- ymd(df$date)


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
#washington state: 4.5%, New York 1.1%, 

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("www/css/bootstrap.css")),
  tags$head(includeScript("www/js/google_analytics.js")),
  tags$head(tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.1.0/css/all.css",
                      integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt", crossorigin="anonymous")),
  tags$head(tags$script(src = "www/js/wordwrap.js")),
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  fluidRow(
    column(width= 9, 
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(
        top = "0%",
        left = "10%",
        right = "50%",
        bottom = "92.5%",
        h1("COVID-19 Confirmed Cases"),
        h3("(",textOutput("num_matching", inline = TRUE),"cases)")
      ),
      absolutePanel(top = 10, right = 8,
                    sliderInput(inputId = "date", "Select a Date", min = as.Date("2020-03-10"), 
                                max = as.Date("2020-03-27"), value = as.Date("2020-03-10"), 
                                step = .1,
                                animate = animationOptions(interval = .1)
                    ),
                    selectInput(inputId = "measure", label = "Select a Variable", 
                                choices = c("confirmed", "deaths"), multiple = FALSE),
                    
                    checkboxInput("legend", "Show legend", TRUE) 
      )
      ),
    column(width=3,
           dataTableOutput("table"))
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
  
  # Reactive expression to subset based on the data
  
  domain <- reactive({
    
    dom <- df %>% dplyr::filter(measure %in% input$measure)
    
  })
  
  datatable <- reactive({
    plot <- df %>% filter(date %in% input$date) %>% filter(measure %in% input$measure) %>% dplyr::select(province_state, measure, value)
  })
  
  data <- reactive({
    
    plot <- df %>% dplyr::filter(date %in% input$date) %>% dplyr::filter(measure %in% input$measure)
    
  })
  
  
  colorpal <- reactive({
    
    colorNumeric(palette = "plasma", domain = domain()$value)
  })
  
  
  output$map <- renderLeaflet({
    leaflet(data = df) %>% addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  
  observe({
    
    pal <- colorpal()
    
    if (unique(data()$measure) == "confirmed"){
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addCircles(lng =  ~ long,lat =  ~ lat, radius = ~data()$value*20, weight = 1, color = "#777777",
                 fillColor = ~pal(data()$value), fillOpacity = 0.6, popup = ~paste0(
                   "<h4/><b>",data()$province_state,"</b><h5/>",
                   "<h5/>",data()$measure,": ", data()$value,
                   "<h5/>","Death Rate: ", data()$death_rate)
      )
  }
  else if (unique(data()$measure) == "deaths"){
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addCircles(lng =  ~ long,lat =  ~ lat, radius = ~data()$value*1000, weight = 1, color = "#777777",
                 fillColor = ~pal(data()$value), fillOpacity = 0.6, popup = ~paste0(
                   "<h5/><b>",data()$province_state,"</b><h5/>",
                   "<h5/>",data()$measure,": ", data()$value,
                   "<h5/>","Death Rate: ", data()$death_rate)
      )
  }
  })
  
  
  observe({
    proxy <- leafletProxy("map", data = domain())
    
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~value
      )
    }
  })
  
  output$num_matching <-renderText({format(sum(data()$value), big.mark = ",")})
  
  output$table <- renderDataTable(datatable())
  
  
}

shinyApp(ui, server)