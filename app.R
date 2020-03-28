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
library(leaflet.providers)



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
  tags$style(HTML(".js-irs-0 .irs-single,
                  .js-irs-0 .irs-bar-edge,
                  .js-irs-0 .irs-bar,
                  .js-irs-0 .irs-grid-pol
                  {background: #a65c85ff; border-color:#a65c85ff}")),
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  fluidRow(
    column(width= 9, 
      div(id = "BoxMap",leafletOutput("map", height = 1000)),
      absolutePanel(
        top = "0%",
        left = "10%",
        right = "50%",
        bottom = "92.5%",
        h1(id="big-heading",strong("COVID-19 Explorer")),
        tags$style(HTML("#big-heading{color: #000000;}")),
        h3(id="nums","(",textOutput("num_matching", inline = TRUE),")"),
        tags$style(HTML("#nums{color: #000000;}"))
      ),
      absolutePanel(
        top = "0%",
        left = "65%",
        right = "10%",
        bottom = "92.5%",
          div(
            sliderInput(inputId = "date", "Select a Date: ", min = as.Date("2020-03-10"), 
                        max = as.Date("2020-03-27"), value = as.Date("2020-03-10"), 
                        step = .1,
                        animate = animationOptions(interval = .1)
            ),
            selectInput(inputId = "measure", label = "Select a Variable: ", 
                        choices = c("Confirmed Cases", "Deaths"), multiple = FALSE),
            style="font-size:125%; color: black")
      )
      ),
    column(width=3,
           htmlOutput("tabletitle"),
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
  
  datatable2 <- reactive({
    
    if (unique(data()$measure) == "Confirmed Cases"){
      plot <- df %>% filter(date %in% input$date) %>% filter(measure %in% input$measure) %>% 
        dplyr::select(State, value, c_PercentChange ) %>%
        rename("Confirmed Cases" = value, "Percent Change" = c_PercentChange)
    }
    
    else if (unique(data()$measure) == "Deaths"){
      plot <- df %>% filter(date %in% input$date) %>% filter(measure %in% input$measure) %>% 
        dplyr::select(State, value, d_PercentChange ) %>%
        rename("Deaths" = value, "Percent Change" = d_PercentChange)
      
    }
  })
  
  data <- reactive({
    
    plot <- df %>% dplyr::filter(date %in% input$date) %>% dplyr::filter(measure %in% input$measure)
    
  })
  
  
  colorpal <- reactive({
    
    colorNumeric(palette = "plasma", domain = domain()$value)
  })
  
  
  output$map <- renderLeaflet({
    leaflet(data = df) %>% addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  
  observe({
    
    pal <- colorpal()
    
    if (unique(data()$measure) == "Confirmed Cases"){
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addCircles(lng =  ~ long,lat =  ~ lat, radius = ~data()$value*20, weight = 1, color = "#777777",
                 fillColor = ~pal(data()$value), fillOpacity = 0.6, popup = ~paste0(
                   "<h4/><b>",data()$State,"</b><h5/>",
                   "<h5/>",data()$measure,": ", data()$value,
                   "<h5/>","Death Rate: ", data()$death_rate)
      )
  }
  else if (unique(data()$measure) == "Deaths"){
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addCircles(lng =  ~ long,lat =  ~ lat, radius = ~data()$value*1000, weight = 1, color = "#777777",
                 fillColor = ~pal(data()$value), fillOpacity = 0.6, popup = ~paste0(
                   "<h5/><b>",data()$State,"</b><h5/>",
                   "<h5/>",data()$measure,": ", data()$value,
                   "<h5/>","Death Rate: ", data()$death_rate)
      )
  }
  })
  
  
  observe({
    proxy <- leafletProxy("map", data = domain())
    
    pal <- colorpal()
    proxy %>% clearControls() %>% addLegend(position = "bottomright",
                          pal = pal, values = ~value, title = unique(data()$measure))
      
    
  })
  
  output$num_matching <-renderText({
    
    paste0(format(sum(data()$value), big.mark = ",")," ", unique(data()$measure))
    
    
    })
  
  output$tabletitle <- renderText({ 
    paste0("<h1><b> Measure: ", input$measure,"</b>" )
  })
  
  output$table <- renderDataTable(datatable2(), options = list(searching = FALSE))
  
  
}

shinyApp(ui, server)