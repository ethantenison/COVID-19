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




df$value <- as.numeric(df$value)
df$date <- ymd(df$date)
options(scipen=999)

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
  HTML('<meta name="viewport" content="width=1024">'),
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
                  {background: #d73027; border-color:#d73027}")),
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  fluidRow(
    column(width= 9, 
      div(id = "BoxMap",leafletOutput("map", height = 800
                                      )),
      absolutePanel(
        top = "0%",
        left = "10%",
        right = "50%",
        bottom = "92.5%",
        h1(id="big-heading",strong("COVID-19 Explorer")),
        tags$style(HTML("#big-heading{color: ##d73027;}")),
        h3(id='nums', textOutput("date2", inline = TRUE)),
        h3(id="nums","(",textOutput("num_matching", inline = TRUE),")"),
        tags$style(HTML("#nums{color: ##d73027;}"))
      ),
      plotOutput("linegraph", height = 400)
      ),
    column(width=3,
           selectInput(inputId = "measure", label = h3(strong("Select a Variable: ")), 
                       choices = c("Confirmed Cases", "Deaths"), multiple = FALSE),
           div(
             sliderInput(inputId = "date", h3(strong("Select a Date: ")), min = as.Date("2020-03-10"), 
                         max = as.Date("2020-04-06"), value = as.Date("2020-04-06"), 
                         step = .01,
                         animate = animationOptions(interval = .01)
             ),
             style="font-size:150%; color: #d73027"),
          # htmlOutput("tabletitle"),
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
        dplyr::select(State, value, c_PercentChange, death_rate) %>%
        rename("Confirmed Cases" = value, "% Change Confirmed" = c_PercentChange, "Death Rate" = death_rate)
    }
    
    else if (unique(data()$measure) == "Deaths"){
      plot <- df %>% filter(date %in% input$date) %>% filter(measure %in% input$measure) %>% 
        dplyr::select(State, value, d_PercentChange, death_rate) %>%
        rename("Deaths" = value, "% Change Deaths" = d_PercentChange, "Death Rate" = death_rate)
      
    }
  })
  
  data <- reactive({
    
    plot <- df %>% dplyr::filter(date %in% input$date) %>% dplyr::filter(measure %in% input$measure)
    
  })
  
  data_line <- reactive({
    
    plot <- df %>% dplyr::filter(date <= input$date) %>% dplyr::filter(measure %in% input$measure)
    
    midpoint <- mean(data()$value)
    top <- filter(data(), data()$value >= midpoint)
    top <- unique(top$State)
    plot <- filter(plot, State %in% top)
    
  })
  
  
  colorpal <- reactive({
    
    colorNumeric(palette = "RdYlBu", domain = domain()$value, reverse = TRUE)
  })
  
  
  output$linegraph <- renderPlot({
    
    theme_set(theme_bw())
    ggplot(data = data_line(), aes(x=data_line()$date, y=data_line()$value, group = data_line()$State,color = data_line()$State)) +
      geom_line(size = 1) + labs(color = "States above the mean") +
      theme( panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(colour = "black"),
             text=element_text(face = "bold", size=12),
             plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
             axis.text.y = element_text(hjust = 1.25)) +
      xlab("Date") +
      ylab("") +
      scale_x_date(date_labels="%m/%d",date_breaks  ="1 day")
    
  })
  
  
  output$map <- renderLeaflet({
    leaflet(data = df) %>% addTiles(
      urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
      attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>', 
      options = tileOptions(minZoom = 0, maxZoom = 18)
    ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  
  observe({
    
    
    pal <- colorpal()
    
    if (unique(data()$measure) == "Confirmed Cases"){
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      addCircles(lng =  ~ long,lat =  ~ lat, radius = ~data()$value*10, weight = 1, color = "#777777",
                 fillColor = ~pal(data()$value), fillOpacity = 0.6, popup = ~paste0(
                   "<h4/><b>",data()$State,"</b><h5/>",
                   "<h5/>",data()$measure,": ", data()$value,
                   "<h5/> Percentage Change in Confirmed Cases: ", data()$c_PercentChange,"%",
                   "<h5/>","Death Rate: ", data()$death_rate)
      )
    }
    else if (unique(data()$measure) == "Deaths"){
      leafletProxy("map", data = data()) %>%
        clearShapes() %>%
        addCircles(lng =  ~ long,lat =  ~ lat, radius = ~data()$value*300, weight = 1, color = "#777777",
                   fillColor = ~pal(data()$value), fillOpacity = 0.6, popup = ~paste0(
                     "<h5/><b>",data()$State,"</b><h5/>",
                     "<h5/>",data()$measure,": ", data()$value,
                     "<h5/> Percentage Change in Deaths: ", data()$d_PercentChange,"%",
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
  
  
  output$date2 <-renderText({
    
    paste0(format(as.Date(unique(data()$date))))
    
    
  })
  
  output$num_matching <-renderText({
    
    paste0(format(sum(data()$value), big.mark = ",")," ", unique(data()$measure))
    
    
    })
  
  # output$tabletitle <- renderText({ 
  #   paste0("<h3><b> Measure: ", input$measure,"</b>" )
  # })
  
  output$table <- renderDataTable(datatable2(), options = list(searching = FALSE))
  
  
}

shinyApp(ui, server)