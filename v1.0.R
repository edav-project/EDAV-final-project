library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(tigris)
library(zoo)
state=states(cb=T)

# you would need to modify the implementation here, we need column having:
# id: the full name of each state
# region: the abbreviation of each state: like NY for new york
# date: 
# value: the totals
geodata = function (selectedyear){
  data <- read.csv("nics-firearm-background-checks copy.csv") %>%
    select(1,2, 27,28)
  data$month <- as.Date(as.yearmon(data$month))
  
  year = paste0(toString(selectedyear),"-10","-01")
  
  data_new <- data %>%
    filter(data$month==year)
  data_new$state <- tolower(data_new$state)
  data_new <- data_new %>%
    filter(data_new$state!='guam')
  data_new <- data_new %>%
    filter(data_new$state!='mariana islands')
  data_new <- data_new %>%
    filter(data_new$state!='puerto rico')
  data_new <- data_new %>%
    filter(data_new$state!='virgin islands')
  names(data_new)[2] <- 'id'
  names(data_new)[4] <-'region'
  names(data_new)[3] <-'value'
  
  return(data_new)
}


#data_new <- data_new[c(1,3,4)]



ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearSlider",
                  "Please select a year",
                  min = 1998,
                  max = 2019,
                  value = 2019,
                  step = 1)
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput('tim')
    )
  )
  
  #textOutput("temp"),
  
)

#server.r

#df$location <- gsub( " " , "+" , df$location)
server = function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    data_new = geodata(input$yearSlider)
    states_merged_value <- geo_join(state, data_new, "STUSPS", "region")
    bins <- c(0, 1000, 2000, 5000, 10000, 20000, 50000, 100000, Inf)
    pal <- colorBin("YlOrRd", domain=states_merged_value$value,bins = bins)
    
    leaflet()%>%
      addTiles()%>%
      setView( -98.483330, 38.712046, zoom = 4 ) %>%
      addPolygons(
        data = states_merged_value,
        layerId=states_merged_value$id,
        fillColor = ~pal(value),
        weight = 1, 
        highlight=highlightOptions(weight=10, color="blue",bringToFront=TRUE)
      )
  })
  
  output$temp <- renderPrint({
    
    #input$map_shape_click
  })
  
   observeEvent(input$map_shape_click,{
     output$tim <- renderPlot({
       # this is where we would draw the other pictures
       #plot = function(state, year)
       #plot(input$map_shape_click$id,input$yearSlider)
       data_new = geodata(input$yearSlider)
       temp <- data_new %>% filter( data_new$id == input$map_shape_click$id)
       ggplot(data = temp, aes(id, value)) + geom_point()
  })
   })
  
}

shinyApp(ui = ui, server = server)
