library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(tigris)
library(zoo)
library(readr)

state=states(cb=T)
df_violence_sum <- read_csv("dataset/gun_violence_sum.csv")
df_ABBstate <- read_csv("dataset/ABBstate.csv")
df_population <- read_csv("dataset/clean_population_by_state.csv")

main_map_plotter = function(input_year,data_new,bind_data,population){
  
  data_new <- filter(data_new, year == input_year)
  population <- filter(population, year == input_year)
  
  data_new <- merge(data_new,bind_data,by="state")
  data_new <- merge(data_new,population,by="state")
  data_new$rate <- data_new$total_TOLL/data_new$population*100000
  
  states_merged_value <- geo_join(state, data_new, "STUSPS", "ABBstate")
  
  bins <- c(0, 2, 4, 6,8,10,15,Inf)
  pal <- colorBin("YlOrRd", domain=states_merged_value$rate,bins = bins)
  
  m <- leaflet() %>%
    addTiles() %>%
    setView( -98.483330, 38.712046, zoom = 4 ) %>%
    addPolygons(
      data = states_merged_value,
      
      fillColor = ~pal(rate),
      weight = 1, 
      highlight=highlightOptions(weight=10, color="blue",bringToFront=TRUE)
    )
  m
  
}

#main_map_plotter("2013")

heat_map_plotter = function(input_state,input_year){
  
  data <- read_csv("../dataset/gun-violence-data_01-2013_03-2018.csv")
  names(data)[15] <- "lat"
  names(data)[17] <- "lon"
  data$date <- as.character(data$date)
  data <- as.data.frame(data)
  data_new <- data 
  data_new <- as.data.frame(data_new)
  data_new <- data_new %>%
    filter(state==input_state) #select state
  data_new$date <- as.character(data_new$date)
  data_new$date<-as.Date(data_new$date)
  data_new_d<-data_new[which( format(as.Date(data_new$date, format="%d/%m/%Y"),"%Y")==input_year), ] #select year
  
  qmplot(lon,lat,data=data_new_d,colour=I("red"),size = I(0.8),alpha=0.3,main="Gun violence Map")+
    theme(legend.position="none")
}

#heat_map_plotter("California","2013")

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearSlider",
                  "Please select a year",
                  min = 2014,
                  max = 2018,
                  value = 2018,
                  step = 1),
      plotOutput('heatmap')
    ),
    mainPanel(
      leafletOutput("mainmap")#,
     # plotOutput('tim')
    )
  )
  
  #textOutput("temp"),
  
)

server = function(input, output, session) {
  
  output$mainmap <- renderLeaflet({main_map_plotter(input$yearSlider,df_violence_sum,df_ABBstate,df_population)})
  
  #output$heatmap <- renderPrint({heat_map_plotter(input$map_shape_click,input$year) })
  
  observeEvent(input$map_shape_click, {print(input$map_shape_click)})
}





shinyApp(ui = ui, server = server)
