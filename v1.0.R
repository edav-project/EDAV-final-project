library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(tigris)
library(zoo)
library(readr)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(stringr)
state=states(cb=T)

df_violence_sum <- read_csv("dataset/gun_violence_sum.csv")
df_ABBstate <- read_csv("dataset/ABBstate.csv")
df_population <- read_csv("dataset/clean_population_by_state.csv")

df_gun_violence <- read_csv("../dataset/gun-violence-data_01-2013_03-2018.csv")
names(df_gun_violence)[15] <- "lat"
names(df_gun_violence)[17] <- "lon"
df_gun_violence$date <- as.character(df_gun_violence$date)

main_map_plotter <- function(input_year,data_new,bind_data,population){
    data_new <- filter(data_new, year == input_year)
    population <- filter(population, year == input_year)
    
    data_new <- merge(data_new,bind_data,by="state")
    data_new <- merge(data_new,population,by="state")
    data_new$rate <- data_new$total_TOLL/data_new$population*100000
    
    states_merged_value <- geo_join(state, data_new, "STUSPS", "ABBstate")
    
    bins <- c(0, 2, 4, 6,8,10,15,Inf)
    pal <- colorBin("YlOrRd", domain=states_merged_value$rate,bins = bins)
    
    state_popup <- paste0("<strong>State: </strong>",
                         states_merged_value$state,
                         "<br><strong>Incidents per 100,000 people </strong>",
                         states_merged_value$rate) %>% lapply(htmltools::HTML)
    leaflet()%>%
    addTiles()%>%
    setView( -98.483330, 38.712046, zoom = 4 ) %>%
    addPolygons(
      data = states_merged_value,
      layerId=states_merged_value$state,
      fillColor = ~pal(rate),
      weight = 1, 
      color = "white",
      label = state_popup,
      highlight=highlightOptions(weight=10, color="grey",bringToFront=TRUE)
     )#%>%
      #addLegend(pal = pal, values = ~rate,
      #          opacity = 0.7, title = "Incidents per 100k people",
      #          position = "bottomright")
}

heat_map_plotter <- function(input_state,input_year,data){
  
  
  data_new <- data %>%
    filter(state==input_state) #select state
  data_new$date <- as.character(data_new$date)
  data_new$date<-as.Date(data_new$date)
  data_new["year"]=str_sub( as.character(data_new$date),1,4)
  data_new <- data_new %>%
    filter(year==input_year)
  qmplot(lon,lat,data=data_new,colour=I("red"),zoom=8,size = I(0.8),maptype="toner",alpha=0.3,main="Gun violence Map")+
    theme(legend.position="none")
}

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearSlider",
                  "Please select a year",
                  min = 2014,
                  max = 2018,
                  value = 2018,
                  step = 1),
      
     
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput('heatmap')
    )
  )
  
  #textOutput("temp"),
  
)

server = function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    main_map_plotter(input$yearSlider,df_violence_sum,df_ABBstate,df_population)
  })
  
  heatmap <- reactiveValues(state = "New York",year = 2018)
  observeEvent(input$map_shape_click,
               {heatmap$state <- input$map_shape_click$id
               heatmap$year <- input$yearSlider})
  
  output$heatmap <- renderPlot({
    heat_map_plotter(heatmap$state,heatmap$year,df_gun_violence)
    #input$map_shape_click
  })
  
   observeEvent(input$map_shape_click,{
     print(input$map_shape_click)
     print(class(input$yearSlider))})
  
}

shinyApp(ui = ui, server = server)
