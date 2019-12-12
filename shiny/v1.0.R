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
library(tidyverse)
library(GGally)
library(parcoords)
library(r2d3)
state=states(cb=T)

df_violence_sum <- read_csv("dataset/gun_violence_sum.csv")
df_ABBstate <- read_csv("dataset/ABBstate.csv")
df_population <- read_csv("dataset/clean_population_by_state.csv")

df_gun_violence <- read_csv("dataset/gun-violence-data_01-2013_03-2018.csv")
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

###########################
## pre data processing for timeseries+pcp
gun_vio=read.csv("dataset/clean_gun_violence_2013.3-2018.3.csv")
gun_vio["year"]=str_sub( as.character(gun_vio$date),1,4)
gun_vio<-gun_vio[which(as.numeric(as.character(gun_vio$year))>2013)  ,]
back_check=read.csv("dataset/nics-firearm-background-checks copy.csv")
GDP<-read.csv("dataset/GDP_states.csv")
GDP_sub<-GDP %>% select(-c(1,2,4,5,6,7,8,9))

population_tp=read.csv("dataset/clean_population_by_state.csv")
population<-select(population_tp,2,3,4)

gun_vio$date<-as.Date(gun_vio$date)
gun_vio$state<-as.character(gun_vio$state)

back_check$month<-as.character(back_check$month)
back_check["year"]=str_sub(back_check$month,1,4)

gun_vio["year"]<-format(as.Date(gun_vio$date, format="%d/%m/%Y"),"%Y")
gun_vio["month"]<-format(as.Date(gun_vio$date, format="%d/%m/%Y"),"%m")

###########################
## timeseris transform data
gun_vio$date<-as.Date(gun_vio$date)
gun_vio_tp<-subset(gun_vio,select=c("date","state","n_killed","n_injured","month","year"))
# population_sub<-population[which(population$State==state),]
gun_vio_tp$n_killed<-as.numeric(as.character(gun_vio_tp$n_killed))
gun_vio_tp$n_injured<-as.numeric(as.character(gun_vio_tp$n_injured))
gun_vio_tp["n_sum"]<-gun_vio_tp$n_killed+gun_vio_tp$n_injured
gun_vio_month<-gun_vio_tp%>%
  group_by(month,year,state)%>%
  summarize(n_killed=sum(n_killed),n_injured=sum(n_injured),n_sum=sum(n_sum))
gun_vio_month<-select(gun_vio_month,1,2,3,6)
gun_vio_month<-as.data.frame(gun_vio_month)
gun_vio_month["date_by_month"]=     str_c(as.character(gun_vio_month$year),"-",as.character(gun_vio_month$month),"-15")
gun_vio_month$date_by_month<-as.Date( gun_vio_month$date_by_month)

#############################
## function to plot time series
TimePlot<-function(state)
{
  gun_vio_month_state<-gun_vio_month[which(gun_vio_month$state==state),]
  population_sub<-population[which(population$state==state),]
  
  gun_vio_avg=merge(gun_vio_month_state,population_sub,by.x="year",by.y="year")
  gun_vio_avg["gun_violence_avg"]<-gun_vio_avg$n_sum/(gun_vio_avg$population*1.0)*1000000.0
  
  ggplot(gun_vio_avg)+
    geom_line(aes(x=date_by_month,y=gun_violence_avg))+
    ggtitle("Gun Violence from Jan,2014 to Mar,2018")
}


###########################
## pcp transform data
back_check<-back_check[which(  as.numeric(as.character(back_check$year))>2013), ]
back_check_state<-back_check%>%
  group_by(year,state)%>%
  summarize(total_year=sum(totals))

gun_vio_state<-gun_vio_month%>%filter(!is.na(gun_vio_month$n_sum))%>%
  group_by(year,state)%>%
  summarize(n_sum_state=sum(n_sum))

back_check_state<-as.data.frame(back_check_state)
gun_vio_state<-as.data.frame(gun_vio_state)


###########################
## function to plot pcp
Parcoord<-function(year)
{
  year<-as.character(year)
  population_sub<-population[which(population$year==year),]
  back_check_state_year<-back_check_state[which(back_check_state$year==year),]
  back_check_state_year_avg<-merge(population_sub,back_check_state_year,by.x="state",by.y="state")
  
  back_check_state_year_avg["checking_avg"]<-back_check_state_year_avg$total_year/back_check_state_year_avg$population*100000.0
  
  gun_vio_state_year<-gun_vio_state[which(gun_vio_state$year==year),]
  gun_vio_state_year_avg<-merge(gun_vio_state_year,population_sub,by.x="state",by.y="state")
  gun_vio_state_year_avg["gun_violence_avg"]<-gun_vio_state_year_avg$n_sum_state/(gun_vio_state_year_avg$population*1.0)*1000000.0
  gun_vio_state_year_avg<-select(gun_vio_state_year_avg,1,6)
  check_vio<-merge(back_check_state_year_avg, gun_vio_state_year_avg, by.x="state", by.y="state")
  GDP_year<-subset(GDP,select=c("GeoName",str_c("X",year)))
  
  colnames(GDP_year)<-c("state","GDP_total")
  
  GDP_avg=merge(GDP_year,population_sub,by.x="state",by.y="state")
  
  GDP_avg["GDP_avg"]=GDP_avg$GDP_total*1000000/GDP_avg$population
  
  check_vio_gdp<-merge(check_vio, GDP_avg, by.x="state", by.y="state")
  check_vio_gdp<-select(check_vio_gdp,1,2,6,7,11)
  
  
  colnames(check_vio_gdp)<-c("state","year","checking_avg","gun_violence_avg","GDP_avg")
  
  parcoords(check_vio_gdp[c("state","checking_avg","gun_violence_avg","GDP_avg")],rownames = FALSE,reorder = TRUE, brushMode="1D",alpha = .3, color = list(colorScale = "scaleOrdinal",colorBy = "state", colorScheme = "schemeCategory10"), withD3 = TRUE)
  # ggpairs(check_vio_uem[c("checking_totals","gun_violence_death","GDP_avg")])
}


###########################
## shiny begins

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearSlider",
                  "Please select a year",
                  min = 2014,
                  max = 2018,
                  value = 2018,
                  step = 1)
      
     
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput('heatmap'),
      plotOutput("timeseries"),
      parcoordsOutput("pcp")
    )
  )
  
  #textOutput("temp"),
  
)

server = function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    main_map_plotter(input$yearSlider,df_violence_sum,df_ABBstate,df_population)
  })
  
  #heatmap <- reactiveValues(state = "New York",year = 2018)
  heatmap <- reactiveValues(state = "New York")
  observeEvent(input$map_shape_click,
               {heatmap$state <- input$map_shape_click$id
               #heatmap$year <- input$yearSlider
               })
  
  # plot time series
  output$timeseries <- renderPlot({
    TimePlot(heatmap$state)
  })
  
  # plot heatmap
  output$heatmap <- renderPlot({
    heat_map_plotter(heatmap$state,input$yearSlider,df_gun_violence)
    #input$map_shape_click
  })
  
  # plot pcp
  output$pcp <- renderParcoords({
    Parcoord(input$yearSlider)
  })

  
}

shinyApp(ui = ui, server = server)
