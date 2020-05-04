library(shinydashboard)
require(rlang)
require(shiny)
require(plotly)
require(scales)
require(plyr)
require(rpivotTable)
require(htmlwidgets)
require(ggplot2)
require(stats)

load(url("https://github.com/garza4213/Projects/raw/master/Well_App_Data.RData"))
# Define UI for application that draws a histogram
ui <- 
    fluidPage(
    sidebarLayout(
        sidebarPanel(selectInput("Operator","Well Operator",choices = wells2$Operator,
                                 selected = c("EOG","Chevron","BP"), multiple= TRUE),
                     sliderInput("Well_Start","Well Start",min = as.Date("2008-01-01","%Y-%M-%d"),
                                 max = as.Date("2019-12-01","%Y-%M-%d"), 
                                 value =  c(as.Date("2008-01-01","%Y-%M-%d"),as.Date("2019-12-01","%Y-%M-%d")),
                                 timeFormat="%Y-%m-%d")),
        mainPanel(
            
            tabsetPanel(
                tabPanel("Permian & Eagleford Oil Well Basin",plotlyOutput(outputId =  "map"),plotlyOutput(outputId = "well_start")),
                tabPanel("Pivot Table",  rpivotTableOutput(outputId = "table"))))
        ))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    data_sub <-reactive({filter(wells2,wells2$Operator == input$Operator & wells2$well_start>=input$Well_Start[1] &
                                    wells2$well_start<=input$Well_Start[2])}) 
    output$map<-renderPlotly({
        map <- plot_ly( data = wells2,
                        lat = ~Latitude,
                        lon = ~Longitude,
                        type = "scattermapbox",
                        mode='marker',
                        hovertext = ~Operator,
                        color = ~ Operator)
        map <- map  %>%
            layout(mapbox= list(style = "white-bg",
                                zoom = 5,
                                center = list(lon = -103 ,lat= 32),
                                layers = list(list(below = 'traces',
                                                   sourcetype = "raster",
                                                   source = list("https://basemap.nationalmap.gov/
              arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}"))))
            )})
    
    output$map<-renderPlotly({
        map <- plot_ly( data = data_sub(),
                        lat = ~Latitude,
                        lon = ~Longitude,
                        type = "scattermapbox",
                        mode='marker',
                        hovertext = ~Operator,
                        color = ~ Operator)
       
        map <- map  %>%
            layout(mapbox= list(style = "white-bg",
                                zoom = 5,
                                center = list(lon = -103 ,lat= 32),
                                layers = list(list(below = 'traces',
                                                   sourcetype = "raster",
                                                   source = list("https://basemap.nationalmap.gov/
              arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}"))))
            )})
    
    
    output$well_start<-renderPlotly( {p<-ggplot(data = data_sub(),aes(x = well_start,color=Operator))+
        geom_line(stat = 'count')+
        ggtitle('New Well Development Time-Series')+
        xlab('Date')+ylab('Well Count')
    
    ggplotly(p, tooltip = c("Operator","y")) 
    })
    output$table<-renderRpivotTable(rpivotTable(data_sub(),
                                                rows = "Basin",cols = "Operator",aggregatorName = "Count",
                                                vals = "wellID"))
    
}

shinyApp(ui = ui, server = server)
