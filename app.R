library(spData)
library(tidyverse)
library(leaflet)

load(url("https://github.com/freezekent/milneigh/blob/main/siprineigh.RData?raw=true"))

ui <- fluidPage(

    # Application title
    titlePanel("Military Expenditures in the Neighborhood"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("year",
                        "Pick Year", c("1992" = 1992,
                                       "1993" = 1993,
                                       "1994" = 1994,
                                       "1995" = 1995,
                                       "1996" = 1996,
                                       "1997" = 1997,
                                       "1998" = 1998,
                                       "1999" = 1999,
                                       "2000" = 2000,
                                       "2001" = 2001,
                                       "2002" = 2002,
                                       "2003" = 2003,
                                       "2004" = 2004,
                                       "2005" = 2005,
                                       "2006" = 2006,
                                       "2007" = 2007,
                                       "2008" = 2008,
                                       "2009" = 2009,
                                       "2010" = 2010,
                                       "2011" = 2011,
                                       "2012" = 2012,
                                       "2013" = 2013,
                                       "2014" = 2014,
                                       "2015" = 2015,
                                       "2016" = 2016,
                                       "2017" = 2017,
                                       "2018" = 2018,
                                       "2019" = 2019)),
            
            radioButtons("ind", "Factor to Display", 
                         c("Military Spending (Millions of Constant 2018 USD) " = "milexp",
                         "Ratio of Military Spending to Land Neighbors" = "millrat",
                         "Ratio to Military Spending to Land and Maritime Neighbors" = "millmrat"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("worldmap", height=1000)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$worldmap <- renderLeaflet({
        
        sipsmall <- siprineigh %>%
            filter(year==input$year) %>%
            rename(iso_a2 = isocc) 
        
        worldsip <- world %>%
            merge(sipsmall, by = 'iso_a2')
        
        if (input$ind == "milexp") {
            worldsip$ind <- worldsip$milexp
        } else if (input$ind == "millrat") {
            worldsip$ind <- worldsip$millrat
        } else {
            worldsip$ind <- worldsip$millmrat
        }
        
        maplbl = paste(round(worldsip$ind, 3))
        
        mypalette <- colorQuantile(palette="plasma", domain=worldsip$ind, na.color="transparent", n = 5)
        
        mypal_colors <- unique(mypalette(sort(worldsip$ind)))
        mypal_labs <- round(quantile(worldsip$ind, seq(0, 1, 0.2), na.rm=T), 2)
        mypal_labs <- paste(lag(mypal_labs), mypal_labs, sep = " - ")[-1]
        
        leaflet(worldsip) %>%
            addTiles() %>%
            setView(lat=10, lng=0, zoom=2) %>%
            addPolygons( fillColor = ~mypalette(ind), 
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         highlightOptions = highlightOptions(
                             weight = 3,
                             color = "#666",
                             fillOpacity = 1,
                             bringToFront = TRUE),
                         label = maplbl) %>%
            addLegend(colors = mypal_colors, labels = mypal_labs, opacity = 0.5, title = NULL, position = "bottomright")
    
    

    
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
