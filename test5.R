library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(sf)  
library(leaflet)
library(tmap)

data(World)                   # The first column is the 3 character name, just like in the olympics dataset
names(World)[1] <- "country"
load("covid_19_vaccine_world_progress.RData")

ui <- dashboardPage(
  dashboardHeader(title = "Covid-19 Vaccine"),                   # Title of the dashboard
  dashboardSidebar(
    checkboxGroupInput("vaccines", h4("Vaccines"),               # Element 2: season selection
                       choices = list("Pfizer/BioNTech" = "Pfizer/BioNTech", 
                                      "Sputnik V" = "Sputnik V",
                                      "Oxford/AstraZeneca" = "Oxford/AstraZeneca",
                                      "Moderna" = "Moderna"),
                       selected = c("Pfizer/BioNTech", "Sputnik V", "Oxford/AstraZeneca", "Moderna")),
    numericInput("max_country", h4("Max number countries"),  # Element 4: nb of countries displayed
                 min = 2, max = 12, step = 1, value = 6)
  ),
  
  ####### NEW ELEMENTS BELOW !!!!!
  dashboardBody(
    fluidRow(
        tabBox(
            title = "Results", height = "870px", width = 12,
            tabPanel("World Map",
                     valueBoxOutput("box2", width = 3),
                     valueBoxOutput("box1", width = 5),
                     infoBoxOutput("box3"),
                     leafletOutput("plot", height = 450)
                     ),
            tabPanel("Table", 
                     DT::dataTableOutput("table_1")
                    ),
            tabPanel("Graph", 
                     plotOutput("plot", height = 300)
        )
      )
    )   
  )
)

server <- function(input, output){
  ####### NEW ELEMENTS BELOW !!!!!
  data <- reactive({                # Creates the dynamic data
    covid_19_vaccine_world_progress %>%                  # Filter years, seasons & gender
      filter(vaccines %in% input$vaccines) 
    
  })
  
  filter_country <- reactive({            # Performs a filter to limit countries
    data() %>% 
      group_by(country) %>%           # Analysis by country
      summarise(total_vaccinations = n()) %>%  # Sum number of medals
      arrange(desc(total_vaccinations)) %>%    # Order
      head(input$max_country) %>%     # Keep only max_country
      pull(country)                   # Keep only the Country column/variable
  }) 
  
  
  output$table_1 <- DT::renderDataTable(data()) # Create the output object!

  output$plot <- renderPlot({
    data() %>% filter(country %in% filter_country()) %>%
      ggplot(aes(x = vaccines, fill = country)) + geom_bar()
  }, height = 300) 
  
  output$box1 <- renderValueBox({
    covid_19_vaccine_world_progress <- data()
    valueBox(
      value = sum(covid_19_vaccine_world_progress$total_vaccinations), 
      subtitle =  "World Total Vaccinations",
      color = "blue",
      width = 4  # Width in Bootstrap mode: needs a column()!
    )
  })
  
  output$box2 <- renderInfoBox({
    covid_19_vaccine_world_progress <- data()
    valueBox(
      value = sum(covid_19_vaccine_world_progress$people_fully_vaccinated),
      subtitle = "People Fully Vaccinated", 
      color = "blue",
      width = 4
    )
  })
  
  output$box3 <- renderInfoBox({
    covid_19_vaccine_world_progress <- data()
      valueBox(
        value = unique(covid_19_vaccine_world_progress$country),
        subtitle = "Total Countries", 
        color = "blue",
        width = 4
    )
  })
  output$plot <- renderLeaflet({
        data_map <- data() %>%
            group_by(Country) %>%
            count() %>%                                                     # Counts nb instances, like n()
            left_join(World %>% select(Country, geometry))                  # Joining the geometry column
        palet <- colorNumeric("Blues",                                      # Blue palette
                          domain = data_map %>% pull(n)#,                    # Domain of labels: nb medals
        )
                
        labels <- sprintf(                                                  # Below we define the labels
            "<strong>%s</strong><br/>%g Medals",                            # Adding text to label
            data_map$Country,                                               # We show the country name...
            data_map$n                                                      # ... and the nb medals
        ) %>% lapply(htmltools::HTML)                                       # Embedded all into html language
                   
        data_map %>% 
            data.frame() %>%                                # Turn into dataframe (technical)
            sf::st_sf() %>%                                 # Format in sf
            st_transform("+init=epsg:4326") %>%             # Convert in particular coordinate reference 
            leaflet() %>%                                   # Call leaflet
            addPolygons(fillColor = ~palet(n),        # Create the map (colored polygons)
                        weight = 2,                         # Width of separation line
                        opacity = 1,                        # Opacity of separation line
                        color = "white",                    # Color of separation line
                        dashArray = "3",                    # Dash size of separation line
                        fillOpacity = 0.7,                  # Opacity of polygon colors
                        highlight = highlightOptions(       # 5 lines below control the cursor impact
                            weight = 2,                       # Width of line
                            color = "#CBCBCB",                # Color of line
                            dashArray = "",                   # No dash
                            fillOpacity = 0.7,                # Opacity
                            bringToFront = TRUE),
                        label = labels,                     # LABEL! Defined above!
                        labelOptions = labelOptions(        # Label options below...
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
            ) %>%
            addLegend(pal = palet,                    # Legend: comes from palet colors defined above
                      values = ~n,                    # Values come from lifeExp variable
                      opacity = 0.7,                  # Opacity of legend
                      title = "Map Legend",           # Title of legend
                      position = "bottomright")       # Position of legend
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
