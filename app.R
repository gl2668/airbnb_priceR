#
# This is an Airbnb web application. You can run the application by clicking
# the 'Run App' button above.
#
#
#

library(caret)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(shinythemes)

# Load File
listings <- readRDS("shiny_listings.rds")
tube <- readRDS("london_tube.rds")

# Define UI for the Airbnb application
ui <- fluidPage(
    
    # Set Shiny Theme
    theme = shinytheme("united"),

    # Application title
    titlePanel("Airbnb PriceR: Airbnb Analytics", windowTitle = "Airbnb Data for Hosts and Guests"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "neighbourhood", 
                        label = "Neighbourhood",
                        choices = c("Barking and Dagenham", "Barnet", "Bexley",
                                    "Brent", "Bromley", "Camden", "City of London",
                                    "Croydon", "Ealing", "Enfield", "Greenwich",
                                    "Harrow", "Hackney", "Hammersmith and Fulham",
                                    "Haringey", "Havering", "Hillingdon", "Hounslow",
                                    "Islington", "Kensington and Chelsea", "Kingston upon Thames",
                                    "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge",
                                    "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets",
                                    "Waltham Forest", "Wandsworth", "Westminster"
                                    )),
            sliderInput("rooms",
                        "Number of Rooms",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("accommodates",
                        "Number of Guests",
                        min = 1,
                        max = 10,
                        value = 2),
            selectInput(inputId = "room_types_input", 
                        label = "Type of Lodging",
                        choices = c("All types",
                                    "Entire home/apt",
                                    "Private room",
                                    "Shared room",
                                    "Hotel room"
                        ))
        ),

        # Output of Shiny pp
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Listings",
                                 h2("Explore Airbnb listings in London"),
                                 h3("Number of Listings"),
                                 p(textOutput("count")),
                                 h3("PriceR Estimate"),
                                 p(textOutput("estimate")),
                                 br(),
                                 leafletOutput(outputId = "map"),
                                 br()),
                        tabPanel("Explore Listings",
                                 h4("Listings Table"),
                                 p("Explore listings based on your filter in the table below"),
                                 br(),
                                 DT::dataTableOutput("prices")),
                        tabPanel("Neighbourhood Analytics",
                                 br(),
                                 plotOutput("insights"),
                                 br(),
                                 plotOutput("histogram")),
                        tabPanel("Info",
                                 br(),
                                 p("Thank you for visiting this page! This web-app was built through RShiny.
                                   If you are interested, you can check out the code and data on my",
                                   tags$a(href="https://github.com/gl2668/airbnb_priceR", "github"),
                                   " page"),
                                 br(),
                                 p("The data from this web-app was retrieved from InsideAirbnb.com. If you are 
                                 interested in using the data for visualization and ML applications, you can find a version on", 
                                   tags$a(href="https://www.kaggle.com/gl2668/london-airbnb-listings", "kaggle")),
                                 br(),
                                 p("If you have any questions / feedback, please feel free to shoot me an email at
                                   gl2668@columbia.edu. I welcome and greatly appreciate it!"))
                                 )
            
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Data Table Output
    output$prices <- renderDataTable({
        if (input$room_types_input == "All types"){
            listings_table <- listings %>% 
                dplyr::filter(price <= 800) %>%
                dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                              bedrooms == input$rooms,
                              accommodates == input$accommodates) %>%
                select("Price (£)" = price,
                       "Property Type" = property_type,
                       "Review" = review_scores_rating,
                       "Link" = listing_link)
            DT::datatable(listings_table,
                          rownames = FALSE,
                          options = list(pageLength = 8,
                                         columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                          escape = FALSE)
        } else {
            listings_table <- listings %>% 
                dplyr::filter(price <= 800) %>%
                dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                              bedrooms == input$rooms,
                              accommodates == input$accommodates,
                              room_type == input$room_types_input) %>%
                select("Price (£)" = price,
                       "Property Type" = property_type,
                       "Review" = review_scores_rating,
                       "Link" = listing_link)
            DT::datatable(listings_table,
                          rownames = FALSE,
                          options = list(pageLength = 8,
                                         columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                          escape = FALSE)
        }
        
    })
    
    # Text Output - Count
    output$count <- renderText({
        paste("There are", listings %>% 
            dplyr::filter(price <= 800) %>%
            dplyr:: filter(neighbourhood_cleansed == input$neighbourhood,
                   bedrooms == input$rooms,
                   accommodates == input$accommodates) %>%
            nrow(), "listings that match your filter in this neighbourhood")
    })
    
    # Text Output - Estimated Price
    output$estimate <- renderText({
        listings_estimate <- listings %>% 
            dplyr::filter(price <= 800) %>% 
            dplyr::filter(neighbourhood_cleansed == input$neighbourhood) %>%
            na.omit()
        
        ctrl <- trainControl("cv", number = 10)
        naive_linear <- train(price ~ accommodates + bedrooms, 
                              data = listings_estimate, 
                              method = "lm", 
                              trControl = ctrl,
                              preProcess = c("center", "scale"))
        
        accommodates <- input$accommodates
        bedrooms <- input$rooms
        test <- data.frame(accommodates, bedrooms)
        
        y_hat <- predict(naive_linear, newdata = test) %>% round(2)
        
        paste("Based on your filter, a listing in this neighbourhood should cost GBP", y_hat, "a night")
    })
    
    # Scatter Plot
    output$insights <- renderPlot ({
        data_sub <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                   bedrooms == input$rooms,
                                   accommodates == input$accommodates,
                                   price <= 800)
        
        ggplot(data_sub, aes(x = review_scores_rating,
                             y = price,
                             color = factor(bathrooms))) +
          geom_point(size = 3,
                     alpha = 0.8) +
          theme(panel.background = element_rect(fill = "white",
                                                colour = "black",
                                                size = 0.5, 
                                                  linetype = "solid"),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(size = .1, 
                                                  color = "black",
                                                  linetype = "dashed")) +
          labs(x = "Review Score Rating (over 100)",
               y = "Price (GBP)",
               color = "Bathrooms",
               title = "Airbnb Statistics for the Neighbourhood",
               subtitle = "Based on Input Filters") +
          geom_hline(yintercept = mean(data_sub$price),
                     colour = "red",
                     linetype = "dashed") +
          geom_vline(xintercept = mean(data_sub$review_scores_rating, na.rm = TRUE),
                     colour = "red",
                     linetype = "dashed")
    })
    
    # Histogram
    output$histogram <- renderPlot ({
        
        data_sub2 <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                               bedrooms == input$rooms,
                                               accommodates == input$accommodates,
                                               price <= 800)
        
        ggplot(data_sub2, aes(x = price,)) +
            geom_histogram() +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed")) +
            labs(x = "Price (GBP)",
                 y = "Number of Listings",
                 title = "Histogram of Airbnb Prices in the Neighbourhood",
                 subtitle = "Based on Input Filters") +
            geom_vline(xintercept = mean(data_sub2$price, na.rm = TRUE),
                       colour = "red",
                       linetype = "dashed")
    })
    
    # Map
    output$map <- renderLeaflet({
        
        content <- paste("Station Name:", tube$Name, "<br/>",
                         "Line:", tube$Line, "<br/>")
        
        tubeIcons <- icons(
            iconUrl = "underground.png",
            iconWidth = 10, iconHeight = 10,
            iconAnchorX = 7.5, iconAnchorY = 8.5)
        
        if (input$room_types_input == "All types"){
            data_sub3 <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                                bedrooms == input$rooms,
                                                accommodates == input$accommodates,
                                                price <= 800)
            
            leaflet(data_sub3) %>% 
            setView(lng = -0.118092, lat = 51.509865, zoom = 10)  %>% #setting the view over ~ center of North America
            addTiles() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircles(data = data_sub3, 
                       lat = ~ latitude, 
                       lng = ~ longitude, 
                       weight = 2, 
                       popup = ~as.character(listing_link), 
                       label = ~as.character(paste0("Price: ", sep = " ", price)), 
                       fillOpacity = 0.3,
                       color = "orange",
                       group = "Listings") %>%
            addMarkers(data = tube, 
                       lat = ~ lat, 
                       lng = ~ lng,
                       icon = tubeIcons,
                       popup = ~as.character(content),
                       group = "Tube",
                       options = markerOptions(opacity = 0.3)) %>%
                addLayersControl(
                    overlayGroups = c("Listings", "Tube"),
                    options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Tube")
                
        } else {
            
            data_sub3 <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                                    bedrooms == input$rooms,
                                                    accommodates == input$accommodates,
                                                    room_type == input$room_types_input,
                                                    price <= 800)
            
            leaflet(data_sub3) %>% 
                setView(lng = -0.118092, lat = 51.509865, zoom = 10)  %>% #setting the view over ~ center of North America
                addTiles() %>% 
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircles(data = data_sub3, 
                           lat = ~ latitude, 
                           lng = ~ longitude, 
                           weight = 2, 
                           popup = ~as.character(listing_link), 
                           label = ~as.character(paste0("Price: ", sep = " ", price)), 
                           fillOpacity = 0.3,
                           color = "orange",
                           group = "Listings") %>%
                addMarkers(data = tube, 
                           lat = ~ lat, 
                           lng = ~ lng,
                           icon = tubeIcons,
                           popup = ~as.character(content),
                           group = "Tube",
                           options = markerOptions(opacity = 0.3)) %>%
                addLayersControl(
                    overlayGroups = c("Listings", "Tube"),
                    options = layersControlOptions(collapsed = FALSE)) %>% hideGroup("Tube")
        }
            
    })
}

# Run the application
# You should not have any R code after this line of code
shinyApp(ui = ui, server = server)
