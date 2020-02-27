#
# This is an Airbnb web application. You can run the application by clicking
# the 'Run App' button above.
#
#
#

library(dplyr)
library(DT)
library(caret)
library(ggplot2)

# Load File
listings <- readRDS("shiny_listings.rds")

# Define UI for the Airbnb application
ui <- fluidPage(

    # Application title
    titlePanel("Airbnb PriceR", windowTitle = "Airbnb"),

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
                        "Number of rooms:",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("accommodates",
                        "Number of guests:",
                        min = 1,
                        max = 10,
                        value = 2),
            selectInput(inputId = "room_types_input", 
                        label = "Type of Lodging",
                        choices = c("Entire home/apt",
                                    "Private room",
                                    "Shared room",
                                    "Hotel room"
                        ))
        ),

        # Output of Shiny pp
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Listings",
                                 h4("Number of Listings"),
                                 p(textOutput("count")),
                                 br(),
                                 h4("PriceR Estimate"),
                                 p(textOutput("estimate")),
                                 br(),
                                 h4("Explore Listings"),
                                 p("Explore listings based on your filter in the table below"),
                                 br(),
                                 DT::dataTableOutput("prices"),
                                 br()),
                        tabPanel("Host Insights",
                                 br(),
                                 plotOutput("insights"),
                                 br(),
                                 plotOutput("histogram")))
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$prices <- renderDataTable({
        listings_table <- listings %>% 
            dplyr::filter(price <= 800) %>%
            dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                          bedrooms == input$rooms,
                          accommodates == input$accommodates,
                          room_type == input$room_types_input) %>%
            select("Price" = price,
                   "Property Type" = property_type,
                   "Review" = review_scores_rating,
                   "Link" = listing_url)
        DT::datatable(listings_table,
                      rownames = FALSE,
                      options = list(pageLength = 8))
    })
    
    output$count <- renderText({
        paste("There are", listings %>% 
            dplyr::filter(price <= 800) %>%
            dplyr:: filter(neighbourhood_cleansed == input$neighbourhood,
                   bedrooms == input$rooms,
                   accommodates == input$accommodates) %>%
            nrow(), "listings that match your filter in this neighbourhood")
    })
    
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
        
        paste("Based on your filter, a listing in this neighbourhood should cost USD", y_hat, "a night")
    })
    
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
               y = "Price (USD)",
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
            labs(x = "Price (USD)",
                 y = "Number of Listings",
                 title = "Histogram of Airbnb Prices",
                 subtitle = "Based on Input Filters") +
            geom_vline(xintercept = mean(data_sub2$price, na.rm = TRUE),
                       colour = "red",
                       linetype = "dashed")
    })
}

# Run the application
# You should not have any R code after this line of code
shinyApp(ui = ui, server = server)
