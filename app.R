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
library(tidyr)
library(wordcloud)

# Load File
listings <- readRDS("shiny_listings.rds")
tube <- readRDS("london_tube.rds")
reviews <- readRDS("reviewShiny.rds")
reviewtdm <- readRDS("reviewTDM.rds")

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
                        label = "Room Type",
                        choices = c("All types",
                                    "Entire home/apt",
                                    "Private room",
                                    "Shared room",
                                    "Hotel room"
                        )), width = 4
        ),

        # Output of Shiny pp
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Info",
                                 br(),
                                 p("This website is a tool that can be used by Airbnb hosts. We hope to provide a data-driven way for hosts to analyze and understand
                                 the landscape of short-term accommodations in London. Hosts can also beneft from this website by looking at different cleaning-fees, 
                                 review and rating scores and prices. They can use the tools and visualisations
                                 on this webapp to get a sense of how prices are set on Airbnb and get a pulse-check for how they are positioned
                                 in relation to competitors. For anyone who wishes to become a host and list their property for the first time, this 
                                 tool would be very useful to gather information on how they should price their listing. New Airbnb hosts can use the website for a 
                                   first-cut understanding of the competitive landscape for market entry and pricing strategy"),
                                 br(),
                                 p("This web-app was built through RShiny. If you are interested to find out more about how this app was built, 
                                 you can check out the code and data on the", tags$a(href="https://github.com/gl2668/airbnb_priceR", "github page"),
                                   " page"),
                                 br(),
                                 p("The data from this web-app was retrieved from InsideAirbnb.com. It comprises over 90 columns and approximately 
                                 85,000 host listings in London, UK from 2009 to 5th November 2019. If you are 
                                 interested in using the data for visualization and ML applications, you can find a version on", 
                                   tags$a(href="https://www.kaggle.com/gl2668/london-airbnb-listings", "kaggle")),
                                 br()),
                        tabPanel("Listings",
                                 h3("Explore Airbnb listings in London"),
                                 p("The interactive web-app aims to help you (both Airbnb hosts and guests) to engage with data from Airbnb! Use the 
                                   different toggles and sliders on the left to interact with the data"),
                                 h4("Number of Listings"),
                                 p(textOutput("count")),
                                 h4("PriceR Estimate"),
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
                                 h4("Neighbourhood Analytics"),
                                 p("Find out more about the supply and prices of listings in your neighbourhood"),
                                 br(),
                                 plotOutput("insights"),
                                 br(),
                                 plotOutput("histogram"),
                                 br(),
                                 plotOutput("listingsBreakdown"),
                                 br(),
                                 plotOutput('priceBreakdown'),
                                 br()),
                        tabPanel("Cleaning Fees",
                                 h4("Cleaning Fees of Airbnb Listings"),
                                 p("Users can find out how much on average cleaning fees contribute towards total price"),
                                 br(),
                                 plotOutput("average_cleaning_fees"),
                                 plotOutput("average_cleaning_fees_neighbourhood")),
                        tabPanel("Guest Ratings",
                                 h4("Guest Ratings and Text Reviews"),
                                 p("Explore Guest Ratings and the Sentiment Polarity Scores of plain-text reviews"),
                                 br(),
                                 plotOutput("ratings"),
                                 br(),
                                 plotOutput("sentiment_analysis"),
                                 br(),
                                 h4("WordCloud for Positive Reviews in the Neighbourhood"),
                                 plotOutput("positiveWordcloud"),
                                 br(),
                                 h4("WordCloud for Negative Reviews in the Neighbourhood"),
                                 plotOutput("negativeWordcloud"),
                                 br())
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
        
        paste("Based on the input filters, a listing in this neighbourhood should cost GBP", y_hat, "a night")
    })
    
    # Scatter Plot
    output$insights <- renderPlot ({
        data_sub <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                   bedrooms == input$rooms,
                                   accommodates == input$accommodates,
                                   price <= 800)
        
        ggplot(data_sub, aes(x = review_scores_rating,
                             y = price)) +
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
          labs(x = "Review Score Rating (0-100)",
               y = "Price (GBP)",
               color = "Bathrooms",
               title = "Price and Ratings of Listings based on all Input Filters",
               subtitle = "Orange Dashed Lines are Average Values based on Input Features") +
          geom_hline(yintercept = mean(data_sub$price),
                     colour = "#F7965C",
                     linetype = "dashed") +
          geom_vline(xintercept = mean(data_sub$review_scores_rating, na.rm = TRUE),
                     colour = "#F7965C",
                     linetype = "dashed")
    })
    
    # Histogram
    output$histogram <- renderPlot ({
        
        data_sub2 <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                               bedrooms == input$rooms,
                                               accommodates == input$accommodates,
                                               price <= 800)
        
        ggplot(data_sub2, aes(x = price,)) +
            geom_histogram(fill = "#434343") +
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
                 title = "Histogram of Airbnb Prices based on all Input Filters",
                 subtitle = "Orange Dashed Lines are Average Values based on Input Features") +
            geom_vline(xintercept = mean(data_sub2$price, na.rm = TRUE),
                       colour = "#F7965C",
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
                           lat = ~latitude, 
                           lng = ~longitude, 
                           weight = 2, 
                           popup = ~as.character(paste0("Price: ", price, "<br>",
                                                        "Rating: ", review_scores_rating, "<br>",
                                                        listing_link)), 
                           label = ~as.character(paste0("Price: ", price)), 
                           fillOpacity = 0.3,
                           color = "#F7965C",
                           group = "Listings") %>%
                addMarkers(data = tube, 
                           lat = ~lat, 
                           lng = ~lng,
                           icon = tubeIcons,
                           popup = ~as.character(content),
                           group = "Rail Transport",
                           options = markerOptions(opacity = 0.3)) %>%
                addLayersControl(
                    overlayGroups = c("Listings", "Rail Transport"),
                    options = layersControlOptions(collapsed = FALSE)) %>% 
                hideGroup("Rail Transport")
                
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
                           popup = ~as.character(paste0("Price: ", price, "<br>",
                                                        "Rating: ", review_scores_rating, "<br>",
                                                        listing_link)), 
                           label = ~as.character(paste0("Price: ", sep = " ", price)), 
                           fillOpacity = 0.3,
                           color = "#F7965C",
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
    
    # Rating
    output$ratings <- renderPlot ({
        
        data_sub4 <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                                price <= 800)
        
        ggplot(data_sub4, aes(x = review_scores_rating)) +
            geom_histogram(fill = "#434343") +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed")) +
            labs(x = "Review Score Rating (over 100)",
                 y = "Count",
                 title = "Histogram of Airbnb Ratings based on selected Neighbourhood",
                 subtitle = "Orange Dashed Lines are Average Values based on Input Features") +
            geom_vline(xintercept = mean(data_sub4$review_scores_rating, na.rm = TRUE),
                       colour = "#F7965C",
                       linetype = "dashed")
    })
    
    # Average Cleaning Fees
    output$average_cleaning_fees <- renderPlot ({
        data_sub5 <- listings %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                                price <= 800)
        
        data_sub5 %>%
            group_by(beds) %>%
            summarize(avg_room_fee = round(mean(price, na.rm =TRUE), 2),
                      avg_cleaning_fee = round(mean(cleaning_fee, na.rm =TRUE), 2)) %>%
            filter(beds < 11, beds > 0) %>%
            mutate(total_fee = avg_room_fee + avg_cleaning_fee) %>%
            gather(fee_type, avg_fee, avg_cleaning_fee:avg_room_fee) %>%
            ggplot(aes(x = beds, y = avg_fee/beds, fill = fee_type)) + 
            geom_col() +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed")) +
            scale_x_discrete(name = "Number of Beds in Listing", limits = c(1: 10), breaks = seq(1, 10, 1)) +
            scale_y_continuous(name = "Average Fees per Bed (GBP)") +
            scale_fill_manual(labels = c("Cleaning Fees Per Bed", "Room Fee Per Bed"), values = c("#F7965C", "#FFC39F")) +
            labs(title = "Breakdown of Total Fees by Number of Beds based on Selected Neighbourhood") +
            guides(fill = guide_legend(title = "Fee Type")) +
            geom_text(aes(label = round(avg_fee/beds), y = round(avg_fee/beds) + 0.05),
                      position = position_stack(vjust = 0.5))
    })
    
    # Sentiment Analysis
    output$sentiment_analysis <- renderPlot ({
        data_sub6 <- reviews %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood)
        
        data_sub6 %>%
            ggplot(aes(x = polarity)) + 
            geom_histogram(fill="#434343") +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed")) +
            labs(x = "AFINN Sentiment Score of Reviews",
                 y = "Count",
                 title = "Sentiment Analysis of Reviews based on selected Neighbourhood",
                 subtitle = "Orange Dashed Lines are Average Values based on Input Features") +
            geom_vline(xintercept = mean(data_sub6$polarity, na.rm = TRUE),
                       colour = "#F7965C",
                       linetype = "dashed")
    })
    
    # Positive WordCloud
    output$positiveWordcloud <- renderPlot ({
        data_sub7 <- reviewtdm %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood)
        
        wc <- data_sub7 %>%
            filter(polarity > 5) %>%
            group_by(term) %>%
            summarise(n = sum(count)) %>%
            arrange(desc(n))
        set.seed(1234)
        
        suppressWarnings(wordcloud(words = wc$term,
                                   freq = wc$n,
                                   max.words = 80, 
                                   random.order = FALSE,
                                   colors = 'dodgerblue2'))
    })
    
    # Negative WordCloud
    output$negativeWordcloud <- renderPlot ({
        data_sub8 <- reviewtdm %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood)
        
        nc <- data_sub8 %>%
            filter(polarity < -5) %>%
            group_by(term) %>%
            summarise(n = sum(count)) %>%
            arrange(desc(n))
        set.seed(1234)
        
        wordcloud(words = nc$term,
                  freq = nc$n,
                  max.words = 80, 
                  random.order = FALSE,
                  colors = "orangered2")
    })
    
    # average_cleaning_fees_neighbourhood
    output$average_cleaning_fees_neighbourhood <- renderPlot ({
        data_sub9 <- listings %>% dplyr::filter(bedrooms == input$rooms,
                                                accommodates == input$accommodates,
                                                price <= 800)
        
        data_sub9 %>% 
            group_by(neighbourhood_cleansed, room_type) %>%
            summarize(clean_fee = mean(cleaning_fee, na.rm = TRUE)) %>%
            ggplot(aes(x = neighbourhood_cleansed, y = room_type, fill = clean_fee)) +
            geom_tile(color = "white", size = 0.7) +
            labs(x = "", y = "") +
            scale_y_discrete(expand=c(0,0))+
            scale_x_discrete(expand=c(0,0))+
            scale_fill_viridis_b(name="Average Fee (GBP)", option = "B") +
            ggtitle("Average Cleaning Fee Per Night") +
            coord_fixed()+
            theme(axis.text.x=element_text(angle=90),
                  axis.text=element_text(face="bold"),
                  axis.ticks=element_line(size=0.6),
                  plot.background=element_blank(),
                  panel.border=element_blank())
    })
    
    # Listing Breakdown
    output$listingsBreakdown <- renderPlot ({
        data_sub10 <- listings %>% dplyr::filter(price <= 800,
                                                 neighbourhood_cleansed == input$neighbourhood,
                                                 accommodates <= 10)
        
        data_sub10 %>%
            select(room_type, accommodates) %>%
            group_by(room_type, accommodates) %>%
            count() %>%
            ggplot(aes(fill=room_type, y=n, x=factor(accommodates))) +
            geom_bar(position ="stack", stat="identity", alpha=0.7) +
            scale_fill_manual(name = "Room Type", 
                              values = c("red", "lightblue", "orange", "grey", "blue")) +
            labs(x= "Number of Guests Listing can Accommodate", y = "Count of Units",
                 title = "Number of Listings by Maximum Pax and Room Type") +
            theme_minimal() +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed"),
                  panel.grid.minor = element_blank(),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6),
                  plot.title = element_text(size = 12, margin = margin(b = 10)))

    })
    
    # Price Breakdown
    output$priceBreakdown <- renderPlot ({
        data_sub11 <- listings %>% 
            dplyr::filter(price <= 800,
                          neighbourhood_cleansed == input$neighbourhood,
                          accommodates <= 10) %>%
            dplyr::select(accommodates, room_type, price)
            
        
        data_sub11 %>% 
            ggplot(aes(x = factor(accommodates), y = price)) +
            geom_boxplot(alpha = 0.2) +
            facet_wrap(~room_type) + 
            labs(x= "Number of Guests Listing can Accommodate", y = "Price (GBP)",
                 title = "Price by Maximum Pax and Room Type") +
            theme(panel.spacing = unit(1, "lines"))
    })
    
}

# Run the application
# You should not have any R code after this line of code
shinyApp(ui = ui, server = server)
