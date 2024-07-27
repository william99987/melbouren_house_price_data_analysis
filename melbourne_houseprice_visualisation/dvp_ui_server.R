# Load necessary libraries
library(shiny)
library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(packcircles)
library(ggiraph)
library(shinythemes)

# Source the external R scripts
source("dvp_melbourne_house_location.R")
source("dvp_london_house_location.R")

# Manually defining a bounding box
melbourne_bbox <- st_as_sfc(st_bbox(c(xmin = 144.3336, ymin = -38.4339, xmax = 145.8783, ymax = -37.1751), crs = st_crs(4326)))
# Filter the Victoria data to only include locations within Greater Melbourne
vic_data_sf_melbourne <- st_intersection(vic_data_sf, melbourne_bbox)

# Define UI
ui <- navbarPage(
  title = "Melbourne and London Housing Data Visualization",
  theme = shinytheme("cerulean"),
  # UI for melbourne panel
  tabPanel("Melbourne",
           fluidPage(
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
             )),
           h2("Introduction to The Greater Melbourne and London Housing Visualization"),
           fluidRow(
             column(12, 
                    p(HTML(" Welcome to the Victoria and London Housing Data Visualization application.</br>
                      This project offers comprehensive insights into the housing markets of Greater Melbourne and Greater London. Through this application, we aim to explore two main questions:</br>
                      <strong>1.How does location affect house prices in the Greater Melbourne and London areas? </strong></br>
                      <strong>2.How do demographic trends and economic indicators influence these house prices? </strong></br>
                      By engaging with this project, you will gain valuable insights from the visualizations and be encouraged to explore additional interesting patterns and trends on your own.
                      "))
             )
           ),
           
           wellPanel(
             
             wellPanel(
               fluidRow(column(12),h4("Choropleth map for house price distribution in The Greater Melbourne Area"),
                        p(HTML("This visualization presents a choropleth map of house prices in the Greater Melbourne area. 
                               A distinct pattern reveals that house prices escalate as they approach the
                               <span class='highlight-turquoise'>Melbourne CBD</span>. Additionally, prices tend
                               to rise near the <span class='highlight-turquoise'>coastline</span>, reflecting 
                               a common preference for beachfront properties. <span class='highlight-coral'> Try
                               exploring the data on your own!</span> 
                               </br> <span class='highlight-coral'>Tips:</span> Experiment 
                               with filtering the map by selecting a higher price range and using the animation feature which is a play
                               button down the year filter in the right-hand side panel. You will observe that houses in proximity to more expensive
                               suburbs tend to increase in price range as the expensive suburbs."))),
               fluidRow(
               column(8,
                      wellPanel(
                        leafletOutput("vic_map", height = "400px"))
               ),
               column(4,
                      wellPanel(
                        h4("Toolpanel for Map"),
                        sliderInput("year_vic_map", "Select Year:", 
                                    min = 2012, max = 2022, value = 2022, step = 1, width = "100%",
                                    animate = animationOptions(interval = 2000, loop = TRUE)),
                        sliderInput("price_range_vic_map", "Select Price Range:", 
                                    min = 0, max = 6200000, value = c(0, 6200000), step = 1000000, width = "100%"),
                      )
               )
             ),
              fluidRow(
                h3("Line chart for house price by specific suburb"),
                column(8,wellPanel(
                                girafeOutput("suburb_line_chart", height = "400px")
                              )),
                       column(4,wellPanel(
                         h4("Toolpanel for suburb line chart"), 
                         selectInput("selected_suburb", "Select Suburb:", choices = unique(vic_data_sf_melbourne$LOCALITY)))))
             ),
             
             wellPanel(
               fluidRow(column(12,   h4("Bar chart of house price for the Greater Melbourne by suburbs"),
                               p(HTML("This bar chart illustrates the <span class='highlight-coral'>top house prices</span> 
                               in various localities of Victoria by different years. 
                               A clear <span class='highlight-coral'>disparity in house prices</span> is evident, 
                               with <span class='highlight-turquoise'>Toorak leading significantly</span> across years, followed 
                               by <span class='highlight-coral'>neighbourhood suburbs</span> like <span class='highlight-turquoise'>
                               East Melbourne</span> and <span class='highlight-turquoise'>Canterbury</span>, 
                               which also exhibit high median house prices.
                               The chart shows a <span class='highlight-turquoise'>gradual decline in house prices</span> 
                               as we move from left to right, highlighting the concentration of 
                               expensive properties in specific localities. The <span class='highlight-coral'>
                               steep drop</span> from the highest-priced locality to others indicates a marked difference in
                               real estate values within Victoria, suggesting areas of 
                               <span class='highlight-corale'>concentrated affluence</span>.
                               </br>
                               <span class='highlight-coral'>Tips:</span> Try using
                                      the filter in the right panel to adjust price range,
                                      year, and the range of 
                                      thetop suburbs and using the animation feature which is a play
                               button down the year filter.")))),
               fluidRow(
                column(8,wellPanel(girafeOutput("vic_bar_chart"))),
                 column(4,wellPanel(
                   h4("Toolpanel for Bar Chart"),
                   sliderInput("year_vic_bar", "Select Year:", 
                               min = 2012, max = 2022, value = 2022, step = 1, width = "100%",
                               animate = animationOptions(interval = 1000, loop = TRUE)),
                   sliderInput("price_range_vic_bar", "Select Price Range:", 
                               min = 0, max = 6200000, value = c(0, 6200000), step = 1000000, width = "100%"),
                   sliderInput("top_n_bar", "Select Top N Localities:", 
                               min = 5, max = 50, value = 30, step = 5, width = "100%")
                 ))
               )),
           
           wellPanel(
             fluidRow(column(12,
             h4("Bubble chart of house price for the Greater Melbourne by suburbs"),
             p(HTML("This interactive circular packed bubble chart displays house prices by 
                    locality in Victoria for 
                    the selected year. Each bubble represents a locality, with the size of
                    the bubble corresponding to the median house 
                    price in that area. Larger bubbles indicate higher house prices. </br>
                    <span class = 'highlight-coral'> Tips: </span>
                    Users can 
                    filter the data by year and select specific suburbs
                    or a price range to explore.And users can use the animation feature which is a play
                               button down the year filter")))),
             fluidRow(
               column(8, wellPanel(
                 girafeOutput("vic_bubble_chart", height = "400px", width = "100%"))
               ),
               column(4,wellPanel(
                 h4("Toolpanel for Bubble Chart"),
                 sliderInput("year_vic_bubble", "Select Year:", 
                             min = 2012, max = 2022, value = 2022, step = 1, width = "100%",
                             animate = animationOptions(interval = 1000, loop = TRUE)),
                 sliderInput("price_range_vic_bubble", "Select Price Range:", 
                             min = 0, max = 6200000, value = c(0, 6200000), step = 1000000, width = "100%"),
                 sliderInput("top_n_bubble", "Select Top N Localities:", 
                             min = 5, max = 50, value = 30, step = 5, width = "100%")
               )
               ))
           )
  ),
 
wellPanel(
  h2("Australian House Price with Indicators"),
  h3("Australian house price index growth rate - Annual GDP growth rate bubble chart"),
  p(HTML("The bubble chart illustrates the relationship between 
         the Australian GDP Growth Rate and the House Price Index 
         Growth Rate over the years. Each bubble represents a year,
         with its size corresponding to the House Price Index Growth Rate (%).
         The color gradient from blue to red
         indicates the progression of years from <span class='highlight-coral'>2000 
         to 2020</span>. This chart provides insights into how 
         <span class='highlight-turquoise'>economic growth</span>, as measured by GDP,
         correlates with changes in house prices across Australia. 
         The pattern suggests that higher GDP growth rates often correspond with
         higher house price index growth rates.")),
  fluidRow(
    column(12, 
           wellPanel(
             girafeOutput("vic_gdp_indicators_plot", height = "400px"))
    )),
  h3("Australian house price index growth rate - annual unemployment rate bubble chart"), 
  p(HTML("The bubble chart illustrates the relationship between the Australian Unemployment 
         Rate and the House Price Index Growth Rate over the years. Each bubble represents a year, 
         with its size corresponding to the House Price
         Index Growth Rate (%). The color gradient from blue 
         to red indicates the progression of years from <span class='highlight-turquoise'>2000 to
         2020</span>. This chart provides insights into how <span class='highlight-turquoise'>unemployment
         rates</span> correlate with changes in house prices across Australia. The pattern suggests that higher
         unemployment rates often correspond with lower house price index growth rates.This high unemployment rate
         and low house growth rate often happens in <span class = 'highlight-coral'>early 1990s </span>.Low unemployment
         rate and high house price growth rate is <span class = 'highlight-coral'>dominent </span> in recent years")),
  fluidRow(
    column(12, 
           wellPanel(
             girafeOutput("vic_unemploy_indicators_plot", height = "400px")))
  )
)),
# UI for london panel
tabPanel("London",
         fluidPage(
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
           ),
           h2("Introduction to London Housing Data"),
           fluidRow(
             column(12, 
                    p("This section provides a comprehensive visualization of housing data for London. 
                         You can explore the housing prices through interactive maps, bar charts, and bubble charts.
                         Use the slider to select a year and see how the housing market has changed over time.")
             )
           ),
           
           
           wellPanel(
           wellPanel(
             h4("Chorolepeth map for house price distribution in London"),
             fluidRow(column(12,                        
                      p(HTML("This visualisation is a choropleth map of house prices in the Greater London area. A clear pattern emerges, indicating that house prices increase as they near the
                                <span class='highlight-turquoise'>central London</span>.
                                Another observation is that prices also rise closer to the
                                <span class='highlight-coral'>Thames River</span>, aligning with the common preference for waterfront properties.
                                <span class='highlight-coral'>Try exploring the data by yourself!</span> </br>
                                <span class='highlight-coral'>Tips:</span> You can try filtering the map using a higher price
                                range and using the animation function in the right-hand side panel(The play button under the year filter).
                                You will find out that houses near a more expensive borough tend to grow into price range as the expensive borough.")))),
             fluidRow(
               column(8,
                      wellPanel(
                        leafletOutput("london_map", height = "400px"))
               ),
               column(4,
                      wellPanel(
                        h4("Toolpanel for Map"),
                        sliderInput("year_london_map", "Select Year:", 
                                    min = 1995, max = 2022, value = 2022, step = 1, width = "100%",
                                    animate = animationOptions(interval = 1000, loop = TRUE)),
                        sliderInput("price_range_london_map", "Select Price Range:", 
                                    min = 0, max = 1365000, value = c(0, 1365000), step = 227500, width = "100%")
                      )
               )
             ),
               fluidRow(
                 h3("Line chart for house price by specific suburbs"),
                 column(8, 
                        wellPanel(girafeOutput("borough_line_chart", height = "400px"))
                 ),
                 column(4, wellPanel(selectInput("selected_borough", "Select Borough:", choices = unique(london_data_sf$District))))
               )
             ),
             
             wellPanel(
               h4("Bar chart of house price for the Greater London by boroughs"),
               p(HTML("This bar chart illustrates the top house prices in 
                         various boroughs of London by different years. A clear disparity
                         in house prices is evident, with <span class ='highlight-coral'>Westminster </span> leading significantly across years,
                         followed by boroughs like Kensington and Chelsea, which also exhibit high median house prices. 
                         The chart shows a <span class ='highlight-coral'> gradual decline </span>in house prices as we move from left to right,
                         highlighting the concentration of expensive properties in specific boroughs. 
                         The steep drop from the highest-priced borough to others indicates a marked difference
                         in real estate values within London.</br> 
                         <span class ='highlight-coral'>Tips:</span> Try using the filter above to adjust price range, year and the range of the top boroughs.Try using the play button under the year filter, it will have animation effect.")),
               fluidRow(
                 column(8, 
                        wellPanel(girafeOutput("london_bar_chart"))
                 ),
                 column(4,wellPanel(
                   h4("Toolpanel for Bar Chart"),
                   sliderInput("year_london_bar", "Select Year:", 
                               min = 1995, max = 2022, value = 2022, step = 1, width = "100%",
                               animate = animationOptions(interval = 1000, loop = TRUE)),
                   sliderInput("price_range_london_bar", "Select Price Range:", 
                               min = 0, max = 1365000, value = c(0, 1365000), step = 227500, width = "100%"),
                   sliderInput("top_n_bar", "Select Top N Boroughs:", 
                               min = 3, max = 33, value = 15, step = 3, width = "100%")
                 ))
               )),
           
           wellPanel(
             h4("Bubble chart of house price for the Greater London by boroughs"),
             p(HTML("This interactive circular packed bubble chart displays house prices by borough 
                    in London for the selected year. Each bubble represents a borough, with the size 
                    of the bubble corresponding to the median house price in that area. Larger bubbles 
                    indicate higher house prices. </br>
                    <span class ='highlight-coral'> Tips: </span>Users can filter the data by year and select specific boroughs or a price range to explore.Try using the play button under the year filter, it will have animation effect.")),
             fluidRow(
               column(8, wellPanel(
                 girafeOutput("london_bubble_chart", height = "400px"))
               ),
               column(4,wellPanel(
                 h4("Toolpanel for Bubble Chart"),
                 sliderInput("year_london_bubble", "Select Year:", 
                             min = 1995, max = 2022, value = 2022, step = 1, width = "100%",
                             animate = animationOptions(interval = 1000, loop = TRUE)),
                 sliderInput("price_range_london_bubble", "Select Price Range:", 
                             min = 0, max = 1365000, value = c(0, 1365000), step = 227500, width = "100%"),
                 sliderInput("top_n_bubble", "Select Top N Boroughs:", 
                             min = 3, max = 33, value = 15, step = 3, width = "100%")
               )
               ))
           )),
         

        wellPanel(
          h2("UK House Price with Indicators"),
          h3("UK house price growth rate - UK GDP growth rate bubble chart"),
          p(HTML("The bubble chart illustrates the relationship between the UK GDP 
                 rowth Rate and the House Price Index Growth Rate over the years.
                 Each bubble represents a year, with its size corresponding to
                 the House Price Index Growth Rate. 
                 The color gradient from blue to red 
                 indicates the progression of years from <span class='highlight-coral'>2000 to 2020</span>.
                 This chart provides insights into how <span class='highlight-turquoise'>economic growth</span>, 
                 as measured by GDP, correlates with changes in house prices across the UK. 
                 The pattern suggests that higher GDP growth rates often correspond with higher
                 house price index growth rates, as the bubble grows larger to the right.")),
          
          fluidRow(
            column(12, 
                   wellPanel(
                     girafeOutput("london_gdp_indicators_plot", height = "400px"))
            )),
          
          h3("UK house price growth rate - UK Unemployment rate bubble chart"),
          p(HTML("The bubble chart illustrates the relationship between 
          the UK Unemployment Rate and the House Price Index Growth Rate over the years. 
          Each bubble represents a year, with its size corresponding to the 
        House Price Index Growth Rate. 
          The color gradient from <span class='highlight-turquoise'>blue to red</span> 
          indicates the progression of years from <span class='highlight-turquoise'>2000 to 2020</span>.
          This chart provides insights into how <span class='highlight-turquoise'>unemployment rates</span> 
          correlate with changes in house prices across the UK. The pattern suggests that higher
          unemployment rates often correspond with lower house price index growth rates.
          Comparing this with the <span class='highlight-coral'>UK GDP Growth Rate vs. House Price
          Index Growth Rate</span> chart, we can observe that economic downturns, marked by higher 
          unemployment rates and lower GDP growth rates, generally coincide with lower house price index
          growth rates. This visualization, along with the Australian charts, highlights the significant 
          impact of economic indicators such as GDP growth and unemployment rates on the housing market.")),
          fluidRow(
            column(12, 
                   wellPanel(
                     girafeOutput("london_unemploy_indicators_plot", height = "400px")))
          )
        )
)
)
)

#  server logic
server <- function(input, output) {
  #Filter data for the map
  filtered_vic_data_map <- reactive({
    vic_data_sf_melbourne %>% 
      filter(Year == input$year_vic_map, 
             House_Price >= input$price_range_vic_map[1], 
             House_Price <= input$price_range_vic_map[2])
  })
  #Filter data for the vic bar chart
  filtered_vic_data_bar <- reactive({
    vic_data_sf_melbourne %>% 
      filter(Year == input$year_vic_bar, 
             House_Price >= input$price_range_vic_bar[1], 
             House_Price <= input$price_range_vic_bar[2]) %>%
      arrange(desc(House_Price)) %>%
      head(input$top_n_bar)
  })
  #Filter data for the vic bubble chart
  filtered_vic_data_bubble <- reactive({
    vic_data_sf_melbourne %>% 
      filter(Year == input$year_vic_bubble, 
             House_Price >= input$price_range_vic_bubble[1], 
             House_Price <= input$price_range_vic_bubble[2]) %>%
      arrange(desc(House_Price)) %>%
      head(input$top_n_bubble)
  })
  #Filter data for the london map
  filtered_london_data_map <- reactive({
    london_data_sf %>% 
      filter(Year == input$year_london_map, 
             Median >= input$price_range_london_map[1], 
             Median <= input$price_range_london_map[2])
  })
  #Filter data for london bar chart
  filtered_london_data_bar <- reactive({
    london_data_sf %>% 
      filter(Year == input$year_london_bar, 
             Median >= input$price_range_london_bar[1], 
             Median <= input$price_range_london_bar[2]) %>%
      arrange(desc(Median)) %>%
      head(input$top_n_bar)
  })
  #Filter data for london bubble chart
  filtered_london_data_bubble <- reactive({
    london_data_sf %>% 
      filter(Year == input$year_london_bubble, 
             Median >= input$price_range_london_bubble[1], 
             Median <= input$price_range_london_bubble[2]) %>%
      arrange(desc(Median)) %>%
      head(input$top_n_bubble)
  })
  
  # Reactive expression to filter data by selected suburb
  filtered_suburb_data <- reactive({
    vic_data_sf_melbourne %>% filter(LOCALITY == input$selected_suburb)
  })
  
  # Reactive expression to filter data by selected borough
  filtered_borough_data <- reactive({
    london_data_sf %>% filter(District == input$selected_borough)
  })
  # render vic leaflet
  output$vic_map <- renderLeaflet({
    data <- filtered_vic_data_map()
    create_vic_map(data, input$year_vic_map)
  })
  # render london leaflet
  output$london_map <- renderLeaflet({
    data <- filtered_london_data_map()
    create_london_map(data, input$year_london_map)
  })
  # rebder vic bar chart
  output$vic_bar_chart <- renderGirafe({
    data <- filtered_vic_data_bar()
    create_vic_bar_chart(data, input$year_vic_bar, input$top_n_bar)
  })
  # render london bar chart
  output$london_bar_chart <- renderGirafe({
    data <- filtered_london_data_bar()
    create_london_bar_chart(data, input$year_london_bar)
  })
  # render vic bubble chart
  output$vic_bubble_chart <- renderGirafe({
    data <- filtered_vic_data_bubble()
    create_vic_bubble_chart(data, input$year_vic_bubble, input$top_n_bubble)
  })
  # render london bubble chart
  output$london_bubble_chart <- renderGirafe({
    data <- filtered_london_data_bubble()
    create_london_bubble_chart(data, input$year_london_bubble)
  })
  # render vic indicator bubble chart
  output$vic_gdp_indicators_plot <- renderGirafe({
    vic_create_gdp_vs_house_price_bubble_chart(vic_indicator_data_long)
  })
  # render vic indicator bubble chart
  output$vic_unemploy_indicators_plot <- renderGirafe({
    vic_create_unemployment_vs_house_price_bubble_chart(vic_indicator_data_long)
  })
  # render london indicator chart
  output$london_gdp_indicators_plot <- renderGirafe({
    ld_create_gdp_vs_house_price_bubble_chart(ld_indicator_data_long)
  })
  #render london indicator bubble chart
  output$london_unemploy_indicators_plot <- renderGirafe({
    ld_create_unemployment_vs_house_price_bubble_chart(ld_indicator_data_long)
  })
  
  # Render the line chart for the selected suburb
  output$suburb_line_chart <- renderGirafe({
    data <- filtered_suburb_data()
    create_suburb_line_chart(data, input$selected_suburb)
  })
  
  # Render the interactive line chart for the selected borough
  output$borough_line_chart <- renderGirafe({
    data <- filtered_borough_data()
    create_london_suburb_line_chart(data, input$selected_borough)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

                                      
