library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(packcircles)
library(htmltools)
library(ggiraph)
library(tidyr)
library(scales)

# Read the data
vic_combined_data <- read.csv('vic_combined_data_cleaned.csv')
vic_data_sf <- st_as_sf(vic_combined_data, wkt = "geometry", crs = 4326)
indicator_data <- read.csv('gdp_houseindex_unemploy_vic.csv')

# function for creating map
create_vic_map <- function(data, selected_year) {
  data_filtered <- data %>% filter(Year == selected_year)
  price_palette <- colorNumeric(palette = "YlOrRd", domain = data_filtered$House_Price)
  
  # labels for the vic map
  labels <- sprintf(
    "<strong>Suburb:</strong> %s<br/><strong>Median house price: $</strong> %s",
    data_filtered$LOCALITY, format(data_filtered$House_Price, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
  
  leaflet(data = data_filtered) %>%
    addTiles() %>%
    setView(lng = 144.9631, lat = -37.8136, zoom = 10) %>%
    addPolygons(fillColor = ~price_palette(House_Price), 
                color = "#444444", 
                weight = 1,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,
                highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
                ) %>%
    addLegend("bottomright", pal = price_palette, values = ~House_Price,  
              title = paste("Victoria House Price in", selected_year),
              labFormat = labelFormat(suffix = " $"),
              opacity = 0.6)
}
# create_vic_map(vic_data_sf,2016)

# Function to create Bar plot for Victoria
create_vic_bar_chart <- function(data, selected_year, top_n) {
  data_filtered <- data %>%
    filter(Year == selected_year) %>%
    arrange(desc(House_Price)) %>%
    head(top_n)
  
  tooltips <- sprintf(
    "<strong>Suburb:</strong> %s<br/><strong>Median House Price:</strong> $%s",
    data_filtered$LOCALITY, format(data_filtered$House_Price, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
  p <- ggplot(data_filtered, aes(x = reorder(LOCALITY, -House_Price), y = House_Price, 
                                 tooltip = tooltips, data_id = LOCALITY)) +
    geom_bar_interactive(stat = "identity", fill = brewer.pal(9, "Blues")[6], alpha = 0.6) +
    theme_minimal() +
    labs(title = paste("Top", top_n, "House Prices in Victoria Localities (", selected_year, ")", sep = ""),
         x = "Locality",
         y = paste("House Price in ", selected_year, " (AUD)", sep = "")) +
    scale_y_continuous(labels = comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom", legend.title = element_blank())
          
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
}

#bar <- create_vic_bar_chart(vic_data_sf, 2015, 30)
#print(bar)

# Function to create interactive line chart for house price trends in a selected suburb
create_suburb_line_chart <- function(data, selected_suburb) {
  tooltips <- sprintf(
    "<strong>Year:</strong> %d<br/><strong>House Price:</strong> $%s",
    data$Year, format(data$House_Price, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
  p <- ggplot(data, aes(x = Year, y = House_Price)) +
    geom_line_interactive(aes(x = Year, y = House_Price), color = "blue") +
    geom_point_interactive(aes(x = Year, y = House_Price, tooltip = tooltips), color = "red")  +
    scale_y_continuous(labels = comma) +
    labs(title = paste("House Price Trends in", selected_suburb),
         x = "Year",
         y = "Median House Price (AUD)") +
    theme_minimal()
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"), 
                   opts_hover(css = "fill:red;stroke:black;stroke-width:2px;"))
}


# Function to create circular packed bubble chart for Victoria
create_vic_bubble_chart <- function(data, selected_year, top_n) {
  data_filtered <- data %>%
    filter(Year == selected_year) %>%
    arrange(desc(House_Price)) %>%
    head(top_n)
  
  #tooltips style
  tooltips <- sprintf(
    "<strong>Suburb:</strong> %s<br/><strong>Median House Price:</strong> $%s",
    data_filtered$LOCALITY, format(data_filtered$House_Price, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
  # Generate the circle packing layout
  packing <- circleProgressiveLayout(data_filtered$House_Price, sizetype = 'area')
  data_filtered <- cbind(data_filtered, packing)
  
  # Generate the vertices for each circle
  circle_data <- circleLayoutVertices(packing, npoints = 50)
  
  # Calculate the bounding circle
  center_x <- mean(circle_data$x)
  center_y <- mean(circle_data$y)
  max_radius <- max(sqrt((circle_data$x - center_x)^2 + (circle_data$y - center_y)^2))
  
  bounding_circle <- circleLayoutVertices(data.frame(
    x = center_x,
    y = center_y,
    r = max_radius
  ), npoints = 100)
  
  p <- ggplot() + 
    geom_polygon(data = bounding_circle, aes(x, y), fill = NA, color = "black", size = 1) +
    geom_polygon(data = circle_data, aes(x, y, group = id), fill = "steelblue", color = "black", alpha = 0.3) +
    geom_polygon_interactive(data = circle_data, aes(x, y, group = id, tooltip = tooltips[id], data_id = data_filtered$LOCALITY[id]), fill = "steelblue", color = "black", alpha = 0.3) +
    geom_text(data = data_filtered, aes(x, y, label = LOCALITY), size = 2.5, hjust = 0.5, vjust = 0.5) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(title = paste("Victoria House Prices by Locality (", selected_year, ")", sep = ""))
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
}

 # bubble <- create_vic_bubble_chart(vic_data_sf, 2015, 30)
 # print(bubble)

 
 # Convert the dataframe to long format
 vic_indicator_data_long <- indicator_data %>%
   pivot_longer(cols = c(GDP_Growth_Rate, Unemployment_Rate), 
                names_to = "Metric", 
                values_to = "Value")

 # Function to create interactive bubble chart for GDP Growth Rate vs. House Price Index Growth Rate
 vic_create_gdp_vs_house_price_bubble_chart <- function(data) {
   data_gdp <- data %>% filter(Metric == "GDP_Growth_Rate")
   
   # Tooltip style
   tooltips <- sprintf(
     "<strong>Year:</strong> %s<br/><strong>GDP Growth Rate: </strong> %.1f%%<br/><strong>House Price Index Growth Rate: </strong> %.1f%%",
     data_gdp$Year, data_gdp$Value, data_gdp$House_Price_Index_Growth_Rate
   ) %>% lapply(htmltools::HTML)
   
   p <- ggplot(data_gdp, aes(x = Value, y = House_Price_Index_Growth_Rate, size = House_Price_Index_Growth_Rate, color = Year, tooltip = tooltips, data_id = Year)) +
     geom_point_interactive(alpha = 0.7) +
     scale_size(range = c(3, 10)) +
     scale_color_gradient(low = "blue", high = "red") +
     labs(title = "Australia GDP Growth Rate vs. House Price Index Growth Rate",
          x = "GDP Growth Rate (%)",
          y = "House Price Index Growth Rate (%)",
          size = "House Price Index Growth Rate (%)",
          color = "Year") +
     theme_minimal()
   
   girafe(ggobj = p) %>%
     girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
 }
 
 # Function to create interactive bubble chart for Unemployment Rate vs. House Price Index Growth Rate
 vic_create_unemployment_vs_house_price_bubble_chart <- function(data) {
   data_unemployment <- data %>% filter(Metric == "Unemployment_Rate")
   
   # Tooltip style
   tooltips <- sprintf(
     "<strong>Year:</strong> %s<br/><strong>Unemployment Rate: </strong> %.1f%%<br/><strong>House Price Index Growth Rate: </strong> %.1f%%",
     data_unemployment$Year, data_unemployment$Value, data_unemployment$House_Price_Index_Growth_Rate
   ) %>% lapply(htmltools::HTML)
   
   p <- ggplot(data_unemployment, aes(x = Value, y = House_Price_Index_Growth_Rate, size = House_Price_Index_Growth_Rate, color = Year, tooltip = tooltips, data_id = Year)) +
     geom_point_interactive(alpha = 0.7) +
     scale_size(range = c(3, 10)) +
     scale_color_gradient(low = "blue", high = "red") +
     labs(title = "Australia Unemployment Rate vs. House Price Index Growth Rate",
          x = "Unemployment Rate (%)",
          y = "House Price Index Growth Rate (%)",
          size = "House Price Index Growth Rate (%)",
          color = "Year") +
     theme_minimal()
   
   girafe(ggobj = p) %>%
     girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
 }
 
 # # # Example usage
 #  vic_gdp_vs_house_price_bubble_chart <- vic_create_gdp_vs_house_price_bubble_chart(vic_indicator_data_long)
 #  vic_unemployment_vs_house_price_bubble_chart <- vic_create_unemployment_vs_house_price_bubble_chart(vic_indicator_data_long)
 #  
 # # # Print the charts
 #  print(vic_gdp_vs_house_price_bubble_chart)
 #  print(vic_unemployment_vs_house_price_bubble_chart)


