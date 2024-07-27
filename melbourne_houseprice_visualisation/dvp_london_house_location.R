library(sf)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(packcircles)
library(ggiraph)
library(scales)

# Read data
london_house_loc_data <- read.csv('uk_house_cleaned.csv')
indicator_data <- read.csv('gdp_unemployment_house_index.csv')


# Change geocoding: Convert the data to spatial format and transform the coordinate reference system (CRS)
london_data_sf <- st_as_sf(london_house_loc_data, wkt = "geometry", crs = 27700)
london_data_sf <- st_transform(london_data_sf, crs = 4326)

# Handling NA values: Replace NA in 'Median' column with the mean value
london_data_sf$Median <- as.numeric(london_data_sf$Median)
london_data_sf$Median[is.na(london_data_sf$Median)] <- mean(london_data_sf$Median, na.rm = TRUE)  # Replace NA with mean

# Function to create the London map
create_london_map <- function(data, selected_year) {
  data_filtered <- data %>% filter(Year == selected_year)
  price_palette <- colorNumeric(palette = "YlOrRd", domain = data_filtered$Median)
  
  #labels style
  labels <- sprintf(
    "<strong>Borough:</strong> %s<br/><strong>Median house price £: </strong> %s",
    data_filtered$District, format(data_filtered$Median, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
  leaflet(data = data_filtered) %>%
    addTiles() %>%
    addPolygons(fillColor = ~price_palette(Median), 
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
                  direction = "auto"
                  )
                ) %>%
    addLegend("bottomright", pal = price_palette, values = ~Median,  
              title = paste("London House Price in", selected_year),
              labFormat = labelFormat(suffix = " £"),
              opacity = 0.6)
}

 # map <- create_london_map(london_data_sf, 2015)
 # print(map)

# Function to create an interactive line chart for house price trends in a selected London borough
create_london_suburb_line_chart <- function(data, selected_borough) {
  data_filtered <- data %>% filter(District == selected_borough)
  
  # Tooltips style: Define the tooltips for the chart
  tooltips <- sprintf(
    "<strong>Year:</strong> %s<br/><strong>Median house price £: </strong> %s",
    data_filtered$Year, format(data_filtered$Median, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
  p <- ggplot(data_filtered, aes(x = Year, y = Median)) +
    geom_line_interactive(aes(group = 1), color = "blue") +
    geom_point_interactive(aes(x = Year, y = Median, tooltip = tooltips), color = "red") +
    scale_y_continuous(labels = comma) +
    labs(title = paste("House Price Trends in", selected_borough),
         x = "Year",
         y = "Median House Price (GBP)") +
    theme_minimal()
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"), 
                   opts_hover(css = "fill:red;stroke:black;stroke-width:2px;"))
}


# Function to create a bar chart for London house prices
create_london_bar_chart <- function(data, selected_year) {
  data_filtered <- data %>% filter(Year == selected_year)
  
  # Tooltips style: Define the tooltips for the chart
  tooltips <- sprintf(
    "<strong>Borough:</strong> %s<br/><strong>Median house price £: </strong> %s",
    data_filtered$District, format(data_filtered$Median, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
  p <- ggplot(data_filtered, aes(x = reorder(District, -Median), y = Median, tooltip = tooltips, data_id = District)) +
    geom_bar_interactive(stat = "identity", fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    labs(title = paste("House Prices in London Boroughs (", selected_year, ")", sep = ""),
         x = "Borough",
         y = paste("House Price in ", selected_year, " (GBP)", sep = "")) +
    scale_y_continuous(labels = comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom", legend.title = element_blank())
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
}
# 
#   bar <- create_london_bar_chart(london_data_sf, 2015)
#   print(bar)

# Function to create a bubble chart for London house prices
create_london_bubble_chart <- function(data, selected_year) {
  data_filtered <- data %>% filter(Year == selected_year)
  packing <- circleProgressiveLayout(data_filtered$Median, sizetype = 'area')
  data_filtered <- cbind(data_filtered, packing)
  circle_data <- circleLayoutVertices(packing, npoints = 50)
  
  #tooltips style
  tooltips <- sprintf(
    "<strong>Borough:</strong> %s<br/><strong>Median house price £: </strong> %s",
    data_filtered$District, format(data_filtered$Median, big.mark = ",")
  ) %>% lapply(htmltools::HTML)
  
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
    geom_polygon_interactive(data = circle_data, aes(x, y, group = id, tooltip = tooltips[id], data_id = data_filtered$District[id]), fill = "steelblue", color = "black", alpha = 0.3) +
    geom_text(data = data_filtered, aes(x, y, label = District), size = 2.5, hjust = 0.5, vjust = 0.5) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = paste("House Prices in London Boroughs (", selected_year, ")", sep = "")) 
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
}

# bubble <- create_london_bubble_chart(london_data_sf, 2015)
# print(bubble)

# Convert the dataframe to long format for GDP and unemployment data
ld_indicator_data_long <- indicator_data %>%
  pivot_longer(cols = c(GDP_Growth_Rate, Unemployment_Rate), 
               names_to = "Metric", 
               values_to = "Value")

# Function to create bubble chart for GDP Growth Rate vs. House Price Index Growth Rate
ld_create_gdp_vs_house_price_bubble_chart <- function(data) {
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
    labs(title = "UK GDP Growth Rate vs. House Price Index Growth Rate",
         x = "GDP Growth Rate (%)",
         y = "House Price Index Growth Rate (%)",
         size = "House Price Index Growth Rate (%)",
         color = "Year") +
    theme_minimal()
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
}

# Function to create interactive bubble chart for Unemployment Rate vs. House Price Index Growth Rate
ld_create_unemployment_vs_house_price_bubble_chart <- function(data) {
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
    labs(title = "UK Unemployment Rate vs. House Price Index Growth Rate",
         x = "Unemployment Rate (%)",
         y = "House Price Index Growth Rate (%)",
         size = "House Price Index Growth Rate (%)",
         color = "Year") +
    theme_minimal()
  
  girafe(ggobj = p) %>%
    girafe_options(opts_tooltip(css = "background-color:#fff;color:#333;font-size:14px;padding:5px;border-radius:3px;"))
}

# # Example usage
# ld_gdp_vs_house_price_bubble_chart <- ld_create_gdp_vs_house_price_bubble_chart(ld_indicator_data_long)
# ld_unemployment_vs_house_price_bubble_chart <- ld_create_unemployment_vs_house_price_bubble_chart(ld_indicator_data_long)
# # 
# # # Print the charts
#  print(ld_gdp_vs_house_price_bubble_chart)
#  print(ld_unemployment_vs_house_price_bubble_chart)
