# FPL 20231201
# Author: Carl Jenkins
# Shiny Webapp for Feeder Display Shiny
# ChatGPT 3.5 assisted code

#=======================================

# manage packages

# List of required packages
packages_list <- c("shiny",
                   "leaflet",
                   "sf",
                   "dplyr",
                   "rgdal")

# Function to check and install missing packages
check_and_install_packages <- function(packages) {
  # Get missing packages
  missing_packages <- packages[!packages %in% installed.packages()]
  
  # Install missing packages
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

# Check and install missing packages
check_and_install_packages(packages_list)

# Load required packages using lapply
lapply(packages_list, library, character.only = TRUE)

#=======================================

ui <- fluidPage(
  titlePanel("Florida Map"),
  mainPanel(
    leafletOutput("map", width = "100%", height = "800px")
  )
)


server <- function(input, output) {
  output$map <- renderLeaflet({
    shapefile <- sf::st_read("sf806935.shp")
    valid_shapefile <- sf::st_make_valid(shapefile)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -81.5158, lat = 27.9944, zoom = 7) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        data = valid_shapefile,
        color = "black",
        weight = 1,
        fillColor = ifelse(valid_shapefile$Risk == 0, "red",
                           ifelse(valid_shapefile$Risk == 1, "orange",
                                  ifelse(valid_shapefile$Risk == 2, "yellow",
                                         ifelse(valid_shapefile$Risk == 3, "blue",
                                                ifelse(valid_shapefile$Risk == 4, "purple", NA))))),
        fillOpacity = ifelse(valid_shapefile$Risk %in% 0:4, 1, 0)
      )
  })
}

shinyApp(ui, server)