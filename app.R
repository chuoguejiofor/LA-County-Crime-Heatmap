#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(ggmap)
library(dplyr)

la_crime_data <- read.csv("F:/Downloads/crime-in-los-angeles/Crime_Data_2010_2017.csv")


  #Clean geographic data
  la_crime_data <- separate(la_crime_data, col = Location, into = c("Latitude", "Longitude"), sep = ",")
  la_crime_data <- mutate(la_crime_data, Latitude=gsub("\\(", "", la_crime_data$Latitude))
  la_crime_data <- mutate(la_crime_data, Longitude=gsub("\\)", "", la_crime_data$Longitude))


  la_crime_data$Latitude <- as.numeric(la_crime_data$Latitude)
  la_crime_data$Longitude <- as.numeric(la_crime_data$Longitude)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Los Angeles County Crime Heatmap"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        dateRangeInput("dates", NULL, start="2016-01-01", end="2016-12-12", min="2010-01-01", max="2016-12-12"),
        sliderInput("zoom", "Zoom", min=3, max=18, value=10)
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("map")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$map <- renderPlot({
        plot_progress <- Progress$new()
        on.exit(plot_progress$close())
        plot_progress$set(message="Plotting...", detail="Downloading map")
        la_map <- get_map(location = "Los Angeles", zoom = input$zoom, maptype = "roadmap", color = "bw")
        plot_progress$set(message="Plotting...", detail="Filtering dates.")
        la_crime_subset <- filter(la_crime_data, between(as.Date(Date.Occurred, format="%m/%d/%Y"), input$dates[1], input$dates[2]))
        plot_progress$inc(0.25, detail="Calculating.")
        plot_bins <- ceiling(nrow(la_crime_subset)/2000)
        if(plot_bins < 3) {
          plot_bins <- 3
        }
        plot_progress$inc(0.10, detail="Mapping.")
        ggmap(la_map) + 
          stat_density2d(data = la_crime_subset, 
                         aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = plot_bins, geom = "polygon") + 
          scale_fill_gradient(low = "yellow", high = "red") + 
          scale_alpha(range = c(0, 0.3), guide = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

