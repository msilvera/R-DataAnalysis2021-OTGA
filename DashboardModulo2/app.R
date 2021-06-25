library(shiny)
# Define UI for application that draws a histogram
#dashboard_knit.html tiene que estar en la carpeta www para que lo encuentre

ui <- fluidPage(
    htmltools::tags$iframe(src = "dashboard_knit.html", width = '100%',  height = 1000,  style = "border:none;"))


server <- function(input, output) { }


# Run the application 
shinyApp(ui = ui, server = server)