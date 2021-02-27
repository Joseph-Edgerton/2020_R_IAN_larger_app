

library(tidyverse)
library(GGally)
library(network)
library(here)
library(ggimage)
library(googlesheets4)
library(sna)
library(gt)
library(shiny)
library(lubridate)
library(shinyscreenshot)
library(gcalendr)
library(scales)

# Define UI
ui <- fluidPage(
    mainPanel(
        tabsetPanel(
            tabPanel("Plot", plotOutput("plot")),
            tabPanel("Summary", verbatimTextOutput("summary")),
            tabPanel("Table", tableOutput("table"))
        )
    )
)

# Define server logic 
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)




strftime(seq(as.POSIXct('00:00', format = "%H:%M", tz = "UTC"), 
           as.POSIXct(Sys.Date() + 1), by = '30 min'), format = "%I:%M %p", tz = "EST", usetz = TRUE)


