

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
library(chron)
library(glue)

# Define UI
ui <- fluidPage(
  titlePanel("IAN Staff Newsletter with R"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "What's your name?"),
      selectInput("project", "What projects are you working on?", choices = cars,
                  multiple = TRUE),
      checkboxGroupInput("times_unavailable", "What times are you busy?", choices = cars
      ),
      textAreaInput("comments", "Comments", rows = 3),
      sliderInput("stress", "What is your stress level?", value = 10, min = 0, max = 20)
  ),
    mainPanel(
        tabsetPanel(
            tabPanel("Bill's Schedule", plotOutput("plot")),
            tabPanel("Project network", verbatimTextOutput("summary")),
            tabPanel("Stress and comments", tableOutput("table"))
        )
    )
))

# Define server logic 
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)





my_list <- strftime(seq(as.POSIXct("06:00:00", format = "%H:%M:%S", tz = "EST"), 
             as.POSIXct("17:00:00", format = "%H:%M:%S", tz = "EST"),
             by = '30 min'), format = "%I:%M %p", tz = "EST", usetz = TRUE)

class(my_list)

glue("{my_list[1]} - {my_list[2]}")

glue("{my_list[i]} - {my_list[i+1]}") #I want to do this repeatedly 

?sapply
?seq_len

my_list_fun <- function(x){
  glue(". - .+1")
}


map(my_list, ~my_list_fun)

