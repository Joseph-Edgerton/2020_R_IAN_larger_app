

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
library(scales)
library(glue)

# Define UI
ui <- fluidPage(
  titlePanel("IAN Staff Newsletter with R"),
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "What's your name?", choices = cars),
      selectInput("project", "What projects are you working on?", choices = cars,
                  multiple = TRUE),
      selectInput("times_unavailable", "What times are you busy?", choices = schedule_times$times,
                  multiple = TRUE
      ),
      textAreaInput("comments", "Comments", rows = 3),
      sliderInput("stress", "What is your stress level?", value = 10, min = 0, max = 20),
      actionButton("button", label = "Update", class = "btn-primary" )
  ),
    mainPanel(
        tabsetPanel(
            tabPanel("Schedule", tableOutput("schedule_table")),
            tabPanel("Project network", plotOutput("network_plot")),
            tabPanel("Stress and comments", plotOutput("face_plot"))
        )
    )
))

# Define server logic 
server <- function(input, output) {
  
#  observeEvent(input$button,{
  
#  names_table <- reactiveValues({
#    schedule_times %>% 
#      add_column(c(input$names))
#  })
  
    
 # })
  output$schedule_table <- renderTable(schedule_times)  

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
  glue("{x} - {x}")
}

schedule_times <-  tibble(times = unlist(map(.x = my_list, .f = my_list_fun)))

schedule_times
 

 gt() %>% 
  tab_style(
    style = cell_fill(color = "cyan"),
    locations = cells_body(
      columns = vars(Joe),
      rows =  final_sum == TRUE)
  )



