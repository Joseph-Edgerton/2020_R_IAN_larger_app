

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
      sliderInput("stress", "What is your stress level?", value = 10, min = 0, max = 20),
      selectInput("Mon", "Monday Availability?", choices = cars),
      selectInput("Tue", "Tuesday Availability?", choices = cars),
      selectInput("Wed", "Wednesday Availability?", choices = cars),
      selectInput("Thu", "Thursday Availability?", choices = cars),
      selectInput("Fri", "Friday Availability?", choices = cars),
      selectInput("project", "What projects are you working on?", choices = cars,
                  multiple = TRUE),
      textAreaInput("comments", "Comments", rows = 3),
      selectInput("times_unavailable", "What times are you busy?", choices = schedule_times$times,
                  multiple = TRUE
      ),
      actionButton("button", label = "Update", class = "btn-primary" )
  ),
    mainPanel(
        tabsetPanel(
            tabPanel("Availability",
                     selectInput("name", "What's your name?", choices = cars),
                     selectInput("project", "What projects are you working on?", choices = cars),
                     tableOutput("availability_table")),
            tabPanel("Project network", plotOutput("network_plot")),
            tabPanel("Stress and comments", plotOutput("face_plot")),
            tabPanel("Bill's Schedule", tableOutput("schedule_table"))
        )
    )
))

# Define server logic 
server <- function(input, output) {
  
  push <- eventReactive(input$button,{
  
    Name <- reactiveValues(input$name)
    Mon <- reactiveValues(input$mon)
    Tue <- reactiveValues(input$tue)
    Wed <-  reactiveValues(input$wed)
    Thu <-  reactiveValues(input$thu)
    Fri <- reactiveValues(input$fri)
    
    availability_table$Names <- Name()
    availability_table$Monday <- Mon()
    availability_table$Tuesday <- Tue()
    availability_table$Wednesday <- Wed()
    availability_table$Thursday <- Thu()
    availability_table$Friday <- Fri()
    
 })
  
  output$availability_table <- renderTable(
    push()
    )
  
  output$schedule_table <- renderTable(schedule_times)  

}

# Run the application 
shinyApp(ui = ui, server = server)



availability_table <- tibble(Names = "",
                      Monday = "",
                      Tuesday = "",
                      Wednesday = "",
                      Thursday = "",
                      Friday = "")
  
avail_table


names_table <- reactiveValues({
  schedule_times %>% 
    add_column(c(input$names))
})




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
 


tab_style(
  style = cell_fill(color = "cyan"),
  locations = cells_body(
    columns = vars(Name),
    rows =  final_sum == TRUE)
)


ui <- fluidPage(
  checkboxGroupInput("icons", "Choose icons:",
                     choiceNames =
                       list(icon("calendar"), icon("bed"),
                            icon("cog"), icon("bug")),
                     choiceValues =
                       list("calendar", "bed", "cog", "bug")
  ),
  textOutput("txt")
)

server <- function(input, output, session) {
  output$txt <- renderText({
    icons <- paste(input$icons, collapse = ", ")
    paste("You chose", icons)
  })
}

shinyApp(ui, server)