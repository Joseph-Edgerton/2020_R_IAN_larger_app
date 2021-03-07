

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
  
  values <- reactiveValues()
  values$df <- data.frame(Name = character(),
                          Monday = character(),
                          Tuesday = character(),
                          Wednesday = character(),
                          Thursday = character(),
                          Friday = character())
  
  
    observeEvent(input$button,{
      
      if(input$name %in% values$df$Name){
          values$df <- values$df %>%
            mutate(Monday = case_when(Name == input$name ~ input$Mon,
                                      Name != input$name ~ Monday),
                   Tuesday = case_when(Name == input$name ~ input$Tue,
                                       Name != input$name ~ Tuesday),
                   Wednesday = case_when(Name == input$name ~ input$Wed,
                                         Name != input$name ~ Wednesday),
                   Thursday = case_when(Name == input$name ~ input$Thu,
                                        Name != input$name ~ Thursday),
                   Friday = case_when(Name == input$name ~ input$Fri,
                                      Name != input$name ~ Friday))
      } else {
        new_row <- data.frame(Name = input$name,
                              Monday = input$Mon,
                              Tuesday = input$Tue,
                              Wednesday = input$Wed,
                              Thursday = input$Thu,
                              Friday = input$Fri)
        
        values$df <- rbind(values$df, new_row)
      }
      
      
    })
    
  
  output$availability_table <- renderTable(
    values$df
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



& (input$Mon != values$df$Mon | 
     input$Tue != values$df$Tue |
     input$Wed != values$df$Wed |
     input$Thu != values$df$Thr |
     input$Fri != values$df$Fri))

