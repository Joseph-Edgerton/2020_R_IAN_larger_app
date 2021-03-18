

library(tidyverse)
library(GGally)
library(network)
library(here)
library(ggimage)
library(sna)
library(gt)
library(shiny)
library(lubridate)
library(shinyscreenshot)
library(scales)



#theme
# low_stress <- bslib::bs_theme(bg = "#2E4053", fg = "#D4E6F1")
# high_stress <- bslib::bs_theme(bg = "#85929E", fg = "#D1F2EB")
# session$setCurrentTheme(
#   if_else(input$stress %in% 0:10, low_stress, high_stress, missing = NULL)
# )


# Define UI
ui <- fluidPage(
  titlePanel("IAN Staff Newsletter with R"),
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "What's your name?", choices = cars),
      sliderInput("stress", "What is your stress level?", value = 10, min = 0, max = 20),
      splitLayout(
        radioButtons("Mon", "Mon", choices = 1:4),
        radioButtons("Tue", "Tue", choices = 1:4),
        radioButtons("Wed", "Wed", choices = 1:4),
        radioButtons("Thu", "Thu", choices = 1:4),
        radioButtons("Fri", "Fri", choices = 1:4)),
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
                     splitLayout(
                       selectInput("name_search", "What's your name?", choices = cars),
                       selectInput("project_search", "What projects are you working on?", choices = cars)),
                     tableOutput("availability_table")),
            tabPanel("Project network", plotOutput("network_plot")),
            tabPanel("Stress and comments", plotOutput("face_plot")),
            tabPanel("Bill's Schedule", tableOutput("schedule_table"))
        )
    )
))

# Define server logic 
server <- function(input, output, session) {
  
  values <- reactiveValues()
  values$df <- data.frame(Name = character(),
                          Stress = double(),
                          Monday = character(),
                          Tuesday = character(),
                          Wednesday = character(),
                          Thursday = character(),
                          Friday = character(),
                          Project = character(),
                          Comments = character())

    observeEvent(input$button,{
      
      if(input$name %in% values$df$Name){
          values$df <- values$df %>%
            mutate(Stress = case_when(Name %in% input$name ~ as.double(input$stress),
                                      Name != input$name ~ as.double(Stress)),
                   Monday = case_when(Name %in% input$name ~ as.character(input$Mon),
                                      Name != input$name ~ as.character(Monday)),
                   Tuesday = case_when(Name %in% input$name ~ as.character(input$Tue),
                                       Name != input$name ~ as.character(Tuesday)),
                   Wednesday = case_when(Name %in% input$name ~ as.character(input$Wed),
                                         Name != input$name ~ as.character(Wednesday)),
                   Thursday = case_when(Name %in% input$name ~ as.character(input$Thu),
                                        Name != input$name ~ as.character(Thursday)),
                   Friday = case_when(Name %in% input$name ~ as.character(input$Fri),
                                      Name != input$name ~ as.character(Friday)),
                   Project = case_when(Name %in% input$name ~ as.character(input$project),
                             Name != input$name ~ as.character(Project)),
                   Comments = case_when(Name %in% input$name ~ as.character(input$comments),
                                        Name != input$name ~ as.character(Comments)))
      } else {
        new_row <- data.frame(Name = input$name,
                              Stress = input$stress,
                              Monday = input$Mon,
                              Tuesday = input$Tue,
                              Wednesday = input$Wed,
                              Thursday = input$Thu,
                              Friday = input$Fri,
                              Project = input$project,
                              Comments = input$comments)
        
        values$df <- rbind(values$df, new_row)
      }
      
      
      avail_table <- values$df %>% 
        select(Name, Monday, Tuesday, Wednesday, Thursday, Friday)
      
      
      
      
      
      
      
      
    })
    
    
    
    
    
    
  
  output$availability_table <- renderTable(
    avail_table
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

