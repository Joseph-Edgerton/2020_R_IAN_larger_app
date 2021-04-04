

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




#testing data
name_choices <- c("Annie Carew", "Kiri Carini", "Jennifer Clapper", "Simon Costanzo", "Bill Dennison",
          "Kelly Dobroski", "Caroline Donovan", "Joe Edgerton", "Andrew Elmore", "Alex Fries",
          "Steven Guinn", "Heath Kelsey", "Katie May Laumann", "Nathan Miller", "Emily Nastase",
          "Crystal Nichols", "Trish Summers", "Sky Swanson", "Dylan Taillie", "Vanessa Vargas-Nguyen",
          "Suzi Webster")


avail_choices <- c("All", "AM", "PM", "Not")

project_choices <- c("RioGRC", "CBRC", "DarHRC", "SEACAR", "GeorRC", "edX")



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
      selectInput("name", "What's your name?", choices = name_choices),
      sliderInput("stress", "What is your stress level?", value = 10, min = 0, max = 20),
      splitLayout(
        radioButtons("Mon", "Mon", choices = avail_choices),
        radioButtons("Tue", "Tue", choices = avail_choices),
        radioButtons("Wed", "Wed", choices = avail_choices),
        radioButtons("Thu", "Thu", choices = avail_choices),
        radioButtons("Fri", "Fri", choices = avail_choices)),
      selectInput("project", "What projects are you working on?", choices = project_choices,
                  multiple = TRUE),
      textAreaInput("comments", "Comments", rows = 3),
      # selectInput("times_unavailable", "What times are you busy?", choices = schedule_times$times,
      #             multiple = TRUE
      # ),
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
            tabPanel("Stress and comments", 
                uiOutput(
                outputId = "face_plot")),
            tabPanel("Bill's Schedule", tableOutput("schedule_table"))
        )
    )
))

# Define server logic 
server <- function(input, output, session) {
  
  values <- reactiveValues()
  values$df <- data.frame(Name = character(),
                          Stress = integer(),
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
            mutate(Stress = case_when(Name %in% input$name ~ as.integer(input$stress),
                                      Name != input$name ~ as.integer(Stress)),
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
                   Project = case_when(Name %in% input$name ~ as.character(str_c(input$project, collapse = ", ")),
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
                              Project = str_c(input$project, collapse = ", "),
                              Comments = input$comments)
          
        
        values$df <- rbind(values$df, new_row)
      }

      
    })
    
    
    
    
    #stress plots 
    
    #https://tbradley1013.github.io/2018/08/10/create-a-dynamic-number-of-ui-elements-in-shiny-with-purrr/
    
    # values <- reactiveValues()
    # values$df_face <- data.frame() %>% 
    #   add_column(graphs = NA)
    
   my_graphs <-  eventReactive(input$button,{
      req(values$df)
      #function setup
      #curve
      curve_function <- function(x){
        if (x %in% 0:4) return(-1)
        if (x %in% 5:8) return(-0.5)
        if (x %in% 9:12) return(0)
        if (x %in% 13:16) return(0.5)
        if (x %in% 17:20) return(1)
      }
      
      
      # a = color
      # b = stress value
      # c = name
      
      
      my_plot_function_v2 <- function(a, b, c){
        ggplot(data = values$df_face) +
          geom_point(aes(x = 10, y = 10), size = 100, shape = 21, fill = a) +
          geom_point(aes(x = 8.5, y = 15), size = 10, shape = 21, fill = "white") +
          geom_point(aes(x = 11.5, y = 15), size = 10, shape = 21, fill = "white") +
          geom_curve(aes(x = 8.5, y = 8, xend = 11.5, yend = 8), curvature = curve_function(b), size = 5,
                     color = "white", lineend = "round") +
          geom_label(aes(x = 10, y = 19.5, label = c), size = 8, color = "blue",
                     label.padding = unit(0.5, "lines")) +
          xlim(c(0,20)) + ylim(c(0,20)) +
          theme_void()
      }
      
      #main
      values$df %>%
        mutate(graphs = pmap(.l = list(case_when(Stress %in% 0:4 ~ "#440154FF",
                                                                                          Stress %in% 5:8 ~"#3B528BFF",
                                                                                          Stress %in% 9:12 ~ "#21908CFF",
                                                                                          Stress %in% 13:16 ~ "#5DC863FF",
                                                                                          Stress %in% 17:20 ~ "#FDE725FF"),
                                                                                Stress,
                                                                                Name),
                                                                      .f = ~my_plot_function_v2)) %>%
        pull(graphs)
        
        #,
                                  # Name != input$Name ~ as.list(graphs)))
               
    
      # } else {
      #   new_row <- data.frame(graphs = pmap(.l = list(case_when(input$stress %in% 0:4 ~ "#440154FF",
      #                                              input$stress %in% 5:8 ~"#3B528BFF",
      #                                              input$stress %in% 9:12 ~ "#21908CFF",
      #                                              input$stress %in% 13:16 ~ "#5DC863FF",
      #                                              input$stress %in% 17:20 ~ "#FDE725FF"),
      #                                    input$stress,
      #                                    input$name),
      #                          .f = ~my_plot_function_v2))
      #   
      #   # graphs = pmap(.l = list(case_when(input$stress %in% 0:4 ~ "#440154FF",
      #   #                                                         input$stress %in% 5:8 ~"#3B528BFF",
      #   #                                                         input$stress %in% 9:12 ~ "#21908CFF",
      #   #                                                         input$stress %in% 13:16 ~ "#5DC863FF",
      #   #                                                         input$stress %in% 17:20 ~ "#FDE725FF"),
      #   #                                               input$Stress,
      #   #                                               input$Name),
      #   #                                     .f = ~my_plot_function_v2)
      #   
      #   
      #   values$df_face <- rbind(values$df_face, new_row)
      # }
      
      
      
   })
    
    
    #iwalk step
    observeEvent(input$button, {
      req(my_graphs())
      
      iwalk(my_graphs(), ~{
        output_name <- paste0("plot", .y)
        output$output_name <- renderPlot(.x)
      })
    })
    
    #stress face plot output
    output$face_plot <- renderUI({
      req(my_graphs())
      
      plots_list <- imap(my_graphs(), ~{
        tagList(
          plotOutput(
            outputId = paste0("plot_", .y)
          ),
          br()
        )
        
      })
      
      tagList(plots_list)
    })
    
    
    
    
  
  output$availability_table <- renderTable(
    values$df
    )
  
  
  
  # output$schedule_table <- renderTable(schedule_times)   # joe says this is worthless

}

# Run the application 
shinyApp(ui = ui, server = server)





#cool features
#with selectizeInput use , options = list(create = TRUE) to allow user to write own tags







