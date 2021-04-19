

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
      sliderInput("stress", "What is your stress level?", value = 10, min = 1, max = 20),
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
            tabPanel("Bill's Schedule", tableOutput("schedule_table"))
        )
    )
))

# Define server logic 
server <- function(input, output, session) {
  
  # values <- reactiveValues()
  new_df <- data.frame(Name = character(),
                          Stress = integer(),
                          Mon = character(),
                          Tue = character(),
                          Wed = character(),
                          Thu = character(),
                          Fri = character(),
                          Project = character(),
                          Comments = character())

    observeEvent(input$button,{
      
      if(input$name %in% new_df$Name){
          new_df <- new_df %>%
            mutate(Stress = case_when(Name %in% input$name ~ as.integer(input$stress),
                                      Name != input$name ~ as.integer(Stress)),
                   Mon = case_when(Name %in% input$name ~ as.character(input$Mon),
                                      Name != input$name ~ as.character(Mon)),
                   Tue = case_when(Name %in% input$name ~ as.character(input$Tue),
                                       Name != input$name ~ as.character(Tue)),
                   Wed = case_when(Name %in% input$name ~ as.character(input$Wed),
                                         Name != input$name ~ as.character(Wed)),
                   Thu = case_when(Name %in% input$name ~ as.character(input$Thu),
                                        Name != input$name ~ as.character(Thu)),
                   Fri = case_when(Name %in% input$name ~ as.character(input$Fri),
                                      Name != input$name ~ as.character(Fri)),
                   Project = case_when(Name %in% input$name ~ as.character(str_c(input$project, collapse = ", ")),
                             Name != input$name ~ as.character(Project)),
                   Comments = case_when(Name %in% input$name ~ as.character(input$comments),
                                        Name != input$name ~ as.character(Comments))) 
            
            
      } else {
        new_row <- data.frame(Name = input$name,
                              Stress = input$stress,
                              Mon = input$Mon,
                              Tue = input$Tue,
                              Wed = input$Wed,
                              Thu = input$Thu,
                              Fri = input$Fri,
                              Project = str_c(input$project, collapse = ", "),
                              Comments = input$comments)
          
        
        new_df <- rbind(new_df, new_row)
      }

      
    })
    

    
    observeEvent(input$button,{
    req(new_df)

  Name_exp <- new_df$Name
  Proj_exp <- new_df$Project    
      
    edge <- tibble(from = Name_exp,
                   to = Proj_exp)
    
    edge <- edge %>% 
      separate_rows(to, sep = ", ")
    
    
    # unique_names <- tibble(vertex = unique(shiny_test$Name), type = "name")
    # unique_projects <- tibble(vertex = unique(shiny_test$Project), type = "project")
    # 
    # node <- bind_rows(unique_names, unique_projects)
    
    net <- network(edge, vertex.attr = NULL,  matrix.type = "edgelist")
    
    
    #make function to take input and match each project with a corresponding color
    
    
    net %v% "color" = case_when(
      net %v% "vertex.names" == "Annie Carew" ~ "grey",
      net %v% "vertex.names" == "RioGRC" ~ "red",
      net %v% "vertex.names" == "edX" ~ "red")
    
    
    # names_of_peeps <- shiny_test$Name
    # names_of_projects <- shiny_test$Project
    # 
    # net %v% "color" = case_when(
    #   net %v% "vertex.names" %in% names_of_peeps ~ "red",
    #   net %v% "vertex.names" %in% names_of_projects ~ "grey")
    
    # ?sample
    
    # x = names
    # y = projects
    
    
    demo_x_test <- ggnet2(net, label = FALSE, color = "color", layout.exp = 0.25,
                          mode = "kamadakawai", layout.par = list(niter = 500)) +
      guides(color = FALSE, size = FALSE)
    
    
    #, layout.par = list(repulsion)
    #ifelse(net %v% "color" != "grey", TRUE, FALSE)
    
    
    demo_x_test$data <- demo_x_test$data %>% 
      rename(Name = label)
    #need to make the schedule info table to combine with the coordinate table
    
    
    Mon_exp <- new_df$Mon
    Tue_exp <- new_df$Tue
    Wed_exp <- new_df$Wed
    Thu_exp <- new_df$Thu
    Fri_exp <-new_df$Fri
    
    df_1 <- tibble(Mon = Mon_exp,
                   Tue = Tue_exp,
                   Wed = Wed_exp,
                   Thu = Thu_exp,
                   Fri = Fri_exp,
                   Name = Name_exp)
    
    
    df_1 <- df_1 %>% 
      mutate(across(.cols = c(Mon, Tue, Wed, Thu, Fri),
                    .fns = ~case_when(.x == "All" ~ 1,
                                      .x == "AM" ~ 2,
                                      .x == "PM" ~ 6,
                                      .x == "Not" ~ 4))) 
    # mutate(image = case_when(Name == "Alex Fries" ~ here("data","AFries_headshot.jpg"),
    #                          Name == "Annie Carew" ~ here("data","ACarew_headshot.jpg"),
    #                          Name == "Bill Dennison" ~ here("data", "BDennsion_headshot.jpg"),
    #                          Name == "Caroline Donovan" ~ here("data", "CDonovan_headshot.jpg"),
    #                          Name == "Crystal Nichols" ~ here("data", "CNichols_headshot.png"),
    #                          Name == "Dylan Taillie" ~ here("data","DTaillie_headshot.jpg"),
    #                          Name == "Heath Kelsey" ~ here("data", "HKelsey_headshot.jpg"),
    #                          Name == "Jennifer Clapper" ~ here("data", "JClapper_headshot.png"),
    #                          Name == "Joe Edgerton" ~ here("data", "2020_formal_pic_for_UMCES_website.jpg"),
    #                          Name == "Katie May Laumann" ~ here("data", "KMLaumann_headshot.jpg"),
    #                          Name == "Nathan Miller" ~ here("data", "NMiller_headshot.jpg"),
    #                          Name == "Sky Swanson" ~ here("data","SSwanson_headshot.png"),
    #                          Name == "Trish Summers" ~ here("data", "TSummers_headshot.png"),
    #                          Name == "Vanessa Vargas-Nguyen" ~ here("data", "VVargas-Nguyen_headshot.jpg"),
    #                          Name == "Kelly Dobroski" ~ here("data", "JClapper_headshot.png")))
    
    
    joined_df <- full_join(demo_x_test$data, df_1, by = "Name")
    
    p1 <- demo_x_test +
      geom_point(data = joined_df, aes(x = x -0.05, y = y -0.08), size = 2,
                 shape = joined_df$Mon) +
      geom_point(data = joined_df, aes(x = x -0.025, y = y -0.08), size = 2,
                 shape = joined_df$Tue) +
      geom_point(data = joined_df,aes(x = x -0.0, y = y -0.08),size = 2,
                 shape = joined_df$Wed) +
      geom_point(data = joined_df,aes(x = x +0.025, y = y -0.08),size = 2,
                 shape = joined_df$Thu) +
      geom_point(data = joined_df,aes(x = x +0.05, y = y -0.08),size = 2,
                 shape = joined_df$Fri) +
      # geom_image(data = join_table_test_demo, aes(x = x, y = y, image = image), size = 0.06, asp = 1.5) +
      geom_text(data = joined_df, aes(x = x, y = y,
                                                 label = case_when(color != "grey" ~ Name)))
    
    p1
    
    })
    
    
    
    
    greeting_tibble <- tibble(Hello = "Please enter information")
    
  
  output$availability_table <- render_gt(
    if(is.null(new_df) == TRUE){
      gt(greeting_tibble) 
    } else {
      gt(new_df) %>% 
        cols_width(
          vars(Comments) ~ px(300),
          vars(Mon, Tue, Wed, Thu, Fri, Project) ~ px(60)
        ) %>%
        tab_options(
          container.overflow.x = FALSE
        ) %>% 
        cols_align(
          align = "left",
          column = vars(Comments)
        ) %>% 
      tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_column_labels(everything())
        ) %>%
      tab_style(
        style = list(cell_fill(color = "#3F7B52"),
        cell_text(color = "white")),
        locations = cells_body(
          columns = vars(Stress),
          rows =  Stress %in% 1:4)
        ) %>% 
      tab_style(
        style = cell_fill(color = "#A2CF62"),
        locations = cells_body(
          columns = vars(Stress),
          rows =  Stress %in% 5:8)
        ) %>% 
      tab_style(
        style = cell_fill(color = "#FFE35C"),
        locations = cells_body(
          columns = vars(Stress),
          rows =  Stress %in% 9:12)
      ) %>%
      tab_style(
        style = cell_fill(color = "#F79447"),
        locations = cells_body(
          columns = vars(Stress),
          rows =  Stress %in% 13:16)
      ) %>%
      tab_style(
        style = list(cell_fill(color = "#EE3B3B"),
                     cell_text(color = "white")),
        locations = cells_body(
          columns = vars(Stress),
          rows =  Stress %in% 17:20)
      )
    }  
  )      
        
  output$face_plot <- renderPlot(
    p1
  )
  
  # output$schedule_table <- renderTable(schedule_times)   # joe says this is worthless

}

# Run the application 
shinyApp(ui = ui, server = server)







#cool features
#with selectizeInput use , options = list(create = TRUE) to allow user to write own tags







