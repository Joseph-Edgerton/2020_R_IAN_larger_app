

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
          "Suzi Webster", "Ricky Arnold")


avail_choices <- c("All", "AM", "PM", "Not")

project_choices <- c("RioGRC", "CBRC", "DarHRC", "SEACAR", "GeorRC", "edX", "KwanRC", "MissRC",
                     "CGeoRC", "CAdaRC", "MDcbRC", "USGS", "NOAA", "CMC", "IANsup", "UMCES", "UofSC",
                     "Unfund", "Class")



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
                     # splitLayout(
                     #   selectInput("name_search", "What's your name?", choices = cars),
                     #   selectInput("project_search", "What projects are you working on?", choices = cars)),
                     tableOutput("availability_table")),
            tabPanel("Project network", plotOutput("network_plot")),
            tabPanel("Bill's Schedule", textOutput("Bill_text"), 
                     tableOutput("schedule_table"))
        )
    )
))

# Define server logic 
server <- function(input, output, session) {
  
  values <- reactiveValues()
  values$df <- data.frame(Name = character(),
                          Stress = integer(),
                          Mon = character(),
                          Tue = character(),
                          Wed = character(),
                          Thu = character(),
                          Fri = character(),
                          Project = character(),
                          Comments = character())

    observeEvent(input$button,{
      
      if(input$name %in% values$df$Name){
          values$df <- values$df %>%
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
          
        
        values$df <- rbind(values$df, new_row)
      }

      
    })
    

    
    observeEvent(input$button,{
    req(values$df)

  Name_exp <- reactive({as.character(values$df$Name)})
  Proj_exp <- reactive({as.character(values$df$Project)})    
      
    edge <- tibble(from = Name_exp(),
                   to = Proj_exp())
    
    edge <- edge %>% 
      separate_rows(to, sep = ", ")
    
    
    # unique_names <- tibble(vertex = unique(shiny_test$Name), type = "name")
    # unique_projects <- tibble(vertex = unique(shiny_test$Project), type = "project")
    # 
    # node <- bind_rows(unique_names, unique_projects)
    
    net <- network(edge, vertex.attr = NULL,  matrix.type = "edgelist")
    
    
    #make function to take input and match each project with a corresponding color
    # name_choices <- c("Annie Carew", "Kiri Carini", "Jennifer Clapper", "Simon Costanzo", "Bill Dennison",
    #                   "Kelly Dobroski", "Caroline Donovan", "Joe Edgerton", "Andrew Elmore", "Alex Fries",
    #                   "Steven Guinn", "Heath Kelsey", "Katie May Laumann", "Nathan Miller", "Emily Nastase",
    #                   "Crystal Nichols", "Trish Summers", "Sky Swanson", "Dylan Taillie", "Vanessa Vargas-Nguyen",
    #                   "Suzi Webster")
    
    # project_choices <- c("RioGRC", "CBRC", "DarHRC", "SEACAR", "GeorRC", "edX")
    
    net %v% "color" = case_when(
      net %v% "vertex.names" == "Annie Carew" ~ "grey",
      net %v% "vertex.names" == "Kiri Carini" ~ "grey",
      net %v% "vertex.names" == "Jennifer Clapper" ~ "grey",
      net %v% "vertex.names" == "Simon Costanzo" ~ "grey",
      net %v% "vertex.names" == "Bill Dennison" ~ "grey",
      net %v% "vertex.names" == "Kelly Dobroski" ~ "grey",
      net %v% "vertex.names" == "Caroline Donovan" ~ "grey",
      net %v% "vertex.names" == "Joe Edgerton" ~ "grey",
      net %v% "vertex.names" == "Alex Fries" ~ "grey",
      net %v% "vertex.names" == "Steven Guinn" ~ "grey",
      net %v% "vertex.names" == "Heath Kelsey" ~ "grey",
      net %v% "vertex.names" == "Katie May Laumann" ~ "grey",
      net %v% "vertex.names" == "Nathan Miller" ~ "grey",
      net %v% "vertex.names" == "Emily Nastase" ~ "grey",
      net %v% "vertex.names" == "Crystal Nichols" ~ "grey",
      net %v% "vertex.names" == "Trish Summers" ~ "grey",
      net %v% "vertex.names" == "Sky Swanson" ~ "grey",
      net %v% "vertex.names" == "Dylan Taillie" ~ "grey",
      net %v% "vertex.names" == "Vanessa Vargas-Nguyen" ~ "grey",
      net %v% "vertex.names" == "Suzi Webster" ~ "grey",
      net %v% "vertex.names" == "Ricky Arnold" ~ "grey",
      net %v% "vertex.names" == "RioGRC" ~ "tomato3",
      net %v% "vertex.names" == "CBRC" ~ "skyblue",
      net %v% "vertex.names" == "DarHRC" ~ "sandybrown",
      net %v% "vertex.names" == "SEACAR" ~ "paleturquoise2",
      net %v% "vertex.names" == "GeorRC" ~ "plum2",
      net %v% "vertex.names" == "edX" ~ "lightgoldenrod2",
      net %v% "vertex.names" == "KwanRC" ~ "aquamarine",
      net %v% "vertex.names" == "MissRC" ~ "chartreuse2",
      net %v% "vertex.names" == "CGeoRC" ~ "brown3",
      net %v% "vertex.names" == "CAdaRC" ~ "lightsalmon1",
      net %v% "vertex.names" == "MDcbRC" ~ "maroon",
      net %v% "vertex.names" == "USGS" ~ "orangered2",
      net %v% "vertex.names" == "NOAA" ~ "steelblue4",
      net %v% "vertex.names" == "CMC" ~ "slateblue2",
      net %v% "vertex.names" == "IANsup" ~ "thistle2",
      net %v% "vertex.names" == "UMCES" ~ "royalblue4",
      net %v% "vertex.names" == "UofSC" ~ "peachpuff2",
      net %v% "vertex.names" == "Unfund" ~ "yellow2",
      net %v% "vertex.names" == "Class" ~ "orange"
      )
    
    
    
    
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
    
    
    Mon_exp <- reactive({as.character(values$df$Mon)})
    Tue_exp <- reactive({as.character(values$df$Tue)})
    Wed_exp <- reactive({as.character(values$df$Wed)})
    Thu_exp <- reactive({as.character(values$df$Thu)})
    Fri_exp <- reactive({as.character(values$df$Fri)})
    
    df_1 <- tibble(Mon = Mon_exp(),
                   Tue = Tue_exp(),
                   Wed = Wed_exp(),
                   Thu = Thu_exp(),
                   Fri = Fri_exp(),
                   Name = Name_exp())
    
    
    df_1 <- df_1 %>% 
      mutate(across(.cols = c(Mon, Tue, Wed, Thu, Fri),
                    .fns = ~case_when(.x == "All" ~ 1,
                                      .x == "AM" ~ 2,
                                      .x == "PM" ~ 6,
                                      .x == "Not" ~ 4))) %>%  
    mutate(image = case_when(Name == "Alex Fries" ~ here("data","AFries_headshot.jpg"),
                             Name == "Annie Carew" ~ here("data","ACarew_headshot.jpg"),
                             Name == "Andrew Elmore" ~ here("data","AElmore_headshot.jpg"),
                             Name == "Bill Dennison" ~ here("data", "BDennsion_headshot.jpg"),
                             Name == "Caroline Donovan" ~ here("data", "CDonovan_headshot.jpg"),
                             Name == "Crystal Nichols" ~ here("data", "CNichols_headshot_1.png"),
                             Name == "Dylan Taillie" ~ here("data","DTaillie_headshot.jpg"),
                             Name == "Heath Kelsey" ~ here("data", "HKelsey_headshot.jpg"),
                             Name == "Jennifer Clapper" ~ here("data", "JClapper_headshot_1.jpg"),
                             Name == "Joe Edgerton" ~ here("data", "2020_formal_pic_for_UMCES_website.jpg"),
                             Name == "Katie May Laumann" ~ here("data", "KMLaumann_headshot.jpg"),
                             Name == "Kiri Carini" ~ here("data", "KCarini_headshot.jpeg"),
                             Name == "Nathan Miller" ~ here("data", "NMiller_headshot_1.jpg"),
                             Name == "Ricky Arnold" ~ here("data", "RArnold_headshot.jpg"),
                             Name == "Simon Costanzo" ~ here("data", "SCostanzo_headshot.jpg"),
                             Name == "Steven Guinn" ~ here("data", "SGuinn_photo.jpg"),
                             Name == "Sky Swanson" ~ here("data","SSwanson_headshot.png"),
                             Name == "Suzi Webster" ~ here("data","SWebster_headshot.jpg"),
                             Name == "Trish Summers" ~ here("data", "TSummers_headshot_1.jpg"),
                             Name == "Vanessa Vargas-Nguyen" ~ here("data", "VVargas-Nguyen_headshot.jpg"),
                             Name == "Kelly Dobroski" ~ here("data", "KDobroski_headshot.jpg")))
    
    
    
    joined_df <- full_join(demo_x_test$data, df_1, by = "Name")
    
    output$network_plot <- renderPlot(
    
    demo_x_test +
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
      geom_image(data = joined_df, aes(x = x, y = y, image = image), size = 0.06, asp = 1.75) +
      geom_text(data = joined_df, aes(x = x, y = y,
                                                 label = case_when(color != "grey" ~ Name)))
    
    )
    })
    
    
    
    
    greeting_tibble <- tibble(Hello = "Please enter information")
    
  
  output$availability_table <- render_gt(
    if(is.null(values$df) == TRUE){
      gt(greeting_tibble) 
    } else {
      gt(values$df) %>% 
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
        
  # output$network_plot <- renderPlot(
  #   p1
  # )
  
  # output$schedule_table <- renderTable(schedule_times)   # joe says this is worthless


  output$Bill_text <- renderText(
    print("Bill is always available")
  )  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)







#cool features
#with selectizeInput use , options = list(create = TRUE) to allow user to write own tags







