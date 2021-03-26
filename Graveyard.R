#Junk/old stuff

summarise(Project = str_c(Project, collapse = ", "))

%>% 
  group_by(Stress, Monday, Tuesday, Wednesday, Thursday, Friday, Project, Comments) %>% 
  summarise(Project = toString(Project)) %>% 
  ungroup() #trying to figure out how to collapse rows


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
