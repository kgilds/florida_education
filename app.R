#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyLP)
library(tidyverse)
library(readxl)
library(httr)
library(janitor)
library(formattable)



url <- 'http://www.fldoe.org/core/fileparse.php/7584/urlt/1516ABS21DAYSchool.xls'


GET(url, write_disk(absences <- tempfile(fileext = ".xls")))

absences_2 <- read_excel(absences, skip =2)

absences_2 <- clean_names(absences_2)


absences_2 <- rename(absences_2, "absent_21_plus" = "absent_21_days_or_over")

absences_2 <- select(absences_2, 2, 4:6)

absences_2$enrollments <- as.numeric(absences_2$enrollments)
absences_2$absent_21_plus <- as.numeric(absences_2$absent_21_plus)
absences_2 <- mutate(absences_2, percent = absent_21_plus / enrollments)
absences_2 <- mutate(absences_2, percent = percent *100)

meta <- read_excel(absences, range = "A1:A2")


# Define UI for application that draws a histogram


ui <- fluidPage(
  
  navbarPage("Florida School Absences",
  
  tabPanel("About",
           
           #list_group("Kevin", "Kevin"),u 
           
           shinyLP::panel_div(class_type = "primary", panel_title = "Maintainer", content = 
                                
                              tags$a(href="https://kgilds.rbind.io/about/", "Kevin Gilds, MPA", br (),
                              tags$a(href="mailto:kevingilds@gmail.com", "Contact"))),
           
                                
           shinyLP::panel_div(class_type= "primary", panel_title = "About", content = "This Application takes a static spreadsheet from the Florida Department of Education and makes it dynamic. The purpose of the application is to make it easier to browse the data.  The data displayed in the application consists of school enrollments, a count of students who missed 21 days or more of school during the 2015-2016 School year, and a percentage. The percent variable is calculated by the application. The spreadsheet displayed an * for schools with an enrollment with 10 or less students--in this application those schools are excluded."),
           
          
           shinyLP::panel_div(class_type = "primary", panel_title = "Data Source", content =
                                
                                tags$a(href="http://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/students.stml", "Florida Department of Education Website")),
           
            shinyLP::panel_div(class_type = "primary", panel_title = "How to use", content = "Click on the data tab to right and all the schools with data from the Alachua School District will emerge. To change the school district select the school district from the drop down menu on the top right of the application. The application also subsets school types by elementary schools, middle schools, and high schools. The data is presented with the variable percent in descending order so you may see the schools with the highest percent of students with 21 or more absences.")
           
           ),
           
          
          
  
  tabPanel("Data",
           
           fluidPage(
             
           # Sidebar with a slider input for number of bins 
           sidebarLayout(
             sidebarPanel(
               selectInput("school_district",
                           "School District",
                           c(unique(as.character(absences_2$district_name))))),
             
             mainPanel(
               
               tabsetPanel(type = "tabs",
                           tabPanel("All Schools", tableOutput("test")),
                           tabPanel("Elementary Schools", tableOutput("elem")),
                           tabPanel("Middle Schools", tableOutput("middle")),
                           tabPanel("High Schools", tableOutput("high"))
                           
               )#tabsetpanels
               
           
           
             
             ) #mainPanel 2
             
           )#fluidpage 
           
 
  
  )
          
    )
) 
 
          
       
    )
 








server <- function(input, output) {
  
  
  
  #currentFib  <- reactive({ fib(as.numeric(input$n)) })
  
  #dat <- reactive({filter(absences_2, district_name == input$school_district)})
  
  
  dataInput <- reactive ({
    filter(absences_2, district_name == input$school_district)
    
    
  })
  
  
  output$meta <- renderTable({
    
    meta
    
  })
   
   output$test <- renderTable({
      # generate bins based on input$bins from ui.R
      #test  <- filter(absences_2, district_name == input$school_district)
     
      test <- dataInput()
      
      #test$percent <- percent(test$percent)
      
      arrange(test, desc(percent))
      
      #arrange(flights, year, month, day)
      
   })
   
   output$avg_abs <- renderTable({
     
  avg_abs <- dataInput()
  avg_abs$enrollments <- as.numeric(avg_abs$enrollments)
  avg_abs <- group_by(avg_abs, district_name)
  avg_abs <- summarise(avg_abs, mean = mean(percent, na.rm = TRUE))
     
  
  
   })
   
  output$s_avg_abs <- renderTable({
    
    s_avg_abs <- dataInput()
    s_avg_abs <- group_by(s_avg_abs, school_name)
    s_avg_abs <- summarise(s_avg_abs, mean = mean(percent, na.rm = TRUE))
  
    
  })
   
  output$elem <- renderTable({
    dat <- dataInput()
    elem <- dat[grep("ELEMENTARY SCHOOL", dat$school_name, ignore.case = TRUE, fixed = TRUE),]
      
    arrange(elem, desc(percent))
  })
  
  output$middle <- renderTable({
    dat1 <- dataInput()
    middle <- dat1[grep("MIDDLE SCHOOL", dat1$school_name, ignore.case = TRUE, fixed = TRUE),]
  
    arrange(middle, desc(percent))
    })
  
  output$high <- renderTable({
    dat2 <- dataInput()
    high <- dat2[grep("HIGH SCHOOL", dat2$school_name, ignore.case = TRUE, fixed = TRUE),]
    
    arrange(high, desc(percent))
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

