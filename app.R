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



url <- 'http://www.fldoe.org/core/fileparse.php/7584/urlt/1516ABS21DAYSchool.xls'


GET(url, write_disk(absences <- tempfile(fileext = ".xls")))

absences_2 <- read_excel(absences, skip =2)

absences_2 <- clean_names(absences_2)


absences_2 <- rename(absences_2, "absent_21_plus" = "absent_21_days_or_over")

absences_2 <- select(absences_2, 2, 4:6)

absences_2$enrollments <- as.numeric(absences_2$enrollments)
absences_2$absent_21_plus <- as.numeric(absences_2$absent_21_plus)
absences_2 <- mutate(absences_2, percent = absent_21_plus / enrollments)



# Define UI for application that draws a histogram


ui <- fluidPage(
  
  navbarPage("Florida School Absences",
  
  tabPanel("About",
  wells("tell me")    
           ),
 
  tabPanel("Data",
           
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
                       
           )
          
  )
          
  )
 
 
          
   
 
  
  
  
  
   
      # Show a plot of the generated distribution
      
       
   )
)
)




server <- function(input, output) {
  
  
  
  #currentFib  <- reactive({ fib(as.numeric(input$n)) })
  
  #dat <- reactive({filter(absences_2, district_name == input$school_district)})
  
  
  dataInput <- reactive ({
    filter(absences_2, district_name == input$school_district)
    
    
  })
  
  

   
   output$test <- renderTable({
      # generate bins based on input$bins from ui.R
      #test  <- filter(absences_2, district_name == input$school_district)
     
      test <- dataInput()
      
      
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
    elem <- dat[grep("ELEMENTARY ", dat$school_name, ignore.case = TRUE, fixed = TRUE),]
      
    arrange(elem, desc(percent))
  })
  
  output$middle <- renderTable({
    dat1 <- dataInput()
    middle <- dat1[grep("MIDDLE", dat1$school_name, ignore.case = TRUE, fixed = TRUE),]
  
    arrange(middle, desc(percent))
    })
  
  output$high <- renderTable({
    dat2 <- dataInput()
    high <- dat2[grep("HIGH", dat2$school_name, ignore.case = TRUE, fixed = TRUE),]
    
    arrange(high, desc(percent))
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

