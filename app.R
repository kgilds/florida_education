#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(httr)
library(janitor)




url <- 'http://www.fldoe.org/core/fileparse.php/7584/urlt/1516ABS21DAYSchool.xls'

GET(url, write_disk(absences <- tempfile(fileext = ".xls")))
 
absences_2 <- read_excel(absences, skip =2)

absences_2 <- clean_names(absences_2)

names(absences_2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Florida School Absences"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("school_district",
                     "School District",
                     c(unique(as.character(absences_2$district_name))))),
      
      # Show a plot of the generated distribution
      mainPanel(
         
        tabsetPanel(type = "tabs",
                    tabPanel("Data", tableOutput("test")),
                    tabPanel("Absences", tableOutput("avg_abs"))
                    
        
      )
   )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #absences_2$enrollments <- as.numeric(absences_2$enrollments)
  #absences_2$absent_21_plus <- as.numeric(absences_2$absent_21_plus)
   
   output$test <- renderTable({
      # generate bins based on input$bins from ui.R
      test  <- filter(absences_2, district_name == input$school_district)
      test <- rename(test, "absent_21_plus" = "absent_21_days_or_over")
      test <- select(test, 2, 4:6)
      test$enrollments <- as.numeric(test$enrollments)
      test$absent_21_plus <- as.numeric(test$absent_21_plus)
      test <- mutate(test, percent = absent_21_plus / enrollments)
      
      
      
   })
   
   output$avg_abs <- renderTable({
     
  avg_abs <- filter(test, district_name == input$school_district)
  avg_abs <- group_by(avg_abs, district_name)
  avg_abs <- summarise(avg_abs, mean = mean('percent', na.rm = TRUE))
     
  avg_abs
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

