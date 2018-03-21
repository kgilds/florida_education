### Load Libaries####

library(shiny)
library(shinyLP)
library(tidyverse)
library(readxl)
library(httr)
library(janitor)
library(formattable)


############################Data Set up with Scripts#########################################

url <- 'http://www.fldoe.org/core/fileparse.php/7584/urlt/1516ABS21DAYSchool.xls' #location of data on the internet


httr::GET(url, write_disk(absences <- tempfile(fileext = ".xls"))) #obtain the spreadsheet file from the internet

absences_2 <- readxl::read_excel(absences, skip =2) #read the spreadsheet file and skip the first two rows

absences_2 <- janitor::clean_names(absences_2) #Change column names with the Janitor.


absences_2 <- dplyr::rename(absences_2, "absent_21_plus" = "absent_21_days_or_over") #Change name again to shorten 

absences_2 <- dplyr::select(absences_2, 2, 4:6) #select columsn of interest

absences_2$enrollments <- as.numeric(absences_2$enrollments) #Change data to numeric format
absences_2$absent_21_plus <- as.numeric(absences_2$absent_21_plus) #change data to numeric format

absences_2 <- dplyr::mutate(absences_2, percent = absent_21_plus / enrollments) #calculate percent 
absences_2 <- dplyr::mutate(absences_2, percent = percent *100) #Convert from decimal 



# Define UI for application that draws a histogram


ui <- fluidPage(
  
  navbarPage("Florida School Absences",
  
  tabPanel("About",
           
           #list_group("Kevin", "Kevin"),u 
           
           shinyLP::panel_div(class_type = "primary", panel_title = "Maintainer", content = 
                                
                              tags$a(href="https://kgilds.rbind.io/about/", "Kevin Gilds, MPA", br (),
                              tags$a(href="mailto:kevingilds@gmail.com", "Contact"))),
           
                                
           shinyLP::panel_div(class_type= "primary", panel_title = "About", content = "This Application takes a static spreadsheet from the Florida Department of Education and makes it dynamic. The purpose of the application is to make it easier to browse the data by school district and school type.The data displayed in the application consists of the district name, school name,  student enrollments, a count and percent of students who missed 21 days or more of schoolduring the 2015-2016 School year, and a percentage. The percent variable is calculated by the application. The spreadsheet displayed an * for schools with an enrollment with 10 or less students--in this application those schools are excluded."),
           
          
           shinyLP::panel_div(class_type = "primary", panel_title = "Data Source", content =
                                
                                tags$a(href="http://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/students.stml", "Florida Department of Education Website")),
           
            shinyLP::panel_div(class_type = "primary", panel_title = "How to use", content = "Click on the data tab to right and all the schools with data from the Alachua School District will emerge. To change the school district select the school district from the drop down menu on the top right of the application. The application also subsets school types by elementary schools, middle schools, and high schools. The data is presented with the variable percent in descending order so you may see the schools with the highest percent of students with 21 or more absences.")
           
           ),
           
          
          
  
  tabPanel("Data",
           
           fluidPage(
             
           # Sidebar to select the school district
           sidebarLayout(
             sidebarPanel(
               selectInput("school_district",
                           "School District",
                           c(unique(as.character(absences_2$district_name))))),
             
             mainPanel(
               
               tabsetPanel(type = "tabs",
                           tabPanel("All Schools", tableOutput("all")),
                           tabPanel("Elementary Schools", tableOutput("elem")),
                           tabPanel("Middle Schools", tableOutput("middle")),
                           tabPanel("High Schools", tableOutput("high"))
                           
               )#tabsetpanels
               
           
           
             
             ) #mainPanel 
             
           )#fluidpage 
           
 
  
  )
          
    )
) 
 
          
       
    )
 




server <- function(input, output) {
  
  ## Make School District Reactive
  dataInput <- reactive ({
    dplyr::filter(absences_2, district_name == input$school_district)
    
    
  })
  
  
  
   
   output$all <- renderTable({
      # generate bins based on input$bins from ui.R
      #test  <- filter(absences_2, district_name == input$school_district)
     
      all<- dataInput()
      
      #test$percent <- percent(test$percent)
      
      dplyr::arrange(all, desc(percent))
      
      #arrange(flights, year, month, day)
      
   })
   
   
   
  output$elem <- renderTable({
    dat <- dataInput()
    elem <- dat[grep("ELEMENTARY SCHOOL", dat$school_name, ignore.case = TRUE, fixed = TRUE),]
      
    dplyr::arrange(elem, desc(percent))
  })
  
  output$middle <- renderTable({
    dat1 <- dataInput()
    middle <- dat1[grep("MIDDLE SCHOOL", dat1$school_name, ignore.case = TRUE, fixed = TRUE),]
  
    dplyr::arrange(middle, desc(percent))
    })
  
  output$high <- renderTable({
    dat2 <- dataInput()
    high <- dat2[grep("HIGH SCHOOL", dat2$school_name, ignore.case = TRUE, fixed = TRUE),]
    
    dplyr::arrange(high, desc(percent))
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

