### Load Libaries####

library(shiny)
library(shinyLP)
library(shinythemes)
library(tidyverse)
library(readxl)
library(httr)
library(janitor)
library(formattable)
library(DT)

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
#absences_2 <- dplyr::mutate(absences_2, percent = percent *100) #Convert from decimal 



# Define UI for application that draws a histogram


ui <- fluidPage(theme = shinytheme("spacelab"),
  
  navbarPage("Florida School Absences",
  
  tabPanel("About",
           
           #list_group("Kevin", "Kevin"),u 
           
           shinyLP::panel_div(class_type = "primary", panel_title = "Maintainer", content = 
                                
                              tags$a(href="https://kgilds.rbind.io/about/", "Kevin Gilds, MPA", br (),
                              tags$a(href="mailto:kevingilds@gmail.com", "Contact"))),
           
                                
           shinyLP::panel_div(class_type= "primary", panel_title = "About", content = " <ui> <li>The Florida Department of Education maintains a spreadsheet on their website that contains the enrollment of schools and a count of students who missed more than 21 days of school. </li>  <li> This Application takes that static spreadsheet from the Florida Department of Education and makes it interactive. The purpose of the application is to make it easier to browse the data by school district and school type. </li> <li> The data displayed in the application consists of the district name, school name, student enrollments, a count and percent of students who missed 21 days or more of school during the 2015-2016 school year. The percent variable is calculated by the application. </li>  <li> The original spreadsheet file displayed an * for schools with an enrollment with 10 or less students--in this application those schools are excluded. </li> <ui/>" ),
           
          
           shinyLP::panel_div(class_type = "primary", panel_title = "Data Source", content =
                                
                                tags$a(href="http://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/students.stml", "Florida Department of Education Website")),
           
            shinyLP::panel_div(class_type = "primary", panel_title = "How to use", content = "<ol typ='1'> <li>Click on the data link above and all the schools with data from the Alachua School District will display. </li> <li>  Choose to view different types of school by selecting the tab you want review--Elemantary, Middle, and High. </li> <li>To change the school district, select the school district from the drop down menu on the top left corner of the data application. </li> <ol/>")
           
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
                           tabPanel("All Schools", DT::dataTableOutput("all")),
                           tabPanel("Elementary Schools", DT::dataTableOutput("elem")),
                           tabPanel("Middle Schools", DT::dataTableOutput("middle")),
                           tabPanel("High Schools", DT::dataTableOutput("high"))
                           
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
  
  
  
   
   output$all <- renderDataTable({
      
      all<- dataInput()
      
      #test$percent <- percent(test$percent)
      
      #dplyr::arrange(all, desc(percent))
      
      
      DT::datatable(all, rownames = FALSE,  options =  list(order = list(list(4, "desc")))) %>%
      
      
        formatPercentage('percent', 2) 
        
      
      
      #datatable(head(mtcars, 30), options = list(
        #order = list(list(2, 'asc'), list(4, 'desc'))
      #))
      
      #arrange(flights, year, month, day)
      
   })
   
   
   
  output$elem <- renderDataTable({
    dat <- dataInput()
    
    
      
    
    
    elem <- dat[grep("ELEMENTARY SCHOOL", dat$school_name, ignore.case = TRUE, fixed = TRUE),]
    
    elem <- DT::datatable(elem, rownames = FALSE, options= list(order =list(4, "desc"))) %>%
    
   
    formatPercentage('percent', 2)
    
    
    #dplyr::arrange(elem, desc(percent))
    
    
  })
  
  output$middle <- DT::renderDataTable({
    dat1 <- dataInput()
    middle <- dat1[grep("MIDDLE SCHOOL", dat1$school_name, ignore.case = TRUE, fixed = TRUE),]
  
    middle <- DT::datatable(middle, rownames = FALSE, options = list(order=list(4, 'desc'))) %>%
      formatPercentage('percent',2)
    
    #dplyr::arrange(middle, desc(percent))
    })
  
  output$high <- DT::renderDataTable({
    dat2 <- dataInput()
    high <- dat2[grep("HIGH SCHOOL", dat2$school_name, ignore.case = TRUE, fixed = TRUE),]
    
    high <- DT:: datatable(high, rownames= FALSE, options = list(order=list(4, "desc"))) %>%
      formatPercentage('percent', 2)
    
    #dplyr::arrange(high, desc(percent))
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

