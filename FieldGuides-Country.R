#Field Guides by Country

#Pakcages----
library(shiny)

#Data 


#UI----
ui <- fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      #select box
      selectInput("country_input", "Select a Country", choices = unique(file1$Category)),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      tags$hr(),
      p('Please upload files downloaded from Drupal'
      )
    ),
    mainPanel(
      #to show the contents of the imported file 
      #tableOutput('contents')
      
      #to show percent summary table
      tableOutput('summary')
    )
  )
)

# By default, the file size limit is 5MB. It can be changed by
# setting this opztion. Here we'll raise limit to 9MB.
  options(shiny.maxRequestSize = 9*1024^2)
  
#Server----
server<-function(input, output, session) {

  #Reactive value for selected dataset
  dataInput<-reactive({
    get(input$dataInput)
  })
  observe({
    updateSelectInput(session, "country_input", choices = unique(dataInput()$country))
    
  })
  #to create a summary table
  output$summary_table <-renderTable ({
    summary<-dataInput()
    if(is.null(summary))
      return(Null)
  })
  
}
  
#To createa table of the file content
# output$contents <- renderTable({
#   inFile <- input$file1
# 
#   if (is.null(inFile))
#     return(NULL)
#   
#   read.csv(inFile$datapath, header = input$header,
#            sep = input$sep)
# })
#Run Shiny app
shinyApp(ui, server)
