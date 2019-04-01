library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose CSV file',
                accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      conditionalPanel(
        # use a server side condition
        condition = "output.fileUploaded",
        # placeholders will be replaced from the server
        selectInput("country_input", "Select a cunttry", "placeholder 1")
        #selectInput("level", "select level", "placeholder 2")
      )
    ),
    mainPanel(
      h3("Field Guides by Country"),
      plotOutput("PieChart"),
      tableOutput("summary_table"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session){
  # create reactive version of the dataset (a data.frame object)
  filedata <- reactive({
    req(input$datafile)
    infile <- input$datafile
    if (is.null(infile))
      # User has not uploaded a file yet. Use NULL to prevent observeEvent from triggering
      return(NULL)
    temp <- read.csv(infile$datapath)
    temp[order(temp[, 1]),]
  })
  
  # inform conditionalPanel wheter dropdowns sohould be hidden
  output$fileUploaded <- reactive({
    return(!is.null(filedata()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  # update the selectInput elements according to the dataset
  
  ## update 'country_input' selector
  observeEvent(filedata(), {
    updateSelectInput(session, "country_input", choices = unique(filedata()$country))
  })
  
  #show a pie chart 
  output$PieChart<-renderPlot({
    filedata() %>%subset( filedata()$country == input$country_input) %>%
      select(country, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))%>%
      ggplot(aes(x="", y=Proportion, fill=Category))+geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
  })
  
  # show summary table (percentge)
  output$summary_table <- renderTable({
    filedata() %>%subset( filedata()$country == input$country_input) %>%
      select(country, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))
  }, bordered = TRUE)


  # show table
  output$table <- renderTable({
    filedata() %>%subset( filedata()$country == input$country_input)
  }, bordered = TRUE)
}
 

shinyApp(ui, server)
