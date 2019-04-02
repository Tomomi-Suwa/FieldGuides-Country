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
        # placeholders("Country") will be replaced from the server
        selectInput("country_input", "Select a country", "a Selected Country")
      )
    ),
    mainPanel(
      h2(textOutput("title")),
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
    updateSelectInput(session, "country_input", choices = sort(unique(filedata()$Countries)))
  })
  
  #add a reactive title
  output$title <- renderText({ 
    paste("Field Guides in", input$country_input)
  })
  
  #show a pie chart 
  output$PieChart<-renderPlot({
    filedata() %>%subset( filedata()$Countries == input$country_input) %>%
      select(Countries, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))%>%
      #as.factor(Category)
      ggplot(aes(x="", y=Proportion, fill=Category))+ geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)%>%
      scale_fill_manual(values = c("Plants" ="green", "Birds" = "purple", "Fishes"="blue", "Herp" = "brown", "Insects" = "yellow","Mammals" = "orange", "Other" = "pink"))
  })
  
  # show summary table (pefrcentge)
  output$summary_table <- renderTable({
    filedata() %>%subset( filedata()$Countries == input$country_input) %>%
      select(Countries, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))
  }, bordered = TRUE)


  # show table
  output$table <- renderTable({
    filedata() %>%subset( filedata()$Countries == input$country_input)
  }, bordered = TRUE)
}
 

shinyApp(ui, server)
