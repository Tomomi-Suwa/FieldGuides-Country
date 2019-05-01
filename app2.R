library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(DT)

filedata<- read.csv("FG_Category_Country_cleaned_3.12.2019V2.csv")

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      # add select
      selectInput("country", "Select a Country", choices =sort(unique(filedata$Countries))
      ),
      br(),
      h4(textOutput("total")),
      h4("Summary by Category"),
      tableOutput("summary_table")
    ),
    mainPanel(
      h3(textOutput("title")),
      plotOutput("PieChart"),
      h4("Summary Table"),
      DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output, session){
  #Reactive value for selected dataset
  datasetInput<-reactive({
    req(input$country)
    filter(filedata, Countries %in% input$country)
    
  })
  #add a reactive title
  output$title <- renderText({ 
    paste("Summary of Field Guides in", input$country)
  })
  
  #add total number of UNIQUE field guides
  output$total <- renderText({ 
    paste("Total Number: ",
          datasetInput() %>%subset(Countries == input$country)%>%
            distinct(guide_no)%>%
            nrow()
    )
  })  
  #show a pie chart 
  output$PieChart<-renderPlot({
    datasetInput() %>%
      select(Countries, Category) %>%
      #mutate_at(var(Countries,Category), factor)%>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))%>%
      ggplot(aes(x="", y=Proportion, fill=Category))+ geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
      scale_fill_manual(values = c("Plants" ="seagreen2", "Birds" = "mediumpurple3", "Fishes"="deepskyblue3", 
                                   "Herp" = "cyan3", "Insects" = "goldenrod2","Mammals" = "darkorange2",
                                   "Fungi"= "lightgoldenrod4", "Other" = "palevioletred2")) +
      theme_bw() + theme(legend.text=element_text(size=rel(1.2))) +theme(legend.title=element_text(size=15))
  }) 
  
  # show summary table (percentage)
  output$summary_table <- renderTable({
    datasetInput() %>%select(Countries, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))
  }, bordered = TRUE)
  
  #show table
  #to make active hyperlinks: https://stackoverflow.com/questions/30901027/convert-a-column-of-text-urls-into-active-hyperlinks-in-shiny
  output$table <- DT::renderDataTable({
    datasetInput() %>%
      dplyr::select(-category,-country,-state, -language, -date_created) %>%
      dplyr::mutate(URL = paste0("https://fieldguides.fieldmuseum.org/guides/guide/", guide_no)) %>%
      dplyr::mutate(title = paste0("<a href='", URL, "'>",guide_title,"</a>")) %>% 
      dplyr::select(title, guide_no, page_no,Category) %>% 
      dplyr :: rename(Title = title, Guide_No = guide_no,Page_Number = page_no)
  }, escape = FALSE)
}

shinyApp(ui, server) 