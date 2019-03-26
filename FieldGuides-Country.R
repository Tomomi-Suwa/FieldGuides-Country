#Field Guides by Country

#Pakcages----
library(shiny)

#Data 


#UI----
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

#Server#####
server <- function(input, output) { }

#Run Shiny app
shinyApp(ui, server)
