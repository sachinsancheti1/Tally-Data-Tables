library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Tally Data Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel({
    tagList(
      fileInput('xmlfile',"Upload the Tally XML file"),
      tags$hr(),
      uiOutput("displayside"),
      uiOutput("displaydownloadhandler")
    )
  }),
  mainPanel(uiOutput("displaymain"))
))
