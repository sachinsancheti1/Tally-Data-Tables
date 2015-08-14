library(XML)
library(dplyr)
library(shiny)
options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output) {
  output$displayside <- renderUI({
    if(is.null(input$xmlfile))
      return()
    xmltop = datasetInput()
    nam.summary = datasetSummary(xmltop)$summary
      selectInput('tablestodis',label = "Tables to display",
                       choices = levels(nam.summary$name))
    
  })
  output$displaydownloadhandler <- renderUI({
    if(is.null(input$xmlfile))
      return()
    downloadButton('downloadData', 'Download')
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("Insight", '.txt', sep='') },
    content = function(file) {
      write.table(gettable(input$tablestodis), file)
    }
  )
  
  output$displaymain <- renderUI({
    if(is.null(input$xmlfile))
      return(h5("Please upload the Tally xml master export file"))
    selectedtabs <- input$tablestodis
    tabPanel(title = selectedtabs,tableOutput('tk'))
    })
  
  output$tk <- renderTable({
    gettable(input$tablestodis)
  })
  
  datasetInput <- reactive({
    if(is.null(input$xmlfile))
      return()
    thFile <- input$xmlfile
    xmlfile=xmlParse(thFile$datapath)
    xmltop = xmlRoot(xmlfile)
    xmltop
  })
  
  gettable <- function(x){
    xmltop <- datasetInput()
    nam <- datasetSummary(xmltop)$table
    rt = as.numeric(rownames(nam[nam$name ==x,]))
    tlist = character(0)
    for(i in 1:length(rt))({
      jatt = as.data.frame(xmlToDataFrame(xmlChildren(xmltop[[2]][[1]][[2]][[rt[i]]])))
      tlist = c(tlist,names(jatt))
      rm(jatt)
    })
    tlist.n = tlist %>% unique
    master.table = matrix(numeric(0),ncol = length(tlist.n))
    colnames(master.table) <- as.vector(tlist.n)
    for(i in 1:length(rt))({
      jatt = as.data.frame(xmlToDataFrame(xmlChildren(xmltop[[2]][[1]][[2]][[rt[i]]])))
      names(jatt) = make.names(names(jatt))
      master.table = rbind_list(master.table,jatt,all = TRUE)
      rm(jatt)
    })
    is.na(master.table) = "_"
    master.table
  }
  
  datasetSummary <- function(x)({
    jt = sum(sapply(xmlChildren(x[[2]][[1]][[2]]), xmlSize))
    nam = data.frame(name = character(0),size = numeric(0))
    for(i in 1:jt)({
      jatt = data.frame(name = as.vector(sapply(xmlChildren(x[[2]][[1]][[2]][[i]]), xmlName)),
                        size = as.vector(sapply(xmlChildren(x[[2]][[1]][[2]][[i]]), xmlSize)))
      nam = rbind(nam,jatt)
      rm(jatt)
    })
    nam.summary = nam %>% group_by(name) %>% summarise(total = n())
    newList = list("items"=jt,"table" = nam,"summary" = nam.summary)
    return(newList)
  })
})
