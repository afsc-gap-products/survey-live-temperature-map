

USER <- reactiveValues(Logged = FALSE , session = session$user) 

# source("www/Login.R",  local = TRUE)

getDat <- eventReactive(input$search,{
  withProgress(
    message = 'Calculation in progress',
    detail = 'get iris data', value=0 , {
      
      setSpecies <- isolate(input$selectSpecies)
      
      incProgress(0.5)
      
      if (!is.null(setSpecies)) {
        Dat <- iris[which(iris$Species %in% setSpecies),]    
      } else {
        Dat <- NULL
      }
      
      setProgress(1)
    })
  return(Dat)
})  

output$obs <- renderUI({    
  if (USER$Logged == TRUE) {      
    list(
      selectizeInput(
        'selectSpecies', 'Select iris Species', choices = as.character(unique(iris$Species)), multiple = TRUE
      ),
      actionButton('search', 'Search')
    )
  }                 
})     

output$dataTable <- renderUI({    
  if (USER$Logged == TRUE) {      
    dataTableOutput('table')
  }
})

output$table <- renderDataTable(
  getDat(),
  options = list(
    pageLength = 100,
    lengthMenu = c(50,100,200,500)
  )                              
)

