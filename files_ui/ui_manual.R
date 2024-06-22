

ui.manual <- function() {
  tabItem(
    tabName = "manual",
    HTML("<html lang='en'>"), #Always have this as your first line
    
    fluidRow(
      h1("NOAA Branding Guide"),
      
              box(p("Let's look at the NOAA Branding Guide.")#, 
                  # tags$iframe(style="height:400px; width:100%; scrolling=yes",
                  #             src=paste0("Brand JUNE2019.pdf")
                              # )
                  
              ))
  )
}