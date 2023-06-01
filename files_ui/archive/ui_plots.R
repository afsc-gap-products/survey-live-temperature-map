### UI code for the 'eSDM GUI Roadmap and Load or Save Workspace' tab

ui.plots <- function() {
  tabItem(
    tabName = "plots",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      
      
      leafletOutput("mymap"),
      p(),
      actionButton("recalc", "New points"), 
      
      
      
      box(sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30) ), 
      box(selectInput(inputId = "color", 
                      label = "Choose a color", 
                      choices = c("red", "blue", "black", "pink"), 
                      selected = "blue") ), #dropdown
      box(plotOutput("distPlot") )
    )
  )
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

