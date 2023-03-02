ui.glossary <- function() {
  tabItem(
    tabName = "glossary",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      
               h1("Abbreviations, Acronyms, Symbols, and Glossary"),
               column(3, wellPanel(
                 
                 h3("Abbreviations"),
                 DT::dataTableOutput("acronyms")
               )),
               column(9, wellPanel(
                 
                 h3("Glossary"),
                 DT::dataTableOutput("gloss")
               )),
      
      p("AFSC NOAA Technical Memorandum describe the origination and post-survey processing of the data and the methods used to produce the water temperature statistics for each station and the water structure indices for each year."), 
               
               column(12, wellPanel(
                 h3("Literature Cited"),
                 DT::dataTableOutput("infot2")
               ))
      )
    )
}