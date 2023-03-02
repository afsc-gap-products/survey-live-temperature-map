### UI code for the 'eSDM GUI Roadmap and Load or Save Workspace' tab

ui.about <- function() {
  tabItem(
    tabName = "about",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      h1("Surveys Conducted in Alaska by AFSC"), 
      p("For general exploration... for more specific needs or collaboration, please contact..."),
      br(),
      h2("Northern and Eastern Bering Sea"), 
      br(), 
      p("Each year since 1982 the RACE Groundfsh Assessment Program has conducted a trawl survey in the eastern Bering Sea at which time water column temperature data was collected. To better utilize this data in biological comparisons, statistics were calculated from each water column sample. The following links contain all the information:"), 
      img(src="survey_ebs.jpg", width = '35%'),
      
      br(), 
      
      h2("Gulf of Alaska"), 
      
      br(), 
      
      h2("AI")

          )
      )
}


