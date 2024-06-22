ui.metadata <- function() {
  tabItem(
    tabName = "metadata",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      
      # HTML("<h1>Hi this is heading<span class='glyphicon glyphicon-star'></span> Star</h1>"),
      img(src="FISHERIES-Logo WEB ONLY.png", width = '35%'),
      br(),
      
      h1(paste0("AFSC/RACE: Alaska Groundfish Survey ", min(dat$year), " - ", max(dat$year))),
      br(),
      
      
      h2("Cite this tool"),
      p(paste0('Data last updated ', lastdl,
                              ' and this app was last updated ',
                              format(Sys.Date(), format='%B %d %Y'),'.')), 
      p("CITE"),
      br(),
      
      # br(), 
      # p("To check out code used to create this tool:")
      # pre("Text ‘as is’ in a fixed width font."), 
      # br(), 
      # img(src="FISHERIES-Logo WEB ONLY.png", width = '35%'), 
      
      h2("Abstract"), 
      p("The Resource Assessment and Conservation Engineering Division (RACE) of the Alaska Fisheries Science Center (AFSC) conducts bottom trawl surveys to monitor the condition of the demersal fish and crab stocks of Alaska. These data include catch per unit effort for each identified species at a standard set of stations. This is a subset of the main racebase datase. Excluded are non standard stations, earlier years using different gear, and other types of data collected other than species id, species weight, water temperature and depth."), 
      
      h2("Purpose"), 
      p("The data set was developed to describe the temporal distribution and abundance of commercially and ecologically important groundfish species, examine the changes in the species composition of species over time and space, and describe the physical environment of the groundfish habitat."), 
      
      h2("Contact us"), 
      p(strong("Email: "), "[email]"), 
      p(strong("Telephone: "), "[phone number]"), 
      br(), 
      # p("Jeremy Mays or Nancy Robeson?"),
      br(), 
      p("NOAA Fisheries, Alaska Fisheries Science Center"), 
      p("Resource Assessment and Conservation Engineering Division"), 
      p("7600 Sand Point Way NE, Bldg. 4"), 
      p("Seattle, WA 98115"), 
      br(), 
      
      
      h2("Supplemental Links"), 
      a(href = 'https://www.fisheries.noaa.gov/', "NOAA Fisheries"), 
      br(),
      a(href = "https://www.fisheries.noaa.gov/about/resource-assessment-and-conservation-engineering-division", "AFSC RACE Division"), 
      br(), 
      a(href = 'https://github.com/EmilyMarkowitz-NOAA/AFSCRACE_SurveyDataMapApp', "GitHub Repository"), 
      br()
    )
    
  )
}