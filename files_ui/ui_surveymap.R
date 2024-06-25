### UI code for the 'eSDM GUI Roadmap and Load or Save Workspace' tab

ui.surveymap <- function() {
  tabItem(
    tabName = "surveymap",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      div(class="outer",
          
          tags$head(
            # Include our custom CSS
            includeCSS(here::here("files_support", "styles.css")),
            includeScript(here::here("files_support", "gomap.js"))
          ),
          
          # If not using custom CSS, set height of leafletOutput to a number instead of percent
          leafletOutput("survey_leaflet", width="100%", height="95%"),
          
          absolutePanel(id = "controls", 
                        class = "panel panel-default", 
                        fixed = TRUE,
                        draggable = TRUE, top = 60, 
                        left = "auto", right = 20, 
                        bottom = "auto",
                        width = 330, height = "auto",
                        
                        br(),
                        
                        selectInput(inputId = "year", 
                                    label = "Year", 
                                    choices = sort(unique(dat$year)), 
                                    selected = max(dat$year), 
                                    multiple = FALSE),
                        selectInput(inputId = "survey", 
                                    label = "Survey", 
                                    choices = sort(unique(shp_surv$SRVY))[sort(unique(shp_surv$SRVY)) != "NEBS"], 
                                    selected = c("EBS", "NBS"), 
                                    multiple = TRUE),
                        checkboxInput(inputId = "station", 
                                      label = "Station Points", 
                                      value = FALSE),
                        checkboxInput(inputId = "stratum", 
                                      label = "Stratum", 
                                      value = FALSE), 
                        checkboxInput(inputId = "vessel", 
                                      label = "Vessels", 
                                      value = FALSE), 
                        br(),
                        
                        selectInput(inputId = "plot_unit",
                                    label = "Environmental Variable",
                                    choices = c(
                                      "Bottom Temperature (°C)" = "bottom_temperature_c", 
                                      "Surface Temperature (°C)" = "surface_temperature_c", 
                                      "None" = "none"),
                                    selected = "none"),
                        uiOutput("plot_display"),
                        # uiOutput("vessels"),
                        uiOutput("plot_dates"),
                        # actionButton("download_pdf", "Download .pdf"), 
                        downloadButton(outputId = "dl_map",
                                       label = "Download Map (PNG)")
          )
      )
    )
    
  )
  
}


