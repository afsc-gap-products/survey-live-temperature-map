### UI code for the 'eSDM GUI Roadmap and Load or Save Workspace' tab

ui.surveymap <- function() {
  tabItem(
    tabName = "surveymap",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      div(class="outer",
          
          tags$head(
            # Include our custom CSS
            includeCSS("/code/styles.css"),
            includeScript("/code/gomap.js")
          ),
          
          # If not using custom CSS, set height of leafletOutput to a number instead of percent
          leafletOutput("survey_leaflet", width="100%", height="95%"),
          
          # Shiny versions prior to 0.11 should use class = "modal" instead.
          absolutePanel(id = "controls", 
                        class = "panel panel-default", 
                        fixed = TRUE,
                        draggable = TRUE, top = 60, 
                        left = "auto", right = 20, 
                        bottom = "auto",
                        width = 330, height = "auto",
                        
                        br(),

                        sliderInput("year", "Year:", 
                                    min = min(dat_cpue$year, na.rm = TRUE), 
                                    max = max(dat_cpue$year, na.rm = TRUE), 
                                    value = max(dat_cpue$year, na.rm = TRUE),#), 
                                    step = 1, 
                                    sep = "",
                                    animate = TRUE),
                        selectInput("common", "Species", 
                                    choices = c(sort(unique(dat_cpue$common))), 
                                    selected = "Pacific halibut"),
                        selectInput("survey", "Survey", 
                                    choices = c(sort(c("BS", unique(dat_cpue$survey)))), 
                                    selected = c("EBS", "NBS"), 
                                    multiple = TRUE),
                        fluidRow(
                          column(6, selectInput("cpue_unit", 
                                                "CPUE Unit", 
                                                choices = c("kg/ha" = "wtcpue", 
                                                            "number/ha" = "numcpue", 
                                                            "None" = "none"), 
                                                selected = "wtcpue")),
                          column(6,radioButtons("cpue_display", 
                                                "Display", 
                                                choices = c("Sized points" = "pt", 
                                                            "IDW raster" = "idw"), 
                                                selected = "pt"))
                        ), 
                        selectInput("env_unit", "Environmental Unit (IDW)", 
                                    choices = c("Bottom Depth (m)" = "bot_depth", 
                                                "Bottom Temperature (°C)" = "bot_temp", 
                                                "Surface Temperature (°C)" = "surf_temp", 
                                                # 'expression(paste("Bottom Temperature (",degree,"C)"))' = "bot_temp",
                                                # 'paste("Surface Temperature (",degree,"C)")' = "surf_temp",
                                                "None" = "none"), 
                                    selected = "none"),
                        checkboxInput("stat_points", 
                                      "Station Points", 
                                      value = TRUE),
                        checkboxInput("stratum", 
                                      "Stratum", 
                                      value = TRUE), 

                        downloadButton(outputId = "dl_map", 
                                       label = "Download Map (PNG)"), 
                        
                        br(),
                        br(),
                        # verbatimTextOutput
                        textOutput("latlon")
                        
                        # leafletOutput("survey_leaflet")                                            conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                        # Only prompt for threshold when coloring or sizing by superzip
                        #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                        # ),
                        
                        # plotOutput("histCentile", height = 200),
                        # plotOutput("scatterCollegeIncome", height = 250)
          # ),
          # 
          # tags$div(id="cite", 
          #          tags$em(paste0('Data last updated ', lastdl,
          #                 ' and this app was last updated ',
          #                 format(Sys.Date(), format='%B %d %Y'),'.')))
          )
      )
    )
    
    
  )
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

