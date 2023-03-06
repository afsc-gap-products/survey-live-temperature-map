# Q. 
# NBS and EBS are seperate surveys, so should we be combining data for them togeterh in on e idw?
## yes

# GOA vs AI vs NBS/EBS
# change stratum colors to help id the surveys
# can use same color scheme
# note that biomass estimates are different between surveys

# GOA/AI cpue data? Where do I find that?

# should these be idws or should they be density plots?

# where can I treat NAs and INF as 0s (cpue)?

# Contact_Electronic_Mail_Address: AFSC.metadata@noaa.gov
# Bottom temperature
## make on/off for temperature IDW

# rm(list = ls())

# input <- list(year = 2019,
#               # year = 2000,
#               survey = c("EBS"),
#               # survey = c("NBS", "EBS"),
#               # common = "Pacific halibut",
#               # common = "Nudibranch (unidentified)",
#               common = "Pacific ocean perch",
#               cpue_unit = list("kg of fish/ha" = "wtcpue"),
#               # cpue_unit = list("Number of fish/ha" = "numcpue"),
#               cpue_display = list("Sized points" = "pt"),
#               env_unit = list("None" = "none"),
#               stratum = TRUE,
#               stat_points = TRUE)


# source("datadl.R") # Download Data
# source("create_idws.R") # Download Data

## SOURCE DATA -----------------------------------------------------------------

locations <- c(
  "C:/Users/liz.dawson/Work/R/GAPSurveyTemperatureMap/",
  "C:/Users/christopher.anderson/Work/survey-live-temperature-map/",
  "Z:/Projects/survey-live-temperature-map/", 
  "C:/Users/emily.markowitz/Documents/Projects/survey-live-temperature-map/")

for (i in 1:length(locations)){
  if (file.exists(locations[i])) {
    dir_wd  <- locations[i]
  }
}

googledrive_dl <- FALSE
source("./code/style.R") # Universal Documents
source("./code/functions.R") # App-specific files
source("./code/data.R") # Universal Documents

### ui code parsed by tabName
files0 <- list.files(path = here::here("code", "ui_files"), full.names = TRUE)
for (i in 1:length(files0)) {
  source(files0[i], local = TRUE, echo = FALSE, chdir = TRUE)
}

##########DEFINE####################
title0<-" | Bottom Trawl Survey Progression Map App "
# require.login<-T 
# 
# user_base <- tibble(
#   user = c("user1", "user2"),
#   password = c("pass1", "pass2"), 
#   password_hash = sapply(c("pass1", "pass2"), sodium::password_store), 
#   permissions = c("admin", "standard"),
#   name = c("User One", "User Two")
# )

##########ui - USER INTERFACE###########
# Define UI for application that draws a histogram
ui <- tagList(
  dashboardPage(skin = "black",
                
                #####* Title######
                title = tags$head(tags$title(paste0("NOAA Fisheries ",title0," | NOAA Fisheries")), 
                                  tags$link(rel="shortcut icon", 
                                            href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico", 
                                            type="image/vnd.microsoft.icon")), 
                
                #####* Header######
                # toolong<-function(text, MinLineLength, break0){
                #   if (nchar(text)>MinLineLength) {
                #     text0<-c()
                #     text00<-strsplit(x = text, split = " ")[[1]]
                #     
                #     # for (i in 1:ceiling(x = nchar(text)/MinLineLength)) {
                #     #   text0<-c(text0, 
                #     #            [grep(pattern = " ",  
                #     #                 x = substr(x = text, start = i, stop = i+MinLineLength))])
                #     # } 
                #   } else {
                #     text0<-text
                #   }
                #   return(text)
                # }
                
                header = dashboardHeader(title = 
                                           tags$a(href = 'https://www.fisheries.noaa.gov/',
                                                  tags$img(src="FISHERIES-Logo WEB ONLY.png", width = '90'), 
                                                  HTML(title0), 
                                                  style = paste0("text-align: right; 
                                    color: #10497e; 
                                    font-weight: bold; 
                                    font-size: 20px;
                                    font-family:'Arial Narrow';")
                                           ), 
                                         titleWidth = nchar(title0)*17,
                                         
                                         
                                         
                                         
                                         #For login
                                         tags$li(class = "dropdown", 
                                                 style = paste0("padding: 8px;"),
                                                 shinyauthr::logoutUI("logout", class = "btn-primary", 
                                                                      style = "background-color: #1f93d0; border: none; color: #ffffff")),
                                         
                                         
                                         #Other Icons
                                         dropdownMenu(
                                           tags$li(tags$style(HTML('color: #10497e;}'))),                          
                                           type = "notifications",
                                           icon = icon("question-circle"),
                                           # icon = tags$a(icon("question-circle")#,
                                           #               # href = "https://github.com/emilyhmarkowitz/ShinyTemplateNMFS",
                                           #               # title = "Also see:",
                                           #               # style = "color: #1f93d0;"
                                           #               ),
                                           badgeStatus = NULL,
                                           headerText = "See also:",
                                           # style = "color: #1f93d0;")
                                           notificationItem("NOAA Fisheries", icon = icon("fish"), status = "info", # TOLEDO
                                                            href = "https://www.fisheries.noaa.gov/"),
                                           notificationItem("AFSC RACE Division", icon = icon("ship"), status = "info",
                                                            href = "https://www.fisheries.noaa.gov/about/resource-assessment-and-conservation-engineering-division")
                                         ),
                                         # tags$li(class = "dropdown",
                                         #         tags$a(icon("question-circle"),
                                         #                href = "https://rstudio.github.io/shinydashboard/",
                                         #                title = "Also see:",
                                         #                style = "color: #1f93d0;")),
                                         
                                         
                                         
                                         tags$li(class = "dropdown", 
                                                 tags$a(icon("github"), 
                                                        href = "https://github.com/EmilyMarkowitz-NOAA/AFSCRACE_SurveyDataMapApp",
                                                        title = "See the code on github", 
                                                        style = "color: #1f93d0;"))
                ), 
                
                #####* Sidebar######
                sidebar = dashboardSidebar(
                  collapsed = TRUE, 
                  width = nchar(title0)*17, 
                  
                  #Login
                  div(textOutput("welcome"), 
                      style = 
                        "padding-top: 40px; 
         padding-bottom: 20px; 
          text-align:center; 
        color: #10497e; 
        font-weight: bold; 
        font-size: 20px;
        font-family:'Arial Narrow';"), # for Login
                  
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Survey Map", 
                             tabName = "surveymap", 
                             icon = icon("file-image")), # icon = icon("file-image-o")),
                    # menuItem(HTML(paste0("Welcome")),
                    #          tabName = "welcome", icon = icon("address-card")), #icon("sitemap")
                    menuItem(HTML(paste0("Metadata")),
                             tabName = "metadata", icon = icon("cogs")), #icon("sitemap")
                    menuItem(HTML(paste0("Glossary and Literature Cited")),
                             tabName = "glossary", icon = icon("road")), #icon("sitemap")
                    menuItem(HTML(paste0("Download Data")),
                             tabName = "data", icon = icon("road")), #icon("sitemap")
                    # menuItem("Import Data", 
                    #          tabName = "import", icon = icon("cloud-upload")),
                    # menuItem("Calculator", 
                    #          tabName = "calculator", icon = icon("cogs")),
                    menuItem("Licencing", 
                             tabName = "licencing", icon = icon("list-alt")),
                    menuItem("Manual", 
                             tabName = "manual", icon = icon("book"),
                             menuSubItem("Sub Menu Item 1", tabName = "sub_1"), 
                             menuSubItem("Sub Menu Item 2", tabName = "sub_2")
                    )
                  )
                ),
                
                #####* Body######
                body = dashboardBody(
                  
                  shinyjs::useShinyjs(),
                  tags$head(tags$style(HTML('
      .main-header .sidebar-toggle:before {
                              color: #10497e;}'))),
                  tags$head(tags$style(".table{margin: 0 auto;}"),
                            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                        type="text/javascript"),
                            includeScript("www/returnClick.js"), 
                            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                            tags$script(src = "./www/custom.js"),
                            # ),
                            
                            
                            # tags$head(
                            tags$style(HTML('
    /* logo */
      .skin-black .main-header .logo {
        background-color: #ffffff;
        height: 65px;
      }

      /* logo when hovered */
      .skin-black .main-header .logo:hover {
        background-color: #ffffff;
          color: #000000;
      }

      /* navbar (rest of the header) */
      .skin-black .main-header .navbar {
      background-image: linear-gradient(to right, #ffffff , #d9effa);
          color: #000000;
      }

      /* main sidebar */
      .skin-black .main-sidebar {
        background-color: #d9effa;
      }

      /* active selected tab in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #1f93d0;
          color: #ffffff ;
      }

      /* other links in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu a{
        background-color: #d9effa;
          color: #10497e;
      }

      /* other links in the sidebarmenu when hovered */
      .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #1f93d0;
          color: #ffffff;
      }

      /* toggle button when hovered  */
      .skin-black .main-header .navbar .sidebar-toggle:hover{
        background-color: #1f93d0;
          color: #10497e;
      }

      /* body */
      .content-wrapper, .right-side {
        background-color: #ffffff;
          color: #000000;

      }

      .content-wrapper,
      .right-side {
      background-color: #ffffff;
          color: #000000;
      padding: 30px;
      }

     .content-wrapper {
     background-color: #ffffff !important;
          color: #000000;
          
          
                               .leaflet-top .leaflet-control {
                                   margin: 0px;
                                }    

                                .leaflet-right {
                                     margin-right: 40px;
                                  }    
                                .full{
                                background-color: blue;
                                border-radius: 50%;
                                width: 20px;
                                height: 20px;
                                float: left;

                                }
                                .circle {
                                background-color: #FFF;
                                border: 3px solid blue;
                                border-radius: 50%;
                                height: 20px;
                                width: 20px;

                                }

                                .leaflet-control i{
                                  margin-right: 25px;
     }



                                    '))),
                  # div {
                  #   padding-left: 5px;
                  #   padding-top: 10px;
                  # }
                  
                  
                  #Login
                  # shinyauthr::loginUI("login"),
                  # uiOutput("user_table"),
                  # HTML('<div data-iframe-height></div>'),
                  
                  tabItems(
                    #   tabItem(
                    #     tabName = "welcome"#, 
                    #     uiOutput("ui.welcome")
                    #     ),
                    ui.surveymap(),     # Welcome
                    # ui.welcome(),     # Welcome
                    ui.metadata(),      # Roadmap
                    ui.glossary(),      # Roadmap
                    ui.plots(),        # High Quality Maps
                    ui.data(),        # High Quality Maps
                    # ui.import(),       # Import Data
                    # ui.calculator(),   # Evaluation Metrics
                    ui.licencing(),       # Export Predictions
                    ui.manual()        # Manual
                  )
                )), 
  
  #####* Footer######
  
  # tags$footer("U.S. Department of Commerce | National Oceanic and Atmospheric Administration | National Marine Fisheries Service",
  #             align = "center",
  #             style = "
  #             position:absolute;
  #             bottom:0;
  #             width:100%;
  #             height:60px;   /* Height of the footer */
  #             color: #10497e;
  #             font-size: 15px;
  #             font-family:'Arial Narrow';
  #             padding: 10px;
  #             background-color: #ffffff;
  #             z-index: 1000;
  #             ")
  
  tags$footer("U.S. Department of Commerce | National Oceanic and Atmospheric Administration | National Marine Fisheries Service",
              align = "center",
              style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:25px;   /* Height of the footer */
              color: #10497e;
              font-size: 10px;
              font-family:'Arial Narrow';
              padding: 0px;
              background-color: #ffffff;
              z-index: 1000;
              ")
  
)

server <- function(input, output, session) {
  
  # source(file.path("ui_files", "server_reference.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  # source(file.path("ui_files", "server_login.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  # source(file.path("ui_files", "server_login.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  # source(file.path("ui_files", "server_login.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  
  #######* Login######## 
  
  # credentials <- callModule(shinyauthr::login, "login", 
  #                           data = user_base,
  #                           user_col = user,
  #                           pwd_col = password_hash,
  #                           sodium_hashed = TRUE,
  #                           log_out = reactive(logout_init()))
  # 
  # logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  # 
  # observe({
  #   if(credentials()$user_auth) {
  #     shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  #   } else {
  #     shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  #   }
  # })
  # 
  # output$user_table <- renderUI({
  #   # only show pre-login
  #   if(credentials()$user_auth) return(NULL)
  #   
  #   tagList(
  #     tags$p("test the different outputs from the sample logins below 
  #            as well as an invalid login attempt.", class = "text-center"),
  #     
  #     renderTable({user_base[, -3]})
  #     )
  # })
  # 
  # user_info <- reactive({credentials()$info})
  # 
  # user_data <- reactive({
  #   req(credentials()$user_auth)
  #   
  #   if (user_info()$permissions == "admin") {
  #     dplyr::starwars[,1:10]
  #   } else if (user_info()$permissions == "standard") {
  #     dplyr::storms[,1:11]
  #   }
  #   
  # })
  # 
  # output$welcome <- renderText({
  #   req(credentials()$user_auth)
  #   
  #   glue("Welcome {user_info()$name}")
  # })
  
  #######* User Specific Welcome##########
  
  #######* Images##########
  output$ImageFull <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_fisheries_small.png"))
    list(src = filename,
         width = session$clientData$output_ImageFull_width,
         height = session$clientData$output_ImageFull_height
    )
  }, deleteFile = FALSE)
  
  output$Image <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_logo.gif"))
    list(src = filename,
         width = session$clientData$output_Image_width,
         height = session$clientData$output_Image_height
    )
  }, deleteFile = FALSE)
  
## Body ------------------------------------------------------------------------
  source(file.path("code", "server_files", "s_surveymap.R"),
         local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("code", "server_files", "s_glossary.R"),
         local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("code", "server_files", "s_data.R"),
         local = TRUE, echo = FALSE, chdir = TRUE)
  # source(file = here::here("server_files", "s_glossary.R"))
  
  ### * CSV Download####
  output$downloadData <- downloadHandler(
    # filename <- paste0("NOAAAcousticThresholds_", Sys.Date(), ".csv"),
    filename = #function() {
      "downloadData.csv",
    # },
    contentType = "text/csv",
    content = function(file) {
      
      filename0<-file#"downloadData.csv"#file.path(getwd(), "downloadData.csv")
      
      # Threshold Isopleths Results WARNINGS
      
      write.table(input$dataset,
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table("Data",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(input$datasetInput,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # DISCLAIMER
      write.table("LICENCE",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(licence0,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      
    }
  )
  
  ########* R Markdown Report #########
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    contentType = "text/html",
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(getwd(), "report4.Rmd")
      file.copy(from = "report4.Rmd", "report2.Rmd", overwrite = TRUE)
      file.copy("report2.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        ProjectName = input$ProjectName,
        distPlot = input$distPlot,
        table = input$table
      )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
    
  )
}

shinyApp(ui, server)
