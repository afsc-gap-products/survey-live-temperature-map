

# source("data_dl.R") # Download Data

# Support scripts --------------------------------------------------------------
source(here::here("files_support", "style.R"))
source(here::here("files_support", "functions.R"))
# source(here::here("files_support", "data_dl.R"))
source(here::here("files_support", "data.R"))

## ui code parsed by tabName ---------------------------------------------------
source(here::here("files_ui", "ui_surveymap.R"))
source(here::here("files_ui", "ui_metadata.R"))
source(here::here("files_ui", "ui_glossary.R"))
source(here::here("files_ui", "ui_data.R"))
# source(here::here("files_ui", "ui_plots.R"))
source(here::here("files_ui", "ui_licencing.R"))
source(here::here("files_ui", "ui_manual.R"))


# Define -----------------------------------------------------------------------
title0 <- " | Bottom Trawl Survey Temperature and Progress Maps "
subtitle0 <-  "NOAA Fisheries scientists share information on ocean temperatures recorded during the Aleutian Islands, Gulf of Alaska and Bering Sea Bottom Trawl Surveys"

# ui - User Interface ----------------------------------------------------------

# Define UI for application that draws a histogram
ui <- tagList(
  dashboardPage(skin = "black",

## Title -----------------------------------------------------------------------

  title = tags$head(tags$title(paste0("NOAA Fisheries ",title0," | NOAA Fisheries
", subtitle0)), 
                    tags$link(rel="shortcut icon", 
                              href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico", 
                              type="image/vnd.microsoft.icon")), 
  
## Header -----------------------------------------------------------------------
  
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
                           
                           tags$li(class = "dropdown", 
                                   tags$a(icon("github"), 
                                          href = "https://github.com/EmilyMarkowitz-NOAA/AFSCRACE_SurveyDataMapApp",
                                          title = "See the code on github", 
                                          style = "color: #1f93d0;"))
                           ), 
  
## Sidebar -----------------------------------------------------------------------
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
  
## Body -----------------------------------------------------------------------
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
      # ui.plots(),        # High Quality Maps
      ui.data(),        # High Quality Maps
      # ui.import(),       # Import Data
      # ui.calculator(),   # Evaluation Metrics
      ui.licencing(),       # Export Predictions
      ui.manual()        # Manual
    )
  )), 
  
## Footer -----------------------------------------------------------------------

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
  
  ## Images -----------------------------------------------------------------------
  
  output$ImageFull <- renderImage({
    filename <- normalizePath(here::here("./www/noaa_fisheries_small.png"))
    list(src = filename,
         width = session$clientData$output_ImageFull_width,
         height = session$clientData$output_ImageFull_height
    )
  }, deleteFile = FALSE)

  output$Image <- renderImage({
    filename <- normalizePath(here::here("./www/noaa_logo.gif"))
    list(src = filename,
         width = session$clientData$output_Image_width,
         height = session$clientData$output_Image_height
    )
  }, deleteFile = FALSE)
  
  ## Body -----------------------------------------------------------------------
  # readline(here::here("files_server", "s_surveymap.R"))
  # source(here::here("files_server", "s_surveymap.R"))
  # source(here::here("files_server", "s_glossary.R"))
  source(here::here("files_server", "s_data.R"))
  # source(file = here::here("files_server", "s_glossary.R"))
  
  ##  s_surveymap.R ------------------------------------------------------------
  
  
  find_coldpool <- function(input) {
    r <- NULL
    r0 <- ""
    temp <- dat %>% # is this data in the dataset?
      dplyr::filter(SRVY %in% input$survey &
                      year == input$year) %>%
      dplyr::select(SRVY) %>% 
      unique()
    
    if (input$plot_unit == "bt") {
      if ("EBS" %in% input$survey & "NBS" %in% input$survey & # is this data requested?
          "EBS" %in% temp$SRVY & "NBS" %in% temp$SRVY) { # is this data in the dataset?
        df2 <- coldpool::nbs_ebs_bottom_temperature; r0 <- "nbs_ebs_bottom_temperature"
      } else if ("EBS" %in% input$survey & # is this data requested?
                 "EBS" %in% temp$SRVY ) { # is this data in the dataset?
        df2 <- coldpool::ebs_bottom_temperature; r0 <- "ebs_bottom_temperature"
      } else if ("NBS" %in% input$survey & # is this data requested?
                 "NBS" %in% temp$SRVY ) { # is this data in the dataset?
        df2 <- coldpool::nbs_bottom_temperature; r0 <- "nbs_bottom_temperature"
      }
    }
    
    if (input$plot_unit == "st") {
      if ("EBS" %in% input$survey & "NBS" %in% input$survey & # is this data requested?
          "EBS" %in% temp$SRVY & "NBS" %in% temp$SRVY) { # is this data in the dataset?
        df2 <- coldpool::nbs_ebs_surface_temperature; r0 <- "nbs_ebs_surface_temperature"
      } else if ("EBS" %in% input$survey & # is this data requested?
                 "EBS" %in% temp$SRVY ) { # is this data in the dataset?
        df2 <- coldpool::ebs_surface_temperature; r0 <- "ebs_surface_temperature"
      } else if ("NBS" %in% input$survey & # is this data requested?
                 "NBS" %in% temp$SRVY ) { # is this data in the dataset?
        df2 <- coldpool::nbs_surface_temperature; r0 <- "nbs_surface_temperature"
      }
    }
    
    temp <- gsub(pattern = "_", replacement = "", x = names(df2))
    temp <- gsub(pattern = "[A-Za-z]+", replacement = "", x = temp)
    
    if (!(input$year %in% as.numeric(temp)) |
        is.null(df2)) {
      # if there is no data for this year, return an error message
      warning_str <- "The coldpool package has not created rasters for these temperature data yet. "
    } else {
      r <- df2[[which(input$year == as.numeric(temp))]]
    }
    
    return(list("r" = r, 
                "name" = r0))
  }
  
  # BASE MAP ----------------
  output$survey_leaflet <- renderLeaflet({
    
    df0 <- dat %>%
      dplyr::filter(year == input$year &
                      SRVY %in% input$survey)
    
    pal <- colorNumeric(viridis(option = "G", n = 2, begin = .2, end = .8), 
                        domain = shp_surv$survey_definition_id,
                        na.color = "transparent")
    
    a <- leaflet(
      options = leafletOptions(
        crs = leafletCRS(
          crsClass = "L.Proj.CRS",
          code = "EPSG:3338",
          proj4def = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
          resolutions = 2^(16:7))) ) %>%
      addPolygons(data = 
                    rnaturalearth::ne_countries(
                      scale = "medium", 
                      returnclass = "sf") %>% 
                    st_transform(crs = "+proj=longlat +datum=WGS84"),
                  weight = .5, 
                  color = "black", 
                  opacity = .5,
                  fillOpacity = 0.7,
                  smoothFactor = 0.5,
                  label = ~paste(name),
                  labelOptions = labelOptions(direction = "auto")) %>%
      addPolygons(data = shp_surv %>% 
                    dplyr::filter(SRVY %in% input$survey), 
                  weight = 1, 
                  color = "#444444", 
                  opacity = 1,
                  fillColor = ~pal(survey_definition_id), 
                  # fillOpacity = 0.2, 
                  # smoothFactor = 0.5,
                  label = ~paste(survey_long),
                  labelOptions = labelOptions(direction = "auto"))  %>%
      htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container { background: none !important; }" )) %>% # transparent
      # addProviderTiles(
      #   provider = providers$Stamen.TonerLite, 
      #   options = providerTileOptions(noWrap = F, minZoom = 2)
      # addTiles(
      # ) %>%
      addMouseCoordinates(epsg = "EPSG:3338") %>% 
      # addLayersControl() %>%
      addMeasure(
        primaryLengthUnit = "kilometers",
        secondaryAreaUnit = "miles") %>%
      setView(lat = 56.60,
              lng = -159.3,
              zoom = 4) %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()),
        polylineOptions = filterNULL(list(shapeOptions =
                                            drawShapeOptions(lineJoin = "round",
                                                             weight = 3))),
        circleOptions = filterNULL(list(shapeOptions =
                                          drawShapeOptions(),
                                        repeatMode = F,
                                        showRadius = T,
                                        metric = T,
                                        feet = F,
                                        nautic = F))) %>%
      addStyleEditor(position = "bottomleft",
                     openOnLeafletDraw = TRUE)
    
    ## ADD STRATUM POLYGON? -----------------
    if (input$stratum) {
      
      a <- a %>% 
        addPolygons(data = shp_strat %>% 
                      dplyr::filter(SRVY %in% input$survey), 
                    weight = 1,
                    # opacity = 0.5,
                    stroke = 1, 
                    color = "black", 
                    fill = "transparent",
                    fillColor = "transparent",
                    # fillOpacity = 0.01,
                    label = paste0("Stratum: ", shp_strat$stratum),
                    highlightOptions = 
                      highlightOptions(fillColor = 'grey50',
                                       # opacity = 0.5, 
                                       fill = 'grey50',
                                       bringToFront = TRUE))
    }
    
    # ADD STATION POINTS? ---------------------
    if (input$station) {
      
      a <- a %>% 
        addCircleMarkers(data = shp_stn %>% 
                           dplyr::filter(SRVY %in% input$survey), 
                         radius = .1, 
                         weight = .25,
                         opacity = 0.75,
                         stroke = 0.1,
                         color = nmfspalette::nmfs_palette(palette = "urchin")(1),
                         fillOpacity = 0.5, 
                         popup = paste(
                           "<strong>Survey:</strong> ", df1$region_long, "<br>",
                           "<strong>Data State:</strong> ", df1$data_type,  "<br>",
                           "<strong>Station:</strong> ", shp_stn$station, "<br>",
                           "<strong>Stratum:</strong> ", shp_stn$stratum,  "<br>",
                           "<strong>Latitude (&degN):</strong> ", round(shp_stn$lat, 2),  "<br>",
                           "<strong>Longitude (&degW):</strong> ", round(shp_stn$lon, 2),  "<br>"
                         ))
    }
    
    # ADD DATA -------------
    if (paste(input$plot_unit) != "none") {
      
      # pal_pal <- viridis_pal(begin = .2, end = .8, option = "B")
      pal <- leaflet::colorNumeric(palette = viridis_pal(begin = .2, end = .8, option = "B")(2), 
                                   domain = c(-2, 12), 
                                   na.color = viridis(n = 1, begin = .8, end = .8, option = "B"))
      
      if (paste0(input$plot_display == "pt")) {
        
        df0$val <- unlist(df0[,input$plot_unit])
        
        df1 <- df0 %>%
          dplyr::filter(!is.na(val) &
                          format(x = date, format = "%m %d") <= format(input$plot_dates, format = "%m %d")) %>% 
          dplyr::mutate(
            lon = longitude,
            lat = latitude) %>%
          sf::st_as_sf(., coords = c("lon","lat"))
        
        a <- a %>%
          addCircleMarkers(
            data = df1,
            radius = 5, 
            weight = 1,
            opacity = .75,
            fillOpacity = .75,
            stroke = 1,
            popup = paste0(
              "<strong>Survey:</strong> ", df1$region_long, "<br>",
              "<strong>Data State:</strong> ", df1$data_type,  "<br>",
              "<strong>Data Source:</strong> ", ifelse(df1$data_type == "raw", "unpublished", "Fisheries One Stop Shop Data Portal: https://www.fisheries.noaa.gov/foss"),"<br>",
              "<strong>Station:</strong> ", df1$station, "<br>",
              "<strong>Stratum:</strong> ", df1$stratum,  "<br>",
              "<strong>Latitude (&degN):</strong> ", round(df1$latitude, 2),  "<br>",
              "<strong>Longitude (&degW):</strong> ", round(df1$longitude, 2),  "<br>",
              "<strong>Date Surveyed:</strong> ", df1$date,  "<br>",
              "<strong>Bottom Temperature (&degC; ",ifelse(input$plot_unit == "bt", "shown", "not shown"),"):</strong> ", round(df1$bt, 2),  "<br>",
              "<strong>Surface Temperature (&degC; ",ifelse(input$plot_unit == "st", "shown", "not shown"),"):</strong> ", round(df1$st, 2),  "<br>"), 
            color = ~pal(df1$val)
          ) #%>%
        # addLegend(
        #   # values = ~(df1$val),
        #   col = ~pal(df1$val),
        #   # pal = pal, 
        #   zlim = c(-2, 12), 
        #   # position = "bottomleft", 
        #   legend.lab = paste0(ifelse(input$plot_unit == "bt", "Bottom", "Surface"), " Temperature (&degC)"))
        
        # }
      } else if (paste0(input$plot_display == "coldpool")) {
        
        r0 <- find_coldpool(input)
        r <- r0$r
        
        if (!is.null(r)) {
          xyz <- rasterToPoints(r) %>% 
            data.frame() %>% 
            sf::st_as_sf(., coords = c("x", "y"), crs = proj4string(r)) %>% 
            sf::st_transform(., crs = "+proj=longlat +datum=WGS84") 
          names(xyz)[1] <- "val"
          xyz0 <- xyz %>% 
            dplyr::mutate(lon = sf::st_coordinates(xyz)[1], 
                          lat = sf::st_coordinates(xyz)[2])
          
          a <- a %>% 
            leaflet::addCircleMarkers(
              data = xyz, 
              radius = .5, 
              weight = 1,
              opacity = .5,
              fillOpacity = .5,
              stroke = 1,
              popup = paste0(
                "<strong>Survey:</strong> ", paste(c("EBS", "NBS")[c("EBS", "NBS") %in% input$survey], collapse = " and "), "<br>",
                "<strong>Data Source:</strong> coldpool R package: `coldpool::",r0$name,"$",names(r),"`<br>",
                "<strong>Latitude (&degN):</strong> ", round(xyz0$lat, 2),  "<br>",
                "<strong>Longitude (&degW):</strong> ", round(xyz0$lon, 2),  "<br>",
                "<strong>", ifelse(input$plot_unit == "bt", "Bottom", "Surface"), 
                " Temperature (&degC):</strong> ", round(xyz0$val, 2),  "<br>"), 
              color = ~pal(xyz$val)
            ) 
        }
      }
    }
    
    
    # ADD VESSEL POINTS? ---------------------
    if (input$vessel) {
      
      df00 <- df0 %>%
        dplyr::filter(format(x = date, format = "%m %d") <= format(input$plot_dates, format = "%m %d")) %>% 
        dplyr::select(SRVY, year, date, station, stratum, region_long, 
                      vessel_id, vessel_name, vessel_color, 
                      latitude, longitude) %>%
        dplyr::mutate(
          lon = longitude,
          lat = latitude) %>%
        sf::st_as_sf(., coords = c("lon","lat")) %>%
        sf::st_set_crs("+proj=longlat +datum=WGS84")
      
      a <- a %>% 
        addCircleMarkers(data = df00, 
                         lng = df00$longitude, 
                         lat = df00$latitude, 
                         radius = 2, 
                         weight = 5,
                         opacity = 0.75,
                         stroke = 1, 
                         color = ~df00$vessel_color,
                         popup = paste0(
                           "<strong>Survey:</strong> ", df00$region_long, "<br>",
                           "<strong>Year:</strong> ", df00$year,  "<br>",
                           "<strong>Station:</strong> ", df00$station, "<br>",
                           ifelse(is.na(df00$stratum), "", paste0("<strong>Stratum:</strong> ", df00$stratum,  "<br>")),
                           "<strong>Vessel:</strong> ", df00$vessel_name,  "<br>",
                           "<strong>Latitude (&degN):</strong> ", round(df00$latitude, 2),  "<br>",
                           "<strong>Longitude (&degW):</strong> ", round(df00$longitude, 2),  "<br>")
        )
    }
    
    
    return(a)
  })
  
  # output$plot_display_warning <- renderUI({
  #   
  # 
  #       temp <- gsub(pattern = "_", replacement = "", x = names(df2))
  #       temp <- gsub(pattern = "[A-Za-z]+", replacement = "", x = temp)
  #       
  #       
  #       if (!(input$year %in% as.numeric(temp)) |
  #           is.null(df2)) {
  #         # if there is no data for this year, return an error message
  #         warning_str <- "The coldpool package has not created rasters for these temperature data yet. "
  #       }
  #     }
  #   }      
  #   warning_str <- ifelse(warning_str == "", "", 
  #                         HTML(paste("<i style='color: #87CEEB'>", warning_str, "</i>", sep = '<br/>')))
  #   
  #   if (warning_str == "" & 
  #       input$plot_unit != "none" & 
  #       input$plot_display == "coldpool"){
  #     return()
  #   } else {
  #     return(HTML(paste("<i style='color: #87CEEB'>", warning_str, "</i>", sep = '<br/>')))
  #   }
  #   
  # })
  
  output$plot_display <- renderUI({
    
    req(input$plot_unit != "none") # this code will only work when plot_unit is not "none"
    
    r <- find_coldpool(input) # Does this return a raster wrt inputs?
    r <- r$r
    
    temp <- list("Points" = "pt",
                 "Cold Pool R Package (EBS and NBS, only)" = "coldpool")
    if (is.null(r)) {
      temp <- temp[1]
    }
    
    radioButtons(
      inputId = "plot_display", 
      label = HTML(paste("Data Display: ", sep = '<br/>')), 
      # label = "Data Display: \n\n",
      choices = temp,
      selected = "pt")
  })
  
  output$plot_dates <- renderUI({
    
    req(input$plot_display == "pt") # this code will only work when plot_display is "pt"
    
    df00 <- dat %>%
      dplyr::filter(year == input$year & 
                      # !is.na(date) &
                      SRVY %in% input$survey) %>%
      dplyr::select(date) #%>% 
    # unique()
    
    # if (input$plot_display != "pt") {
    #   return()
    # } else {
    # temp <- 
    sliderInput(
      inputId = "plot_dates",
      label = "Dates: ",
      min = min(df00$date, na.rm = TRUE),
      max = max(df00$date, na.rm = TRUE),
      value = max(df00$date, na.rm = TRUE),
      timeFormat = "%B %d", 
      step = 1,
      animate = animationOptions(
        interval = 5000,
        playButton = icon('play', "fa-2x"),
        pauseButton = icon('pause', "fa-2x")))
    #   return(temp)
    # }
  })
  
  # output$vessel <- renderUI({
  #   req(input$plot_display == "pt") # this code will only work when plot_display is "pt"
  #   checkboxInput("vessels", 
  #                 "Vessels", 
  #                 value = FALSE)
  # })
  
  # Download map ---------------------------------------------------------------
  
  # ## observeEvent which makes a call to the Batch-file and saves the image as .png
  # observeEvent(input$download_pdf, {
  #   img = paste0("screen", runif(1,0,1000), ".png")
  #   str = paste('call screenCapture ', img)
  #   shell(str)
  # })
  
  # output$dl_map <- downloadHandler(
  #   filename = "survey_map.png",
  #   
  #   content = function(file) {
  #     mapshot(survey_leaflet$dat, file = file)
  #   }
  # )
  
  output$dl_map <- downloadHandler(
    filename = "map.png",
    content = function(file) {
      saveWidget(reactive_map(), "temp.html", 
                 selfcontained = FALSE)
      webshot::webshot("temp.html", 
                       file = file, 
                       cliprect = "viewport")
    })
  
  
  ## CSV Download --------------------------------------------------------------
  output$downloadData <- downloadHandler(
    # filename <- paste0("NOAAAcousticThresholds_", Sys.Date(), ".csv"),
    filename = #function() {
      "downloadData.csv",
    # },
    contentType = "text/csv",
    content = function(file) {

      filename0<-file#"downloadData.csv"#here::here(getwd(), "downloadData.csv")

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
                                        "Bottom Temperature (°C)" = "bt", 
                                        "Surface Temperature (°C)" = "st", 
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
  
  
  ## R Markdown Report  --------------------------------------------------------
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    contentType = "text/html",

    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- here::here(getwd(), "report4.Rmd")
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
