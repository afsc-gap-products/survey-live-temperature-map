

find_coldpool <- function(input) {
  r <- NULL
  r0 <- ""
  temp <- dat %>% # is this data in the dataset?
    dplyr::filter(SRVY %in% input$survey &
                    year == input$year) %>%
    dplyr::select(SRVY) %>% 
    unique()
  
  if (input$plot_unit == "bottom_temperature_c") {
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
  
  if (input$plot_unit == "surface_temperature_c") {
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
                         "<strong>Survey:</strong> ", df1$survey, "<br>",
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
            "<strong>Survey:</strong> ", df1$survey, "<br>",
            "<strong>Data State:</strong> ", df1$data_type,  "<br>",
            "<strong>Data Source:</strong> ", ifelse(df1$data_type == "raw", "unpublished", "Fisheries One Stop Shop Data Portal: https://www.fisheries.noaa.gov/foss"),"<br>",
            "<strong>Station:</strong> ", df1$station, "<br>",
            "<strong>Stratum:</strong> ", df1$stratum,  "<br>",
            "<strong>Latitude (&degN):</strong> ", round(df1$latitude, 2),  "<br>",
            "<strong>Longitude (&degW):</strong> ", round(df1$longitude, 2),  "<br>",
            "<strong>Date Surveyed:</strong> ", df1$date,  "<br>",
            "<strong>Bottom Temperature (&degC; ",ifelse(input$plot_unit == "bottom_temperature_c", "shown", "not shown"),"):</strong> ", round(df1$bt, 2),  "<br>",
            "<strong>Surface Temperature (&degC; ",ifelse(input$plot_unit == "surface_temperature_c", "shown", "not shown"),"):</strong> ", round(df1$st, 2),  "<br>"), 
          color = ~pal(df1$val)
        ) #%>%
      # addLegend(
      #   # values = ~(df1$val),
      #   col = ~pal(df1$val),
      #   # pal = pal, 
      #   zlim = c(-2, 12), 
      #   # position = "bottomleft", 
      #   legend.lab = paste0(ifelse(input$plot_unit == "bottom_temperature_c", "Bottom", "Surface"), " Temperature (&degC)"))
      
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
              "<strong>", ifelse(input$plot_unit == "bottom_temperature_c", "Bottom", "Surface"), 
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
      dplyr::select(SRVY, year, date, station, stratum, survey, 
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
                         "<strong>Survey:</strong> ", df00$survey, "<br>",
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
