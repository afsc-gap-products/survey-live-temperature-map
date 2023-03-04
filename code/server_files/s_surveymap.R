#######* Survey Map############
## create static element
output$survey_leaflet <- renderLeaflet({
  
  df0 <- dat_cpue %>%
    dplyr::filter(year == input$year &
                    common == input$common &
                    survey %in% input$survey)
  
  a <- leaflet() %>%
    addTiles() %>%
    setView(lat = 56.60, 
            lng = -159.3, 
            zoom = 4.5)  %>%
    onRender(
      "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                }"
    )
  
  # ADD STRATUM POLYGON? -----------------
  if (input$stratum) {
    
    df <- df0 %>%
      dplyr::filter(survey %in% input$survey) %>% 
      dplyr::select(survey, survey_num, stratum_shp) %>% 
      unique()
    
    code_str <- glue::glue('a <- a %>%
        addPolygons(data = {df$stratum_shp}, 
                    weight = 1, 
                    opacity = 0.5, 
                    stroke = 1, 
                    color = nmfspalette::nmfs_palette(palette = 
                           "oceans")({max(dat_cpue$survey_num)+1})[{df$survey_num}])')
    eval(parse(text = code_str))
  }
  
  
  
  # ADD ENV IDW ------------------
  if (paste(input$env_unit) != "none") {
    
    leg_lab <- as.numeric(trimws(formatC(x = eval(parse(text = df[1, paste0(paste(input$env_unit), "_breaks")])), 
                                         digits = 3, 
                                         big.mark = ",")))
    leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
    
    if (sum(unique(df$survey) %in% c("NBS", "EBS")) == 2) {
      df$map_area[df$survey %in% c("NBS", "EBS")] <- "bs.all"
      df$survey[df$survey %in% c("NBS", "EBS")] <- "BS"
    }
    
    for (i in 1:length(unique(df$survey))) {
      df1 <- df %>% 
        dplyr::filter(survey == unique(df$survey)[i])
      
      pal <- viridis::viridis(length(leg_lab))
      
      idw0 <- akgfmaps::make_idw_map(COMMON_NAME = df1$common,
                                     LATITUDE = df1$latitude,
                                     LONGITUDE = df1$longitude,
                                     CPUE_KGHA = as.numeric(unlist(df1[,paste(input$env_unit)])),
                                     region = df1$map_area[1],
                                     set.breaks = breaks,
                                     out.crs = "+proj=longlat +datum=WGS84")
      
      idw1 <- idw0$extrapolation.grid
      
      a <- a %>%
        leafem::addStarsImage(x = idw1,
                              colors = pal,
                              opacity = 0.8) 
    }
    
    a <- a %>%
      addLegend(position = "bottomleft", 
                pal = pal, 
                labels = leg_lab, 
                values = as.numeric(breaks), #~df$wtcpue,
                title = paste0(names(input$env_unit)),
                opacity = 0.8)
  }
  
  
  
  # ADD CPUE -------------
  if (paste(input$cpue_unit) != "none") {
    
    df <- df0[df0[,paste(input$cpue_unit)] > 0,]   
    breaks <- eval(parse(text = df[1, paste0(paste(input$cpue_unit), "_breaks")]))
    
    # ***ADD CPUE IDW-------------
    if (paste(input$cpue_display) == "idw") {
      
      leg_lab <- as.numeric(trimws(formatC(x = breaks, 
                                           digits = 3, 
                                           big.mark = ",")))
      leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
      
        # surv <- unique(input$survey)[i]
        # if (sum(surv %in% c("NBS", "EBS"))>2) {
        #   surv <- c(surv,"BS")
        #   surv <- surv[!(surv %in% c("NBS", "EBS"))]
        # }      
        
      for (i in 1:length(unique(input$survey))) {
        
        idwidx1 <- grep(pattern = input$year, 
                        x = names(idw_list), 
                        ignore.case = TRUE)
        # idwidx2 <- unique(grep(paste(paste(input$survey),
        #                               collapse="|"), 
        #                         names(idw_list),
        #                         value=TRUE))
        idwidx2 <- grep(pattern = paste0("_", paste(input$survey)), 
                        x = names(idw_list), 
                        ignore.case = TRUE)
        common <- input$common
        common <- gsub(pattern = "\\(", 
                                replacement = "", 
                                x = common)
        common <- gsub(pattern = "\\)", 
                                replacement = "", 
                                x = common)
        idwidx3 <- grep(pattern = common, 
                        x = names(idw_list), 
                        ignore.case = TRUE)
        idwidx4 <- grep(pattern = paste(input$cpue_unit), 
                        x = names(idw_list), 
                        ignore.case = TRUE)
        
        idwidx <- data.frame(table(c(idwidx1, idwidx2, 
                                     idwidx3, idwidx4)))
        idwidx <- as.numeric(paste(idwidx$Var1[idwidx$Freq==4]))
        idw1 <- idw_list[idwidx][[1]]

        if (!(length(idw1) %in% c(0,1))) { # not NA or nonexistant
          # 
          #   idw1 <-  sf::st_transform(x = idw1,
          #                           crs = "+proj=longlat +datum=WGS84")
          #   
          #   leg_lab <- levels(x = idw1$var1.pred)
          #   
          # if (sum(unique(idw1$var1.pred) %in% "0 - 0")>0) {
          #   leg_lab[leg_lab == "0 - 0"] <- "0 - <0.001"
          # 
          #   leg_lab[grep(pattern = "0 - ", x = leg_lab)] <- gsub(pattern = "0 - ", 
          #        replacement = "<0.001 - ", 
          #        x = leg_lab[grep(pattern = "0 - ", x = leg_lab)])
          #   
          #   levels(x = idw1$var1.pred) <- leg_lab
          # }
          #   
          # idw <- data.frame(idw1)
          # idw$var1.pred <- paste(idw$var1.pred) # get rid of factor class
          # idw <- idw[idw$var1.pred != "NA",]
          # leg_lab <- unique(idw$var1.pred)
          # pal <- nmfspalette::nmfs_palette(palette = "seagrass", 
          #                                  reverse = TRUE)(length(leg_lab))
          # idw <- left_join(x = idw, 
          #                  y = data.frame(var1.pred = c(leg_lab), 
          #                                 color = pal))
          # sp::coordinates(idw) <- ~x+y
          # 
          # crs(idw) <- "+proj=longlat +datum=WGS84"
          # 
          # akland <- sf::st_read(system.file("data", "ak_russia.shp",
          #                                   package = "akgfmaps"), quiet = TRUE)
          # sps <- sf::st_transform(x = akland,
          #                         crs = crs(idw))
          # sps <- sf::as_Spatial(st_geometry(sps), 
          #                       IDs = as.character(1:nrow(sps)))
          # 
          # library(spatialEco)
          # idw <- spatialEco::erase.point(idw, sps)
          
          
          # idw$var1.pred <- droplevels(idw)$var1.pred
          # table(idw$var1.pred)
 
          
          a <- a %>%
            addCircleMarkers(
              data = idw,
              lng = idw$x,
              lat = idw$y,
              radius = ~ 1,
              color = idw$color,
              stroke = FALSE,
              fillOpacity = 0.5
            ) %>%
            addLegendCustom(
              title = paste0("CPUE (", names(input$cpue_unit), ")"), 
              position = "bottomleft",
              colors = unique(idw$color), 
              labels = unique(idw$var1.pred), 
              sizes = 10, 
              opacity = 0.5)
        }
        
      }
      
      a <- a %>%
        addLegend(position = "bottomleft", 
                  pal = pal, 
                  labels = leg_lab, 
                  values = as.numeric(breaks), 
                  title = paste0(names(input$cpue_unit)),
                  opacity = 0.8)
      
      # ***ADD CPUE PT-------------
    } else if (paste(input$cpue_display) == "pt") {
      
      df4 <- df %>% 
        dplyr::filter(year == input$year &
                        survey %in% input$survey & 
                        common == input$common) %>%
        dplyr::select("latitude", "longitude", 
                      "station", "survey", "stratum", "scientific", 
                      "wtcpue", "numcpue", "datetime", "common", 
                      "surf_temp", "bot_temp", "bot_depth")      
      
      circle_size_x <- 5
      x_scaled <- scale_values(as.numeric(unlist(df[,paste(input$cpue_unit)])))+1
      pt_col <- nmfspalette::nmfs_palette(palette = "crustacean")(1)
      leg_lab <- as.numeric(trimws(formatC(x = scale_values(breaks)+1, #as.numeric(quantile(x_scaled)),
                                           digits = 3, #drop0trailing = TRUE,
                                           big.mark = ",")))
      leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
      
      a <- a %>%
        addCircleMarkers(
          data = df4,
          lng = df4$longitude,
          lat = df4$latitude,
          radius = ~ x_scaled*circle_size_x,
          popup = paste("<strong>Species:</strong> ", paste0(df4$common, " (<em>", df4$scientific, "</em>)"), "<br>",
                        # "<strong><u>Survey Data</u></strong> ", "<br>",
                        "<strong>Station:</strong> ", df4$station, "<br>",
                        "<strong>Stratum:</strong> ", df4$stratum,  "<br>",
                        "<strong>Latitude (&degN):</strong> ", df4$latitude,  "<br>",
                        "<strong>Longitude (&degW):</strong> ", df4$longitude,  "<br>",
                        "<strong>Date Surveyed:</strong> ", df4$datetime,  "<br>",
                        # "<strong><u>Environmental Data</u></strong> ", "<br>",
                        "<strong>Bottom Temperature (&degC):</strong> ", df4$bot_temp,  "<br>",
                        "<strong>Surface Temperature (&degC):</strong> ", df4$surf_temp,  "<br>",
                        "<strong>Average Depth (m):</strong> ", df4$bot_depth,  "<br>",
                        # "<strong><u>Species Data</u></strong> ", "<br>",
                        "<strong>Number CPUE (kg of fish/ha):</strong> ", df4$numcpue,  "<br>",
                        "<strong>Weight CPUE (Number of fish/ha):</strong> ", df4$wtcpue, "<br>"), 
          color = pt_col,
          stroke = FALSE,
          fillOpacity = 0.5
        ) %>%
        addLegendCustom(
          title = paste0("CPUE (", names(input$cpue_unit), ")"), 
          position = "bottomleft",
          colors = pt_col, 
          labels = leg_lab, 
          sizes = quantile(x_scaled*circle_size_x), 
          opacity = 0.5)
      
    }
    
  }
  
  
  # ADD STATION POINTS? ---------------------
  if (input$stat_points) {
    
    df4 <- dat_cpue %>% 
      dplyr::filter(year == input$year &
                      survey %in% input$survey) %>%
      dplyr::select("latitude", "longitude", 
                    "station", "survey", "stratum", "scientific", 
                    "wtcpue", "numcpue", "datetime", "common", 
                    "surf_temp", "bot_temp", "bot_depth")      
    
    
    # df2 <- df4 %>%
    #   dplyr::select("latitude", "longitude", "station") %>%
    #   unique()
    
    a <- a %>%
      addCircleMarkers(
        data = df4,
        lng = df4$longitude,
        lat = df4$latitude, 
        popup = paste("<strong>Station:</strong> ", df4$station, "<br>",
                      "<strong>Stratum:</strong> ", df4$stratum,  "<br>",
                      "<strong>Latitude (&degN):</strong> ", df4$latitude,  "<br>",
                      "<strong>Longitude (&degW):</strong> ", df4$longitude,  "<br>",
                      "<strong>Date Surveyed:</strong> ", df4$datetime,  "<br>",
                      "<strong>Bottom Temperature (&degC):</strong> ", df4$bot_temp,  "<br>",
                      "<strong>Surface Temperature (&degC):</strong> ", df4$surf_temp,  "<br>",
                      "<strong>Average Depth (m):</strong> ", df4$bot_depth,  "<br>"), 
        radius = 2.5, 
        color = nmfspalette::nmfs_palette(palette = "urchin")(1),
        stroke = FALSE, 
        fillOpacity = 0.5) %>%
      addLegend(position = "bottomleft",
                colors = nmfspalette::nmfs_palette(palette = "urchin")(1),
                labels = "Stations",
                # className = "circle",
                opacity = 1)
  }
  
  return(a)
})


output$table <- renderDataTable(input$datasetInput)

# output$table.login <- renderDataTable(DT::renderDT(user_data(), options = list(scrollX = TRUE)))


output$distPlot <- renderPlot({
  
  # generate bins based on input$bins from ui.R
  x    <- as.numeric(data.frame(datasetInput())[, 2]) 
  x <- x[!(is.na(x))]
  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = input$color, border = 'white')
  
})


output$dl_map <- downloadHandler(
  filename = "survey_map.png",
  
  content = function(file) {
    mapshot(survey_leaflet$dat, file = file)
  }
)


output$latlon <- renderText({
  if(is.null(input$hover_coordinates)) {
    "The mouse is outside of map."
  } else {
    paste("Latitude: ", round(input$hover_coordinates[1], digits = 3), 
          "
          Longitude: ", round(input$hover_coordinates[2], digits = 3))
  }
})

