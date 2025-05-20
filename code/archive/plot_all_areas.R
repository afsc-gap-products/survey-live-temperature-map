# Load shape files -------------------------------------------------------------

## EBS + NBS  ------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto")
survey_area$survey.grid <- survey_area$survey.grid %>% 
  sf::st_transform(x = ., survey_area$crs$input) %>%
  dplyr::rename(station = STATIONID) %>%
  dplyr::left_join(x = ., 
                   y = haul %>%
                     # dplyr::rename(station = stationid) %>% 
                     dplyr::select(station, stratum) %>% 
                     dplyr::distinct(), 
                   by = "station") %>% 
  dplyr::mutate(region = "Bering Sea")
survey_area$place.labels$y[survey_area$place.labels$lab == "200 m"] <- -60032.7
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(srvy = ifelse(SURVEY == "EBS_SHELF", "EBS", "NBS"))
shp_bs <- survey_area

# ## EBS  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
survey_area$survey.area <- shp_bs$survey.area %>%
  dplyr::filter(srvy == "EBS")
shp_ebs <- survey_area

## NBS  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto")
survey_area$survey.area <- shp_bs$survey.area %>%
  dplyr::filter(srvy == "NBS")
shp_nbs <- survey_area

## AI  ------------------------------------------------------------------------
# akgfmaps
survey_area <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
survey_area$survey.grid <-
  dplyr::left_join(
    x = survey_area$survey.grid %>%
      dplyr::rename(station = ID,
                    stratum = STRATUM),
    y = goa_goa_strata0 %>%
      dplyr::filter(survey == "AI") %>%
      dplyr::mutate(srvy = "AI",
                    region = stringr::str_to_title(inpfc_area),
                    region = dplyr::case_when(
                      region %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
                      TRUE ~ region)) %>%
      dplyr::select(srvy, stratum, region) %>%
      dplyr::distinct(),
    by = "stratum")  %>%
  dplyr::arrange(region) %>%
  dplyr::filter(!is.na(region))
survey_area$survey.area <- survey_area$survey.area %>%
  dplyr::mutate(SURVEY = "AI",
                srvy = "AI")
shp_ai <- survey_area

# J:\RACE_GF\GOA\GOA 2021\Files for boats\ArcGIS\GOAGRID_2021
# J:\RACE_GF\ALEUTIAN\AI 2022\ArcGIS\Chart Products\aigrid_trawlable_thru2018 - ERRORS,do not use!
# G:\RACE_GF\ALEUTIAN\AI 2022\ArcGIS\Chart Products\aigrid_trawable_thru2018_Emily.shp
# library(sp)
# survey_area <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto")
# survey_area$survey.grid <- rgdal::readOGR(dsn = paste0(dir_wd, '/data/shapefiles/'),# Prepare map objects
#                                           layer = "aigrid_trawable_thru2018_Emily",
#                                           verbose=F) %>%
#   sp::spTransform(x = ., CRS(survey_area$crs$input)) %>%
#   st_as_sf(x = .) %>%
#   dplyr::rename(station = ID,
#                 stratum = STRATUM) %>%
#   dplyr::filter(stratum %in% unique(goa_goa_strata0$stratum) &
#                   stratum != 0) %>% # land
#   sp::merge(
#     x = .,
#     y = goa_goa_strata0 %>%
#       dplyr::filter(survey == "AI") %>%
#       dplyr::mutate(srvy = "AI",
#                     region = stringr::str_to_title(inpfc_area),
#                     region = dplyr::case_when(
#                       region %in% c("Western Aleutians", "Chirikof") ~ "Western Aleutians",
#                       TRUE ~ region)) %>%
#       dplyr::select(srvy, stratum, region) %>%
#       dplyr::distinct(),
#     all.x = TRUE)  %>%
#   dplyr::arrange(region) %>%
#   dplyr::mutate(AIGRID_ID = as.double(AIGRID_ID))
# survey_area$survey.area <- survey_area$survey.area %>%
#   dplyr::mutate(SURVEY = "AI",
#                 srvy = "AI")
# shp_ai <- survey_area

# 
# 
# temp1 <- shp_ai$survey.grid %>%
#   dplyr::mutate(area = sf::st_area(shp_ai$survey.grid$geometry), 
#                 perimeter = sf::st_length(shp_ai$survey.grid$geometry)#, 
#                 # area_diff = (AREA - area), 
#                 # perimeter_diff = (PERIMETER - perimeter)
#                 ) %>%
#   data.frame() %>%
#   dplyr::select(region, AIGRID_ID, stratum, station, area, perimeter)
# names(temp1) <- paste0("r_", names(temp1))
# 
# temp2 <- data.frame(survey_area$survey.grid) %>%
#   dplyr::mutate(area = sf::st_area(survey_area$survey.grid$geometry), 
#                 perimeter = sf::st_length(survey_area$survey.grid$geometry)#, 
#                 # area_diff = AREA != area, 
#                 # perimeter_diff = PERIMETER != perimeter
#                 ) %>%
#   data.frame() %>%
#   dplyr::select(region, AIGRID_ID, stratum, station, area, perimeter)
# names(temp2) <- paste0("a_", names(temp2))
# 
# temp3 <- ai_aigrid_gis0[order(ai_aigrid_gis0$aigrid_id),] %>%
#   dplyr::select(AIGRID_ID = aigrid_id, stratum, station = stationid , AREA = area_km2, PERIMETER = perimeter_km2) %>% 
#   dplyr::mutate(region = NA)
# names(temp3) <- paste0("t_", names(temp3))
# 
# temp <- dplyr::full_join(
#   x = temp1,
#   y = temp2,
#   by = c("r_AIGRID_ID" = "a_AIGRID_ID")) %>%
#   dplyr::mutate(diff_region = r_region != a_region,
#                 diff_stratum = r_stratum != a_stratum,
#                 diff_station = r_station != a_station,
#                 diff_area = as.numeric(r_area) != as.numeric(a_area), 
#                 diff_perim = as.numeric(r_perimeter) != as.numeric(a_perimeter))
# 
# temp <- dplyr::full_join(
#   x = temp1,
#   y = temp3,
#   by = c("r_AIGRID_ID" = "t_AIGRID_ID")) %>%
#   dplyr::mutate(
#     #diff_region = r_region != t_region,
#                 diff_stratum = r_stratum != t_stratum,
#                 diff_station = r_station != t_station,
#                 diff_area = r_AREA != t_AREA, 
#                 diff_perim = r_PERIMETER != t_PERIMETER)

## GOA  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
survey_area$survey.grid <-  
  dplyr::left_join(
    x = survey_area$survey.grid %>%
      dplyr::rename(station = ID, 
                    stratum = STRATUM),
    y = goa_goa_strata0 %>%
      dplyr::filter(survey == "GOA") %>%
      dplyr::mutate(srvy = "GOA",
                    region = stringr::str_to_title(inpfc_area) ) %>%
      dplyr::select(srvy, stratum, region) %>%
      dplyr::distinct(),
    by = "stratum")  %>% 
  dplyr::arrange(region) %>% 
  dplyr::filter(!is.na(region))
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(SURVEY = "GOA", 
                srvy = "GOA")
shp_goa <- survey_area


## bsslope  ------------------------------------------------------------------------
survey_area <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "auto")
# survey_area$survey.grid <- survey_area$survey.grid %>% 
#   sf::st_transform(x = ., shp_bs$crs$input) %>%
#   dplyr::rename(station = STATIONID) %>%
#   dplyr::left_join(x = ., 
#                    y = haul %>%
#                      # dplyr::rename(station = stationid) %>% 
#                      dplyr::select(station, stratum) %>% 
#                      dplyr::distinct(), 
#                    by = "station") %>% 
#   dplyr::mutate(region = "Bering Sea")
survey_area$survey.area <- survey_area$survey.area %>% 
  dplyr::mutate(srvy = "BSS")
shp_bss <- survey_area







gg <- ggplot() +
  ggplot2::geom_sf(data = survey_area$akland, 
                   fill = "black", 
                   color = "transparent") + #ifelse(srvy %in% c("GOA", "AI"), "black", "white")) + 
  # ggplot2::geom_sf(data = survey_area$graticule, 
  #                  color = "grey90", 
  #                  alpha = 0.5) +
  ggplot2::scale_x_continuous(name = "Longitude",
                              breaks = c(-180, -170, -160, -150, -140)) + # survey_area$lon.breaks) +
  # ggplot2::scale_y_continuous(name = "Latitude", 
  #                             breaks = survey_area$lat.breaks) +
  # ggtitle(label = plot_title, 
  #         subtitle = plot_subtitle) +
  ggplot2::theme_classic() + 
  ggplot2::theme(
    # panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5), 
    # panel.grid.major = element_rect(fill = "transparent"), 
    # panel.grid.minor = element_rect(fill = "transparent"), 
    plot.margin=unit(c(0,0,0,0), "cm") , 
    panel.background = element_rect(fill = "transparent"), #grey95
    plot.title = element_text(size = 20, face = "bold"), 
    plot.subtitle = element_text(size=14), 
    legend.text=element_text(size=12), 
    legend.position="right",
    legend.direction="vertical",
    legend.justification="left",
    legend.background = element_blank(),
    legend.title=element_text(size=14),
    axis.text = element_text(size=14), 
    legend.box.background = element_blank(),
    legend.key = element_blank(), 
    legend.key.size=(unit(.3,"cm")), 
    axis.title=element_text(size=14) ) +
  ggplot2::geom_sf(data = shp_ai$survey.grid, 
                   show.legend = FALSE, 
                   color = "lightblue1", 
                   fill = "lightblue1") +
  ggplot2::geom_sf(data = shp_nbs$survey.grid, 
                   show.legend = FALSE, 
                   color = "skyblue1", 
                   fill = "skyblue1") +
  ggplot2::geom_sf(data = shp_ebs$survey.grid, 
                   show.legend = FALSE, 
                   color = "skyblue3", 
                   fill = "skyblue3") +
  ggplot2::geom_sf(data = shp_goa$survey.grid, 
                   show.legend = FALSE, 
                   color = "skyblue4", 
                   fill = "skyblue4") +
  ggplot2::geom_sf(data = shp_bss$survey.area, 
                   show.legend = FALSE, 
                   color = "lightblue4", 
                   fill = "lightblue4") +
  ggplot2::coord_sf(xlim = c(-1394658,  2566293),
                      # range(shp_ai$plot.boundary$x, 
                      #            shp_bs$plot.boundary$x, 
                      #            shp_goa$plot.boundary$x, 
                      #            shp_bss$plot.boundary$x),
                    ylim = c(-1028565.1,  1125549.7))
                      # range(shp_ai$plot.boundary$y, 
                      #            shp_bs$plot.boundary$y, 
                      #            shp_goa$plot.boundary$y, 
                      #            shp_bss$plot.boundary$y))
