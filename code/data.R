#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------

# Download data from oracle saved locally: -------------------------------------

a<-list.files(path = here::here("data", "oracle"))
for (i in 1:length(a)){
  b <- read_csv(file = paste0(here::here("data", "oracle", a[i])))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
}


# Load data from Google Sheet --------------------------------------------------

if (googledrive_dl) {
# https://docs.google.com/spreadsheets/d/16CJA6hKOcN1a3QNpSu3d2nTGmrmBeCdmmBCcQlLVqrE/edit?usp=sharing
googledrive::drive_download(file = googledrive::as_id(dir_googledrive_log),  #"gap_survey_progression.csv",
                            type = "xlsx", 
                            overwrite = TRUE, 
                            path = paste0("./data/gap_survey_progression.xlsx"))
}


# Mapping ref ------------------------------------------------------------------

# report_types <- list(
#   "BS" = list(
#     plot_subtitle = "NOAA Fisheries Bering Sea Bottom Trawl Survey", 
#     shapefile = "NEBSgrid",
#     # reg = "BS",
#     region_akgfmaps = "bs.all"), 
#   "EBS" = list(
#     plot_subtitle = "NOAA Fisheries Bering Sea Bottom Trawl Survey", 
#     shapefile = "NBSgrid",
#     # reg = "BS",
#     region_akgfmaps = "bs.south"), 
#   "NBS" = list(
#     plot_subtitle = "NOAA Fisheries Bering Sea Bottom Trawl Survey", 
#     shapefile = "EBSgrid",
#     # reg = "BS",
#     region_akgfmaps = "bs.north"), 
#   "GOA" = list(
#     plot_subtitle = "NOAA Fisheries Gulf of Alaska Bottom Trawl Survey", 
#     shapefile = "NEBSgrid",
#     # reg = "GOA",
#     region_akgfmaps = "goa"), 
#   "AI" = list(
#     plot_subtitle = "NOAA Fisheries Aluetian Islands Bottom Trawl Survey", 
#     shapefile = "aigrid_trawable_thru2018",
#     # reg = "AI",
#     region_akgfmaps = "ai")
# )

# a <- report_types[names(report_types) == SRVY][[1]]
# for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }

# report_types <- list(
#   "EBS" = list(
#     sectname = "EBS-BTS-Report", 
#     SURVEY = "eastern Bering Sea", 
#     map.area = "bs.south", 
#     SRVY1 = "EBS", 
#     SRVY0 = "BS", # in Oracle
#     SRVY00 = 98, # EBS
#     station_id = akgfmaps::get_survey_stations(
#       select.region = "bs.south"),
#     extrap.box = c(xmn = -180, xmx = -156, ymn = 54, ymx = 62), 
#     strat0 = c("10", "20", "30", "31", "32", "40", "41", "42", "43", "50", "60", "61", "62", "82", "90", 
#                "999"),
#     reg_dat = akgfmaps::get_base_layers(
#       select.region = "bs.south", 
#       set.crs = "auto", 
#       return.survey.grid = TRUE)#,
#     # report_species = report_species_NEBS
#   ), 
#   "NBS" = list(
#     sectname = "NBS-BTS-Report", 
#     SURVEY = "northern Bering Sea", 
#     map.area = "bs.north", 
#     SRVY1 = "NBS", 
#     SRVY0 = "BS", # in Oracle
#     SRVY00 = 143,
#     station_id = akgfmaps::get_survey_stations(
#       select.region = "bs.north"),
#     extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 68),
#     strat0 = c("70", "71", "81", 
#                "999"), 
#     reg_dat = akgfmaps::get_base_layers(
#       select.region = "bs.north", 
#       set.crs = "auto", 
#       return.survey.grid = TRUE)#,
#     # report_species = report_species_NEBS
#   ), 
#   "NEBS" = list(
#     sectname = "NEBS-BTS-Report", 
#     SURVEY = "eastern and northern Bering Sea",
#     map.area = "bs.all", 
#     SRVY1 = c("EBS", "NBS"), 
#     SRVY0 = "BS", # in Oracle
#     SRVY00 = c(98, #NBS
#                143), # EBS
#     station_id = akgfmaps::get_survey_stations(
#       select.region = "bs.all"),
#     extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 68),
#     strat0 = c("10", "20", "30", "31", "32", "40", "41", "42", "43", "50", "60", "61", "62", "82", "90",
#                "70", "71", "81", 
#                "999"), 
#     reg_dat = akgfmaps::get_base_layers(
#       select.region = "bs.all", 
#       set.crs = "auto", 
#       return.survey.grid = TRUE)#,
#     # report_species = report_species_NEBS
#   )
# )

# haul data --------------------------------------------------------------------

# # The surveys we will consider covering in this data are: 
# surveys <- 
#   data.frame(survey_definition_id = c(143, 98, 47, 52, 78), 
#              SRVY = c("NBS", "EBS", "GOA", "AI", "BSS"), 
#              SRVY_long = c("northern Bering Sea", 
#                            "eastern Bering Sea", 
#                            "Gulf of Alaska", 
#                            "Aleutian Islands", 
#                            "Bering Sea Slope") )

haul <- dplyr::left_join(
  x = haul0, 
  y = v_cruises0 %>% 
    dplyr::select(cruise_id,  year, survey_name, vessel_id, survey_definition_id, 
                  vessel_name, start_date, end_date, cruisejoin), 
  by = "cruisejoin") %>% 
  dplyr::mutate(#year = substr(x = cruise, start = 1, stop = 4),  
                bottom_type = NULL, 
                SRVY = dplyr::case_when(
                  survey_definition_id == 143 ~"NBS", 
                    survey_definition_id == 98 ~ "EBS", 
                    survey_definition_id == 47 ~ "GOA", 
                    survey_definition_id == 52 ~ "AI", 
                    survey_definition_id == 78 ~"BSS")  ) %>%
  dplyr::filter(year <= maxyr &
                  year != 2020 & # no surveys happened this year because of COVID
                  (year >= 1982 & SRVY %in% "EBS") | # 1982 BS inclusive - much more standardized after this year
                     (year >= 2010 & SRVY %in% "NBS") | # 1982 BS inclusive - much more standardized after this year
                     SRVY %in% "BSS" | # keep all years of the BSS
                     (year >= 1991 & SRVY %in% c("AI", "GOA")) & # 1991 AI and GOA (1993) inclusive - much more standardized after this year
                  !is.na(SRVY) & 
                  abundance_haul == "Y" &
                  performance >= 0 &
                  !(is.null(stationid)))

# vessel -----------------------------------------------------------------------
vessel_info <-  vessels0 %>% 
  dplyr::select("name", vessel_id) %>% 
  dplyr::distinct() %>% 
  # dplyr::mutate(vessel_shape = substr(x = name, 1,1)) %>%
  dplyr::mutate(vessel_ital = paste0("F/V *", stringr::str_to_title(name), "*")) %>%
  dplyr::mutate(vessel_name = paste0("F/V ", stringr::str_to_title(name)))
  # dplyr::left_join(x = ., 
  #                  y = v_cruises0 %>% 
  #                    dplyr::select(vessel_id, region, year, cruise), 
  #                  by = "vessel_id") %>% 
  # dplyr::filter(!is.na(region))

# TOLEDO, for troubleshooting!
# vessel_info <- rbind(vessel_info, 
#                      vessel_info %>% 
#                        dplyr::filter(year == 2021) %>% 
#                        dplyr::mutate(year = 2022, 
#                                      cruise = gsub(pattern = "2021", replacement = "2022", x = cruise)))
