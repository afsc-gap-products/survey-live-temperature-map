#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------

# Download data from oracle saved locally: -------------------------------------

a<-list.files(path = "./data/oracle/")
for (i in 1:length(a)){
  b <- read_csv(file = paste0("./data/oracle/", a[i]))
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

# haul data --------------------------------------------------------------------
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
