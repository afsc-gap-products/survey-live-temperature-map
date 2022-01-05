#' ---------------------------
#' title: Survey Daily and Anomally Temperature Plot
#' OG author: Jason Conner
#' maintained: Emily Markowitz and Liz Dawson (May 2021)
#' purpose: download oracle data
#' ---------------------------

# Download data from local: ----------------------------------------------------

a<-list.files(path = here::here("data", "oracle"))
for (i in 1:length(a)){
  b <- read_csv(file = paste0(here::here("data", "oracle", a[i])))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
}


# NBS Data ---------------------------------------------------------------------
dat_nbs <- haul0 %>% 
  dplyr::filter(region == 'BS' &
                stratum == c(70,71,81) &
              cruise != 201802 &
              cruise >= 2016) %>% 
  dplyr::mutate(year = substr(x = cruise, start = 1, stop = 4),  
                REGION = "NBS", 
                BOTTOM_TYPE = NULL)

write_csv(x = dat_nbs, 
          file = here::here("data", paste0("dat_nbs_",min(dat_nbs$year),"-", max(dat_nbs$year), ".csv")))

# EBS Data ---------------------------------------------------------------------
dat_ebs <- haul0 %>% 
  dplyr::filter(region == 'BS' &
                  stratum == c(10,20,31,32,41,42,43,50,61,62,82,90) &
                  cruise >= 198701) %>% 
  dplyr::mutate(year = substr(x = cruise, start = 1, stop = 4),  
                REGION = "EBS", 
                BOTTOM_TYPE = NULL)

write_csv(x = dat_ebs, 
          file = here::here("data", paste0("dat_ebs_",min(dat_ebs$year),"-", max(dat_ebs$year), ".csv")))

# Vessel -----------------------------------------------------------------------
vessel_info <-  vessels0 %>% 
  dplyr::select("name", vessel_id) %>% 
  unique() %>% 
  dplyr::mutate(vessel_shape = substr(x = name, 1,1)) %>%
  dplyr::mutate(vessel_ital = paste0("F/V *", stringr::str_to_title(name), "*")) %>%
  dplyr::mutate(vessel_name = paste0("F/V ", stringr::str_to_title(name))) %>% 
  dplyr::left_join(x = ., 
                   y = v_cruises0 %>% 
                     dplyr::select(vessel_id, region, year, cruise), 
                   by = "vessel_id") %>% 
  dplyr::filter(!is.na(region))

# TOLEDO, for troubleshooting!
vessel_info <- rbind(vessel_info, 
                     vessel_info %>% 
                       dplyr::filter(year == 2021) %>% 
                       dplyr::mutate(year = 2022, 
                                     cruise = gsub(pattern = "2021", replacement = "2022", x = cruise)))

# vessel_info <- vessel_info %>% 
#   dplyr::mutate(cruise = paste0())

# *** Static Data --------------------------------------------------------------

# dat_vess <- data.frame(var = c("v", "V", "a", "A"), 
#                        vess = c("F/V Vesteraalen", "F/V Vesteraalen", 
#                                 "F/V Alaska Knight", "F/V Alaska Knight"))

dat_survreg <- data.frame(reg_shapefile = c("EBS_SHELF", "NBS_SHELF", "AI", "GOA"), 
                           region_long = c("Eastern Bering Sea", "Northern Bering Sea", 
                                           "Aleutian Islands", "Gulf of Alaska"), 
                           region = c("EBS", "NBS", "AI", "GOA"), 
                           reg_dates = c("\n(May 25-Aug 04)", # CHANGE
                                         "\n(Aug 02-Aug 28)", 
                                         "\n(May 25-Aug 04)", # CHANGE
                                         "\n(Aug 02-Aug 28)")) # CHANGE
