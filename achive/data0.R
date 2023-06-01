
# Bering Sea
reg_dat <- get_base_layers(select.region = "bs.all", 
                           set.crs = "+proj=longlat +datum=WGS84")
shp_bs <- reg_dat$survey.strata

# Eastern Bering Sea
reg_dat <- get_base_layers(select.region = "bs.south", 
                           set.crs = "+proj=longlat +datum=WGS84")
shp_ebs <- reg_dat$survey.strata

# Northern Bering Sea
shp_nbs <- sf::st_read(system.file("data", 
                                   "ebs_strata.shp", 
                                   package = "akgfmaps"), 
                       quiet = TRUE) %>% 
  dplyr::filter(STRATUM %in% c(70, 71, 81)) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")




# Aleutian Islands
shp_ai <- shp_nbs

# Bering Sea Slope 
shp_bsslope <- shp_nbs

# Gulf of Alaska  
shp_goa <- shp_nbs

################## Load Design Based Estimates #################

load(here::here("data", "publicdata", "all_data.Rdata"))
lastdl <- ageoffile(here::here("data", "publicdata", "all_data.Rdata"))     

load(file = paste0("./data/idw_list.Rdata"))

names(idw_list) <- gsub(pattern = "\\(", 
                        replacement = "", 
                        x = names(idw_list))
names(idw_list) <- gsub(pattern = "\\)", 
                        replacement = "", 
                        x = names(idw_list))








# label = ~ htmltools::htmlEscape("/U+00B0"), 
# label = eval(parse(text = expression(paste("Latitude (",degree,"N))")))),
#       label = as.character(paste0(
# "Station: ", dat_cpue$station,  "
# 
# Stratum: ", dat_cpue$stratum, "
# 
# Latitude (",degree,"N)): ", dat_cpue$latitude, "
# 
# Longitude (",degree,"W)): ", dat_cpue$latitude, "
# 
# Date Surveyed: ", dat_cpue$datetime, "
# 
# **Environmental Variables:**
# 
# Bottom Temperature (",degree,"C): ", dat_cpue$bot_temp, "
# 
# Surface Temperature (",degree,"C): ", dat_cpue$surf_temp, "
# 
# Average Depth (m): ",  dat_cpue$bot_depth, "
# 
# **CPUE of ", dat_cpue$common, "(",dat_cpue$scientific, ")**" ,":
# 
# Number CPUE (kg of fish/ha): ",  dat_cpue$wtcpue, "
# 
# Weight CPUE (Number of fish/ha): ",  dat_cpue$numcpue)),#)), 