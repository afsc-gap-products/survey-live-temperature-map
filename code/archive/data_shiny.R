
# Load shape files -------------------------------------------------------------

# Bering Sea
shp_bs <- get_base_layers(select.region = "bs.all", 
                           set.crs = "+proj=longlat +datum=WGS84")$survey.strata

# Eastern Bering Sea
shp_ebs <- get_base_layers(select.region = "bs.south", 
                           set.crs = "+proj=longlat +datum=WGS84")$survey.strata

# Northern Bering Sea
shp_nbs <- get_base_layers(select.region = "bs.north", 
                           set.crs = "+proj=longlat +datum=WGS84")$survey.strata

# Aleutian Islands
shp_ai <- get_base_layers(select.region = "ai", 
                           set.crs = "+proj=longlat +datum=WGS84")$survey.strata

# Gulf of Alaska  
shp_goa <- get_base_layers(select.region = "goa", 
                           set.crs = "+proj=longlat +datum=WGS84")$survey.strata

# Bering Sea Slope 
# shp_bsslope <- shp_nbs


# Load Design Based Estimates --------------------------------------------------

load(here::here("data", "publicdata", "all_data.Rdata"))
lastdl <- ageoffile(here::here("data", "publicdata", "all_data.Rdata"))

# load(file = paste0("./data/idw_list.Rdata"))
# 
# names(idw_list) <- gsub(pattern = "\\(",
#                         replacement = "",
#                         x = names(idw_list))
# names(idw_list) <- gsub(pattern = "\\)",
#                         replacement = "",
#                         x = names(idw_list))


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