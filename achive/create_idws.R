

source("./functions.R")
source("./data.R")

var1 <- c("wtcpue", "numcpue")
var2 <- c("bot_temp", "bot_depth", "surf_temp")
var_long <- c("CPUE (kg/ha)", "CPUE (number/ha)", 
               "Bottom Temperature (°C)", "Bottom Depth (m)", "Surface Temperature (°C)")

yr <- unique(dat_cpue$year)
survey <- c("NBS", "EBS", "BS")#, "All")
# common <- "Pacific halibut"
common <- unique(dat_cpue$common)
comb1 <- expand.grid(var1, yr, survey, common)
names(comb1) <- c("var", "yr", "survey", "common")

comb2 <- expand.grid(var2, yr, survey, NA)
names(comb2) <- c("var", "yr", "survey", "common")

comb <- rbind.data.frame(comb1, comb2)
  
comb <- comb %>% 
  dplyr::left_join(., data.frame(var = c(var1, var2), var_long)) %>%
  dplyr::mutate(var = as.character(var), 
                survey = as.character(survey), 
                common = as.character(common)) %>%
  tibble()


make_plots_idws <- function(i, comb, dat_cpue) {
  
  temp <- list(idw = NA, 
               plot = NA, 
               filename = NA, 
               yr = comb$yr[i], 
               survey = comb$survey[i], 
               var = comb$var[i],
               var_long = comb$var_long[i],
               common = comb$common[i])
  
  survey0 <- temp$survey
  if (survey0 == "BS") {
    survey0 <- c("NBS", "EBS")
  }
  
  df <- dat_cpue %>%
    dplyr::filter(year == temp$yr &
                    survey %in% survey0) %>%
    dplyr::select("year", "wtcpue", "survey", "surf_temp", "stratum", 
                  "station", "scientific", "numcpue", "longitude", "latitude", 
                  # "datetime", 
                  "common", "bot_temp", "bot_depth", "survey_long", "map_area", 
                  "wtcpue_breaks", "numcpue_breaks", "bot_temp_breaks", "bot_depth_breaks", "surf_temp_breaks")
  
  if (!is.na(temp$common)) {
    df <- df %>%
      dplyr::filter(common == temp$common)
  }
  df <- df[!(is.na(df[,temp$var])),]
  
  
  filename <- paste0(temp$yr, "_", 
                     temp$survey,  "_", 
                     temp$var)
  if (temp$var %in% c("wtcpue", "numcpue")) {
    filename <- paste0(temp$yr, "_", 
                       temp$survey,  "_", 
                       temp$var,  "_", 
                       temp$common)
  }
  temp$filename <- filename
  
  if (nrow(df) > 2 && # if there is no (or very little; 2 is arbirtary) data in this dataset to make the idw with
      ((temp$survey == "BS" & # and if eihter.... if BS and either NBS (common) or EBS are missing
        sum(unique(df$survey) %in% survey0) == 2) ||
       temp$survey == survey0) ) { # or if the survey needed is the one available in df
    
    if (temp$survey %in% "BS"){
      df$map_area <- "bs.all"
    }
    
    breaks <- unique(round(eval(parse(text = df[1, paste0(paste(temp$var), "_breaks")])), digits = 1))
    leg_lab <- as.numeric(trimws(formatC(x = breaks, #as.numeric(quantile(x_scaled)),
                                         digits = 3, #drop0trailing = TRUE,
                                         big.mark = ",")))
    leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
    # x = NA
    # extrap.box = NA
    # grid.cell = c(0.05, 0.05)
    # in.crs = "+proj=longlat"
    # key.title = "auto"
    # log.transform = FALSE
    # idw.nmax = 4
    # use.survey.bathymetry = TRUE
    # return.continuous.grid = TRUE
    # 
    # COMMON_NAME = df$common
    # LATITUDE = df$latitude
    # LONGITUDE = df$longitude
    # CPUE_KGHA = df[,temp$var]
    # region = df$map_area[1]
    # set.breaks = breaks
    # out.crs = "+proj=longlat +datum=WGS84"
    
    spp_idw0 <- make_idw_map0(COMMON_NAME = df$common,
                              LATITUDE = df$latitude, 
                              LONGITUDE = df$longitude, 
                              CPUE_KGHA = df[,temp$var], 
                              region = df$map_area[1], 
                              set.breaks = breaks) # , out.crs = "+proj=longlat +datum=WGS84"
    
    # scale colors
    if (temp$var %in% c("wtcpue", "numcpue")) {
      pal <- nmfspalette::nmfs_palette(palette = "seagrass",
                                       reverse = TRUE)(spp_idw0$n.breaks)
      pal <- c((pal[-1]), "white")
      # pal <- pal[-length(pal)]
      # pal <- c("white", RColorBrewer::brewer.pal(9, name = "Greens")[c(2, 4, 6, 8, 9)])
      # pal <- pal[-length(pal)]
      # pal <- c("white", pal)
      pal_lab <- c("No Catch", leg_lab)
    } else {
      pal <- c(viridis::viridis(spp_idw0$n.breaks+1))
      pal <- pal[-1]
      pal <- pal[-length(pal)]
      pal_lab <- leg_lab
    }
    
    spp_idw0 <- spp_idw0 %>%
      akgfmaps::add_map_labels(region = df$map_area[1]) %>%
      akgfmaps::change_fill_color(new.scheme = pal,
                                  show.plot = TRUE)
    
    spp_idw0$plot <- spp_idw0$plot + 
      scale_color_manual(
        name = paste0(ifelse(is.na(temp$common),
                             paste0(temp$yr, " Survey"),
                             paste0(temp$yr, " ", temp$common)),
                      "\n", temp$var_long),
        values  = pal, #levels(x = spp_idw0$extrapolation.grid$var1.pred),
        labels = pal_lab
      )
    
    # spp_idw0$plot <- spp_idw0$plot + 
    #   scale_color_discrete(
    #     name = paste0(ifelse(is.na(temp$common),
    #                          paste0(temp$yr, " Survey"),
    #                          paste0(temp$yr, " ", temp$common)),
    #                   "\n", temp$var_long),
    #     # values = (pal), 
    #     limits  = levels(x = spp_idw0$extrapolation.grid$var1.pred),
    #     # limits = paste0(c("", rep_len(x = ">", 
    #     #                               length.out = (length(pal_lab)-1))), 
    #     #                 gsub(pattern = " - ", 
    #     #                      replacement = "â€“", 
    #     #                      x = pal_lab)),
    #     labels = pal_lab)  
    
    levels(x = spp_idw0$extrapolation.grid$var1.pred) <- 
      c(ifelse(temp$var %in% c("wtcpue", "numcpue"), 
               "No Catch", ""), 
        leg_lab)
    
    
    # scale_fill_manual(
    #   name = paste0(ifelse(is.na(temp$common), 
    #                        paste0(temp$yr, " Survey"), 
    #                        paste0(temp$yr, " ", temp$common)), 
    #                 "\n", temp$var_long),
    #   values = (pal), 
    #   # breaks = gsub(pattern = " - ", replacement = "â€“", x = pal_lab), 
    #   labels = pal_lab)  
    
    
    temp$plot <- spp_idw0$plot
    temp$idw <- spp_idw0$extrapolation.grid
    # ggplot() + geom_stars(data = temp$idw)
    
    a <- c("pdf", "png")
    code_str <- glue::glue("ggsave(filename = paste0(filename, '.{a}'), 
           plot = temp$plot, 
           device = '{a}', 
           path = './maps/', 
           width = 12, 
           height = 9, 
           units = 'in')")
    eval(parse(text = code_str))
    
  }

  
  return(temp)
}



# Loop ----------------------
plot_list <- list() 

for (i in 2601:nrow(comb)){
  
  print(i)
  
  temp <- make_plots_idws(i, comb, dat_cpue)
  
  # Save yo' work
  plot_list <- c(plot_list, list(temp)) 
  names(plot_list)[i %% 100]<-temp$filename
  
  if ((i %% 100) == 0 ||
      i == nrow(comb)) {
    # diff <- ifelse((i %% 100) == 0, 
    #                100, 
    #                i %% 100)
    plot_list0 <- plot_list#[(i-(diff-1)):i]
    save(plot_list0, file = paste0("./maps/plot_list_",i,".Rdata"))
    
    plot_list <- list()
  }
}


# compile complete file
files<-list.files(path = "./maps/", 
                  pattern = "plot_list_", 
                  full.names = TRUE)
idw_list <- list()
# plot_list <- list()
for (i in 1:(length(files)-1)) {
  load(files[i])
  
  # names(plot_list0)<-names(plot_list0)
  # plot_list <- c(plot_list, plot_list0)   
  
  idw_list0 <- sapply(plot_list0, "[", c("idw") )
  # idw_list0 <- sapply(plot_list0, "[[", c("idw") )
  names(idw_list0)<-names(plot_list0)
  idw_list <- c(idw_list, idw_list0)
}
save(idw_list, file = paste0("./maps/idw_list0.Rdata"))


# idw_list[77]
# a<-idw_list[77][[1]]; ggplot() + geom_stars(data = st_transform(a, "+proj=longlat +datum=WGS84"))

# Fix IDWS --------------------------------------------------------------------

# load(file = paste0("./maps/idw_list0.Rdata"))

akland <- sf::st_read(system.file("data", "ak_russia.shp",
                                  package = "akgfmaps"), quiet = TRUE)
sps <- sf::st_transform(x = akland,
                        crs = "+proj=longlat +datum=WGS84")
sps <- sf::as_Spatial(st_geometry(sps), 
                      IDs = as.character(1:nrow(sps)))

for (i in 1:length(idw_list)){
    print(i)
  if (class(idw_list[i][[1]]) == "stars") {
    
    idw1 <- idw_list[i][[1]]
    idw1 <-  sf::st_transform(x = idw1,
                              crs = "+proj=longlat +datum=WGS84")
    
    leg_lab <- leg_lab0 <- levels(x = idw1$var1.pred)
    pal <- nmfspalette::nmfs_palette(palette = "seagrass", 
                                     reverse = TRUE)(length(leg_lab))
    
    if (length(grep(x = unique(idw1$var1.pred), pattern = "0 - ")) > 0) {
      
      leg_lab[leg_lab == "0 - 0"] <- "<0.001"
      
      leg_lab[grep(pattern = "0 - ", x = leg_lab)] <- gsub(pattern = "0 - ", 
                                                           replacement = "0.001 - ", 
                                                           x = leg_lab[grep(pattern = "0 - ", x = leg_lab)])
      
      levels(x = idw1$var1.pred) <- leg_lab
    }
    
    # idw <- data.frame(idw1)
    # idw$var1.pred <- paste(idw$var1.pred) # get rid of factor class
    # idw <- left_join(x = idw, 
    #                  y = data.frame(var1.pred = c(leg_lab), 
    #                                 color = pal))
    # idw <- idw[idw$var1.pred != "NA",]
    # leg_lab <- unique(idw$var1.pred)
    # 
    # library(concaveman)
    # idw <- idw %>%
    #   st_as_sf(coords = c("x", "y"), 
    #            crs = "+proj=longlat +datum=WGS84")
    # 
    # poly <- list()
    # for (ii in 1:length(leg_lab)){
    #   pnts <- idw %>%
    #     dplyr::filter(var1.pred == leg_lab[ii])
    #   
    #   poly0 <- concaveman(points = pnts, concavity = 1.1)
    # 
    #   # plot(poly, reset = FALSE)
    #   # plot(pnts, add = TRUE)
    #   non_overlaps <- st_intersection(s5) %>%
    #     filter(n.overlaps == 1)
    #   
    #   plot(non_overlaps["polygon"])
    #   
    #   
    #   poly <- c(poly,
    #             temp = list(poly0))
    #   names(poly)[ii]<-leg_lab[ii]
    # }
    # idwp <- SpatialPolygons(Srl = poly,
    #                        pO = 1:length(leg_lab),
    #                        proj4string = crs(idw))
    
    
    # idw <- st_contour(x = idw1, contour_lines = FALSE, na.rm = TRUE, breaks = leg_lab)
    # idw <-st_to_sf(x = idw1,                   
    #                   merge = TRUE, as_points = FALSE, 
    #                 connect8 = TRUE, na.rm = FALSE)
    # plot(idw, col = idw$color)
    # ggplot() %>%
    #   geom_polygon(data = idw, mapping = aes(color = color))
    
    
    idw <- data.frame(idw1)
    idw$var1.pred <- paste(idw$var1.pred) # get rid of factor class
    idw <- left_join(x = idw, 
                     y = data.frame(var1.pred = c(leg_lab), 
                                    color = pal))
    idw <- idw[idw$var1.pred != "NA",]
    leg_lab <- unique(idw$var1.pred)
    # pal <- nmfspalette::nmfs_palette(palette = "seagrass", 
    #                                  reverse = TRUE)(length(leg_lab))

    
    

    # # library(alphahull)
    # poly <- list()
    # for (ii in 1:length(leg_lab)){
    #   a <- idw %>% 
    #     dplyr::filter(var1.pred == leg_lab[ii])
    #   p <- alphahull::ahull(a$x, a$y, alpha = .05)
    #   # plot(p)
    #   poly <- c(poly, 
    #             temp = list(p))
    #   names(poly)[ii]<-leg_lab[ii]
    # }
    # idwp <- SpatialPolygons(Srl = poly, 
    #                        pO = 1:length(leg_lab), 
    #                        proj4string = "+proj=longlat +datum=WGS84")
    
    sp::coordinates(idw) <- ~x+y
    
    crs(idw) <- "+proj=longlat +datum=WGS84"
    
    idw <- spatialEco::erase.point(idw, sps)    
    

    
    
    idw_list[i][[1]] <- idw
  }
}

# a<-idw_list[77][[1]]; ggplot() + geom_stars(data = a)

# sapply(idw_list, st_transform(x = ., crs = "+proj=longlat +datum=WGS84"))
# 
# purrr::lmap(idw_list, st_transform(x = ., crs = "+proj=longlat +datum=WGS84"))

# save(plot_list, file = paste0("./maps/plot_list.Rdata"))
# save(plot_list, file = paste0("./data/plot_list.Rdata"))

save(idw_list, file = paste0("./maps/idw_list.Rdata"))
file.copy(from = "./maps/idw_list.Rdata", 
          to = "./data/idw_list.Rdata", 
          overwrite = TRUE)

# save(idw_list, file = paste0("./data/idw_list.Rdata"))
