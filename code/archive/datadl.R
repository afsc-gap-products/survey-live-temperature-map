
rm(list = ls())


source("functions.R") # App-specific files


files <- c(
  # AI
  "ai1983_2000", 
  "ai2002_2012",
  "ai2014_2018",
  
  # BS Slope
  "bsslope2002_2016", 

  # EBS
  "ebs1982_1984", 
  "ebs1985_1989",
  "ebs1990_1994", 
  "ebs1995_1999",
  "ebs2000_2004", 
  "ebs2005_2008",
  "ebs2009_2012", 
  "ebs2013_2016", 
  "ebs2017_2019",
  
  # NBS
  "nbs1982_2019", 
  
  # GOA
  "goa1984_1987", 
  "goa1990_1999",
  "goa2001_2005", 
  "goa2007_2013",
  "goa2015_2019"
  
  )

# Download data

url <- "https://apps-afsc.fisheries.noaa.gov/RACE/groundfish/survey_data/downloads/"

for (i in 1:length(files)) {
  download.file(url = paste0(url, files[i],".zip"), 
                destfile = paste0("./data/publicdata/zip/", files[i], ".zip"), 
                quiet = TRUE)
  unzip(zipfile = paste0("./data/publicdata/zip/", files[i], ".zip"), 
        overwrite = TRUE, 
        exdir = paste0("./data/publicdata/unzip/", files[i]))
  
  file.copy(from = paste0("./data/publicdata/unzip/", files[i], "/", files[i], ".csv"),
            to = paste0("./data/publicdata/"), 
            overwrite = TRUE)
  
  file.remove(paste0("./data/publicdata/unzip/", files[i], "/", files[i], ".csv"))
}

df.ls<-list()

srvy1 <- toupper(unique(sub("^([[:alpha:]]*).*", "\\1", files)))
a<-list.files(path = here::here("data", "publicdata"), 
              full.names = TRUE, 
              pattern = ".csv")

for (i in 1:length(a)){
  b <- read_csv(file = a[i])
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  urlname <- strsplit(x = a[i][[1]], split = "\\/")[[1]]
  urlname <- gsub(pattern = ".csv", 
                  replacement = "", 
                  x = urlname[length(urlname)])
  b$file <- paste0(url, urlname, ".zip")
  b$survey <- toupper(unique(sub("^([[:alpha:]]*).*", "\\1", urlname)))
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}

dat_cpue0<-SameColNames(df.ls)

dat_cpue <- dat_cpue0 |>
  dplyr::mutate(survey_long = dplyr::case_when(survey == "AI" ~ "Aleutian Islands", 
                                               survey == "ESSLOPE" ~ "Bering Sea Slope", 
                                               survey == "EBS" ~ "Eastern Bering Sea Shelf",  
                                               survey == "GOA" ~ "Gulf of Alaska",  
                                               survey == "NBS" ~ "Northern Bering Sea Shelf")) |>
  dplyr::mutate(map_area = dplyr::case_when(survey == "AI" ~ "ai", 
                                               survey == "ESSLOPE" ~ "bs.slope", 
                                               survey == "EBS" ~ "bs.south",  
                                               survey == "GOA" ~ "goa",  
                                               survey == "NBS" ~ "bs.north")) |>
  dplyr::mutate(stratum_shp = dplyr::case_when(survey == "AI" ~ "shp_ai", 
                                            survey == "ESSLOPE" ~ "shp_bsslope", 
                                            survey == "EBS" ~ "shp_ebs",  
                                            survey == "GOA" ~ "shp_goa",  
                                            survey == "NBS" ~ "shp_nbs")) |>
  dplyr::mutate(common0 = common) |>
  dplyr::mutate(common = str_to_sentence(common)) |>
  dplyr::mutate(survey_num = as.numeric(factor(survey))) |> 
  data.frame()

dat_cpue$common <- gsub(pattern = "unid.", replacement = "(unidentified)", x = dat_cpue$common, ignore.case = TRUE)

dat_cpue <- dat_cpue[!(is.na(dat_cpue$year)), ] # extra rows from .csv files
# dat_cpue$wtcpue[is.na(dat_cpue$wtcpue)] <- 0
dat_cpue$wtcpue[dat_cpue$wtcpue == -9999] <- NA
dat_cpue$numcpue[#is.na(dat_cpue$numcpue) | 
  dat_cpue$numcpue == -9999] <- NA
dat_cpue$bot_temp[dat_cpue$bot_temp == -9999] <- NA
dat_cpue$bot_depth[dat_cpue$bot_depth == -9999] <- NA
dat_cpue$surf_temp[dat_cpue$surf_temp == -9999] <- NA

dat_cpue$common[is.na(dat_cpue$common)] <- 
  dat_cpue$scientific[is.na(dat_cpue$common)]

dat_cpue$common[is.na(dat_cpue$common)] <- NULL #any rows with remaining NA's in common can be deleted

dat_cpue$common[is.na(dat_cpue$common)] <- 
  dat_cpue$scientific[is.na(dat_cpue$common)]

dat_cpue$common[is.na(dat_cpue$common)] <- NULL #any rows with remaining NA's in common can be deleted

# Set breaks for all years for each species
for (i in 1:length(unique(dat_cpue$common))) {
  
  df <- dat_cpue |>
    filter(common == unique(dat_cpue$common)[i])
  
  n.breaks <- 5
  
  var <- c("wtcpue", "numcpue", 
           "bot_temp", "bot_depth", "surf_temp")

  code_str <- glue::glue(
    'dat_cpue${var}_breaks[dat_cpue$common == unique(dat_cpue$common)[i]] <-
      paste0("c(",
             paste(quantile(x = dat_cpue${var}[dat_cpue$common == unique(dat_cpue$common)[i]],
                            na.rm = TRUE,
                            probs = c((1:(n.breaks-1))/n.breaks)), collapse = ", "), ")")
    
    ')
  
  eval(parse(text = code_str))
  
}

save(dat_cpue, dat_cpue0, 
     file = "./data/publicdata/all_data.Rdata")
