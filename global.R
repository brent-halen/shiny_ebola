if(!"data.table" %in% rownames(installed.packages())){install.packages("data.table", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(data.table)
if(!"magrittr" %in% rownames(installed.packages())){install.packages("magrittr", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(magrittr)
if(!"curl" %in% rownames(installed.packages())){install.packages("curl", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(curl)
if(!"readr" %in% rownames(installed.packages())){install.packages("readr", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(readr)
if(!"plyr" %in% rownames(installed.packages())){install.packages("plyr", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(plyr)
if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(dplyr)
if(!"stringr" %in% rownames(installed.packages())){install.packages("stringr", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(stringr)
if(!"reshape2" %in% rownames(installed.packages())){install.packages("reshape2", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(reshape2)
if(!"lubridate" %in% rownames(installed.packages())){install.packages("lubridate", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(lubridate)
if(!"ggplot2" %in% rownames(installed.packages())){install.packages("ggplot2", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(ggplot2)
if(!"ggthemes" %in% rownames(installed.packages())){install.packages("ggthemes", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(ggthemes)
if(!"grid" %in% rownames(installed.packages())){install.packages("grid", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(grid)
if(!"compiler" %in% rownames(installed.packages())){install.packages("compiler", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(compiler)


# Utility functions -------------------------------------------------------

#' Determine suspicion (suspect/probable/confirmed/deceased)
#'
#' @param case_type_vector A vector of composite case types according to the ebola_drc GitHub repo nomenclature.
#'
#' @return A vector of extracted suspicion indicators
#' 
determine_suspicion <- function(case_type_vector) {
  out <- character(length = length(case_type_vector))
  for(i in seq_along(case_type_vector)){
    if(case_type_vector[i] == "deaths"){
      out[i] <- "deceased"
    } else {
      out[i] <- stringr::str_extract(case_type_vector[i], "(suspect|probable|confirmed)")
    }
  }
  return(out)
}

#' Determine case status (new/case/deceased)
#'
#' @param case_type_vector A vector of composite case types according to the ebola_drc GitHub repo nomenclature.
#'
#' @return A vector of extracted case status markers.
#' 
determine_status <- function(case_type_vector) {
  out <- character(length = length(case_type_vector))
  for(i in seq_along(case_type_vector)){
    out[i] <- stringr::str_extract(case_type_vector[i], "(new|case|death)")
  }
  return(out)
}


# Reading DRC data from GitHub repo ---------------------------------------

download_drc_data <- function(start_date, source_file){
  current_drc_data <- readr::read_csv(source_file) %>%
    melt(value.name = "value", id.vars = c("event_date", "report_date", "health_zone"), factorsAsStrings = FALSE) %>%
    na.omit()
  
  current_drc_data$event_date <- lubridate::ymd(current_drc_data$event_date)
  current_drc_data$report_date <- lubridate::ymd(current_drc_data$report_date)
  current_drc_data$value <- as.numeric(current_drc_data$value)
  
  # Truncate early data
  
  current_drc_data %<>% data.frame() %>% filter(event_date >= start_date) %>% return()
}


# Filtering out 'empty' health zones --------------------------------------

filter_empty_health_zones <- function(drc_data){
  
  drc_data$health_zone %<>% as.factor()
  drc_data$variable %<>% as.character()
  
  relevant_health_zones <- drc_data %>% 
    subset(select = c("health_zone", "value")) %>% 
    dplyr::group_by(health_zone) %>% 
    dplyr::summarise(totals = sum(value, na.rm=TRUE)) %>% 
    dplyr::filter(totals > 0) %>% 
    use_series(health_zone)
  
  drc_data %<>% dplyr::filter(health_zone %in% relevant_health_zones) %>% return()
}

# Split by case count -----------------------------------------------------

append_case_status_and_type <- function(drc_data){
  drc_data$case_status <- determine_status(drc_data$variable)
  drc_data$case_type <- determine_suspicion(drc_data$variable)
  return(drc_data)
}


# Generate total values for the DRC ---------------------------------------

append_drc_totals <- function(drc_data){
  drc_data %<>% rbind.data.frame(
    drc_data %>%
      dplyr::group_by(event_date, report_date, variable, case_status, case_type) %>%
      dplyr::summarise(value = sum(value), health_zone = as.factor("DRC total"))
  ) %>% return()
}


# Declare factor values and orders ----------------------------------------

factorise_cases <- function(drc_data){
  regions <- drc_data %>% use_series(health_zone) %>% unique()
  drc_data$health_zone %<>% factor(levels = regions)
  drc_data$case_status %<>% factor(levels = c("death", "case", "new"))
  drc_data$case_type %<>% factor(levels = c("suspect", "probable", "confirmed", "deceased"))
  return(drc_data)
}



# FULL PROCESS: Obtain and process DRC data -------------------------------

get_drc_data <- function(start_date = "2018-05-10", source_file = "https://raw.githubusercontent.com/cmrivers/ebola_drc/master/drc/data.csv"){
  download_drc_data(start_date = start_date, source_file = source_file) %>%
    filter_empty_health_zones() %>%
    append_case_status_and_type() %>%
    append_drc_totals() %>%
    factorise_cases() %>%
    return()
}

bulk_rename_shapefiles <- function() {
  file_names <- dir(paste(getwd(), "shapefiles", sep = "/"), pattern = "Zone_")
  for(i in 1:length(file_names)){
    old_fn <- file_names[i]
    new_fn <- sub("Zone_.*_Puc", "DRC-health_zones", old_fn, perl = TRUE)
    
    file.rename(from = paste(getwd(), "shapefiles", old_fn, sep="/"),
                to = paste(getwd(), "shapefiles", new_fn, sep="/"))
  }
}

download_DRC_shapefiles <- function() {
  SHAPEFILE_DEST <- paste(getwd(), "shapefiles", "DRC-health-zones.zip", sep="/")
  curl_download("https://data.humdata.org/dataset/70ad1012-189c-4b49-b9f0-b97a71ec4c7b/resource/d7cd7eb0-2f82-42d2-8919-8e24e5982ee8/download/zone_ste_puc.zip",
                destfile = SHAPEFILE_DEST)
  
  unzip(SHAPEFILE_DEST, exdir = paste(getwd(), "shapefiles", sep="/"))
  file.remove(SHAPEFILE_DEST)
}

get_DRC_shapefiles <- function(){
  get_DRC_shapefiles()
  bulk_rename_shapefiles()
}

update_drc_data <- cmpfun(function(){
  current_drc_data <- Sys.time() %>%
    format("%d%H%M%S%b%Y") %>%
    paste("./www/raw_data/drc/", "drc-", ., ".csv", sep = "") %T>%
    curl_fetch_disk("https://raw.githubusercontent.com/cmrivers/ebola_drc/master/drc/data.csv", .) %>%
    #read_csv()
    fread()
  
  assign("current_drc_data", current_drc_data, globalenv())
  
  current_drc_data %<>% melt(value_name = "value", measure.vars = c("confirmed_cases", "confirmed_deaths", "probable_cases", "probable_deaths", "suspect_cases", "suspect_deaths", "ruled_out"))
  current_drc_data$event_date %<>% lubridate::ymd()
  current_drc_data$report_date %<>% lubridate::ymd()
  current_drc_data$value %<>% as.numeric()
  
  current_drc_data <- current_drc_data[current_drc_data$variable != "ruled_out",]
  
  current_drc_data %<>% bind_cols(., str_split_fixed(use_series(., variable), "_", 2)) %>% 
    subset(select = -c(variable)) %>% 
    set_colnames(c("event_date", "report_date", "health_zone", "value", "suspicion", "mm"))
  
  
  relevant_health_zones <- current_drc_data %>% 
    subset(select = c("health_zone", "value")) %>% 
    group_by(health_zone) %>% 
    summarise(totals = sum(value, na.rm=TRUE)) %>% 
    dplyr::filter(totals > 0) %>% 
    use_series(health_zone)
  
  current_drc_data %<>% {.[health_zone %in% relevant_health_zones,]}
  
  totals <- current_drc_data %>% group_by(event_date, report_date, suspicion, mm) %>% 
    summarise(value = sum(value), health_zone=as.factor("DRC total"))
  # Reorder totals to match the core dataset
  # totals <- totals[,c(1,2,6,5,3,4)]
  
  current_drc_data %<>% {bind_rows(.,totals)}
  
  regions <- current_drc_data %>% use_series(health_zone) %>% unique()
  regions <- regions[!regions == "DRC total"]
  regions %<>% c("DRC total")
  current_drc_data$health_zone_f <- factor(current_drc_data$health_zone, levels = regions)
  return(current_drc_data)
})


# Declare colour scheme ---------------------------------------------------

default_colour_scheme <- c(white = rgb(238, 238, 238, maxColorValue = 255),
                           light_primary = rgb(236, 231, 216, maxColorValue = 255),
                           dark_primary = rgb(127, 112, 114, maxColorValue = 255),
                           accent_red = rgb(240, 97, 114, maxColorValue = 255),
                           accent_blue = rgb(69, 82, 98, maxColorValue = 255))



# Cache DRC data set ------------------------------------------------------

drc_cache <- c(data_set = NULL, last_updated = NULL)

update_cache <- function(start_date = "2018-05-10", source_file) {
  print(paste("Updating cache with source file", source_file))
  drc_cache[["data_set"]] <- get_drc_data(start_date = start_date, source_file = source_file)
  drc_cache[["last_updated"]] <- Sys.time()
  return(drc_cache[["data_set"]])
}

print_cache_analysis <- function(cache_expiry, cache_age) {
  base_message <- paste("Cache expiry: ", cache_expiry, "s | Cache age: ", round(cache_age, digits = 0), " |>", sep = "")
  
  if (cache_age > cache_expiry) {
    print(paste(base_message, "CACHE EXPIRED"))
  } else {
    print(paste(base_message, "CACHE VALID"))
  }
}

is_cache_valid <- function(cache_expiry, quiet = FALSE) {
  if(is.null(drc_cache[[last_updated]])){
    cache_age <- difftime(as.Date("1970-01-01"), Sys.time(), units = "secs")
  } else {
    cache_age <- difftime(drc_cache[[last_updated]], Sys.time(), units = "secs") 
  }
  
  cache_age %<>% as.numeric() %>% abs()
  
  if (!quiet) {
    print_cache_analysis(cache_expiry, cache_age)
  }
  
  if (cache_age < cache_expiry) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


get_drc_dataset <- function(start_date = "2018-05-10", cache_expiry = 600, source_file = "https://raw.githubusercontent.com/cmrivers/ebola_drc/master/drc/data.csv"){
  if(is.null(drc_cache[["last_updated"]])){
    cache_valid <- FALSE
  } else {
    cache_valid <- is_cache_valid(cache_expiry)
  }
  
  if(cache_valid){
    return(drc_cache[[data_set]])
  } else {
    update_cache(source_file=source_file) %>% return()
  }
}


# PLOT 1: CASE STATUS BY HEALTH ZONE --------------------------------------

plot_case_status_by_health_zone <- function(drc_data,
                                            colour_scheme = default_colour_scheme,
                                            destination_path = paste(getwd(), "visualisations", "drc", sep = "/"),
                                            start_date = "2018-05-10",
                                            major_breaks = "7 days",
                                            minor_breaks = "7 days",
                                            file_prefix = "OVERVIEW",
                                            device = "png",
                                            width = 8,
                                            height = 6) {
  
  p <- ggplot(drc_data[drc_data$case_status != "new",], aes(x = event_date, y = value)) +
    facet_grid(health_zone ~ case_type) +
    geom_line(aes(group = case_type, colour = case_type, alpha = case_type)) + 
    geom_point(aes(colour = case_type, alpha = case_type)) +
    xlim(c(as.Date(start_date), Sys.Date())) +
    scale_x_date(date_labels = "%m/%d", date_breaks = major_breaks, date_minor_breaks = minor_breaks) +
    scale_colour_manual(values = c("suspect" = colour_scheme[["accent_blue"]],
                                   "probable" = colour_scheme[["accent_blue"]],
                                   "confirmed" = colour_scheme[["accent_blue"]],
                                   "deceased" = colour_scheme[["accent_red"]]),
                        guide = FALSE) + 
    scale_alpha_manual(values = c("suspect" = 0.2,
                                  "probable" = 0.4,
                                  "confirmed" = 0.6,
                                  "deceased" = 0.8),
                       guide = FALSE) + 
    ylab("Cases") +
    xlab("Date") +
    ggtitle(paste("Daily EBOV status", "DRC", max(drc_data$event_date), sep = " - ")) +
    labs(subtitle = "Chris von Csefalvay (@chrisvcsefalvay)/CBRD (cbrd.co)") +
    theme(panel.spacing.y = unit(0.6, "lines"), 
          panel.spacing.x = unit(1, "lines"),
          plot.title = element_text(colour = colour_scheme[["accent_blue"]]),
          plot.subtitle = element_text(colour = colour_scheme[["accent_blue"]]),
          axis.line = element_line(colour = colour_scheme[["dark_primary"]]),
          panel.background = element_rect(fill = colour_scheme[["white"]]),
          panel.grid.major = element_line(colour = colour_scheme[["light_primary"]]),
          panel.grid.minor = element_line(colour = colour_scheme[["light_primary"]]),
          strip.background = element_rect(fill = colour_scheme[["accent_blue"]]),
          strip.text = element_text(colour = colour_scheme[["light_primary"]])) 
  
  
  Sys.time() %>%
    format("%d%H%M%S%b%Y") %>%
    toupper() %>%
    paste("DRC-EBOV-", file_prefix, "-", ., ".", device, sep="") %>%
    ggsave(plot = p, device=device, path=destination_path, width = width, height = height)
}

plot_case_status_by_health_zone_plot_return <- function(drc_data,
                                            colour_scheme = default_colour_scheme,
                                            destination_path = paste(getwd(), "visualisations", "drc", sep = "/"),
                                            start_date = "2018-05-10",
                                            major_breaks = "7 days",
                                            minor_breaks = "7 days",
                                            file_prefix = "OVERVIEW",
                                            device = "png",
                                            width = 8,
                                            height = 6) {
  
  p <- ggplot(drc_data[drc_data$case_status != "new",], aes(x = event_date, y = value)) +
    facet_grid(health_zone ~ case_type) +
    geom_line(aes(group = case_type, colour = case_type, alpha = case_type)) + 
    geom_point(aes(colour = case_type, alpha = case_type)) +
    xlim(c(as.Date(start_date), Sys.Date())) +
    scale_x_date(date_labels = "%m/%d", date_breaks = major_breaks, date_minor_breaks = minor_breaks) +
    scale_colour_manual(values = c("suspect" = colour_scheme[["accent_blue"]],
                                   "probable" = colour_scheme[["accent_blue"]],
                                   "confirmed" = colour_scheme[["accent_blue"]],
                                   "deceased" = colour_scheme[["accent_red"]]),
                        guide = FALSE) + 
    scale_alpha_manual(values = c("suspect" = 0.2,
                                  "probable" = 0.4,
                                  "confirmed" = 0.6,
                                  "deceased" = 0.8),
                       guide = FALSE) + 
    ylab("Cases") +
    xlab("Date") +
    ggtitle(paste("Daily EBOV status", "DRC", max(drc_data$event_date), sep = " - ")) +
    labs(subtitle = "Chris von Csefalvay (@chrisvcsefalvay)/CBRD (cbrd.co)") +
    theme(panel.spacing.y = unit(0.6, "lines"), 
          panel.spacing.x = unit(1, "lines"),
          plot.title = element_text(colour = colour_scheme[["accent_blue"]]),
          plot.subtitle = element_text(colour = colour_scheme[["accent_blue"]]),
          axis.line = element_line(colour = colour_scheme[["dark_primary"]]),
          panel.background = element_rect(fill = colour_scheme[["white"]]),
          panel.grid.major = element_line(colour = colour_scheme[["light_primary"]]),
          panel.grid.minor = element_line(colour = colour_scheme[["light_primary"]]),
          strip.background = element_rect(fill = colour_scheme[["accent_blue"]]),
          strip.text = element_text(colour = colour_scheme[["light_primary"]])) 
  
  
  # Sys.time() %>%
  #   format("%d%H%M%S%b%Y") %>%
  #   toupper() %>%
  #   paste("DRC-EBOV-", file_prefix, "-", ., ".", device, sep="") %>%
  #   ggsave(plot = p, device=device, path=destination_path, width = width, height = height)
  return(p)
}

# PLOT 2: EPI CURVE -------------------------------------------------------

plot_epi_curve <- function(drc_data,
                           colour_scheme = default_colour_scheme,
                           destination_path = paste(getwd(), "visualisations", "drc", sep = "/"),
                           start_date = "2018-05-10",
                           major_breaks = "7 days",
                           minor_breaks = "7 days",
                           file_prefix = "EPICURVE",
                           device = "png",
                           width = 8,
                           height = 6){
  
  
  q <- ggplot(drc_data, aes(x = event_date, y = value)) +
    facet_grid(health_zone ~ .) +
    geom_col(aes(group = case_status, colour = case_status, fill = case_status)) + 
    scale_fill_manual(values = c("new" = colour_scheme[["accent_red"]],
                                 "case" = colour_scheme[["dark_primary"]],
                                 "death" = colour_scheme[["accent_blue"]]),
                      name = "Case status",
                      breaks = c("new", "case", "death"),
                      labels = c("new", "alive", "deceased")) +
    scale_colour_manual(values = c("new" = alpha(colour_scheme[["accent_red"]], 0.8),
                                   "case" = alpha(colour_scheme[["dark_primary"]], 0.7),
                                   "death" = alpha(colour_scheme[["accent_blue"]], 0.95)),
                        guide = FALSE) +
    xlim(c(as.Date(start_date), Sys.Date())) +
    scale_x_date(date_labels = "%m/%d", date_breaks = major_breaks, date_minor_breaks = minor_breaks) +
    ylab("Cases") +
    xlab("Date") +
    ggtitle(paste("Daily EBOV epicurve", "DRC", max(drc_data$event_date), sep = " - ")) +
    labs(subtitle = "Chris von Csefalvay (@chrisvcsefalvay)/CBRD (cbrd.co)") +
    theme(panel.spacing.y = unit(0.6, "lines"), 
          panel.spacing.x = unit(1, "lines"),
          plot.title = element_text(colour = colour_scheme[["accent_blue"]]),
          plot.subtitle = element_text(colour = colour_scheme[["accent_blue"]]),
          axis.line = element_line(colour = colour_scheme[["dark_primary"]]),
          panel.background = element_rect(fill = colour_scheme[["white"]]),
          panel.grid.major = element_line(colour = colour_scheme[["light_primary"]]),
          panel.grid.minor = element_line(colour = colour_scheme[["light_primary"]]),
          strip.background = element_rect(fill = colour_scheme[["accent_blue"]]),
          strip.text = element_text(colour = colour_scheme[["light_primary"]]))  + theme(plot.caption = element_text(vjust = 1), 
                                                                                         legend.position = "bottom", legend.direction = "horizontal")
  
  
  Sys.time() %>%
    format("%d%H%M%S%b%Y") %>%
    toupper() %>%
    paste("DRC-EBOV-", file_prefix, "-", ., ".", device, sep="") %>%
    ggsave(plot = q, device=device, path=destination_path, width = width, height = height)
}

plot_epi_curve_plot_return <- function(drc_data,
                           colour_scheme = default_colour_scheme,
                           destination_path = paste(getwd(), "visualisations", "drc", sep = "/"),
                           start_date = "2018-05-10",
                           major_breaks = "7 days",
                           minor_breaks = "7 days",
                           file_prefix = "EPICURVE",
                           device = "png",
                           width = 8,
                           height = 6){
  
  
  q <- ggplot(drc_data, aes(x = event_date, y = value)) +
    facet_grid(health_zone ~ .) +
    geom_col(aes(group = case_status, colour = case_status, fill = case_status)) + 
    scale_fill_manual(values = c("new" = colour_scheme[["accent_red"]],
                                 "case" = colour_scheme[["dark_primary"]],
                                 "death" = colour_scheme[["accent_blue"]]),
                      name = "Case status",
                      breaks = c("new", "case", "death"),
                      labels = c("new", "alive", "deceased")) +
    scale_colour_manual(values = c("new" = alpha(colour_scheme[["accent_red"]], 0.8),
                                   "case" = alpha(colour_scheme[["dark_primary"]], 0.7),
                                   "death" = alpha(colour_scheme[["accent_blue"]], 0.95)),
                        guide = FALSE) +
    xlim(c(as.Date(start_date), Sys.Date())) +
    scale_x_date(date_labels = "%m/%d", date_breaks = major_breaks, date_minor_breaks = minor_breaks) +
    ylab("Cases") +
    xlab("Date") +
    ggtitle(paste("Daily EBOV epicurve", "DRC", max(drc_data$event_date), sep = " - ")) +
    labs(subtitle = "Chris von Csefalvay (@chrisvcsefalvay)/CBRD (cbrd.co)") +
    theme(panel.spacing.y = unit(0.6, "lines"), 
          panel.spacing.x = unit(1, "lines"),
          plot.title = element_text(colour = colour_scheme[["accent_blue"]]),
          plot.subtitle = element_text(colour = colour_scheme[["accent_blue"]]),
          axis.line = element_line(colour = colour_scheme[["dark_primary"]]),
          panel.background = element_rect(fill = colour_scheme[["white"]]),
          panel.grid.major = element_line(colour = colour_scheme[["light_primary"]]),
          panel.grid.minor = element_line(colour = colour_scheme[["light_primary"]]),
          strip.background = element_rect(fill = colour_scheme[["accent_blue"]]),
          strip.text = element_text(colour = colour_scheme[["light_primary"]]))  + theme(plot.caption = element_text(vjust = 1), 
                                                                                         legend.position = "bottom", legend.direction = "horizontal")
  
  
  # Sys.time() %>%
  #   format("%d%H%M%S%b%Y") %>%
  #   toupper() %>%
  #   paste("DRC-EBOV-", file_prefix, "-", ., ".", device, sep="") %>%
  #   ggsave(plot = q, device=device, path=destination_path, width = width, height = height)
  return(q)
}

# Old Global Functions ----------------------------------------------------------------------

# update_crc_data_2 <- cmpfun(function(){
#   # current_drc_data <- Sys.time() %>%
#   #   format("%d%H%M%S%b%Y") %>%
#   #   paste("./www/raw_data/drc/", "drc-", ., ".csv", sep = "") %T>%
#   #   curl_fetch_disk("https://raw.githubusercontent.com/cmrivers/ebola_drc/master/drc/data.csv", .) %>%
#   #   read_csv()
#   
#   current_drc_data <- fread("https://raw.githubusercontent.com/cmrivers/ebola_drc/master/drc/data.csv") 
#   
#   current_drc_data %<>% melt(value_name = "value", measure.vars = c("confirmed_cases", "confirmed_deaths", "probable_cases", "probable_deaths", "suspect_cases", "suspect_deaths", "ruled_out"))
#   current_drc_data$event_date %<>% lubridate::ymd()
#   current_drc_data$report_date %<>% lubridate::ymd()
#   current_drc_data$value %<>% as.numeric()
#   
#   current_drc_data <- current_drc_data[variable != "ruled_out",]
#   
#   current_drc_data %<>% bind_cols(., data.table(str_split_fixed(use_series(., variable), "_", 2))) %>% 
#     subset(select = -c(variable)) %>% 
#     set_colnames(c("event_date", "report_date", "health_zone", "value", "suspicion", "mm"))
#   
#   
#   relevant_health_zones <- current_drc_data %>% 
#     subset(select = c("health_zone", "value")) %>% 
#     group_by(health_zone) %>% 
#     summarise(totals = sum(value, na.rm=TRUE)) %>% 
#     dplyr::filter(totals > 0) %>% 
#     use_series(health_zone)
#   
#   current_drc_data %<>% {.[health_zone %in% relevant_health_zones,]}
#   
#   totals <- current_drc_data %>% group_by(event_date, report_date, suspicion, mm) %>% 
#     summarise(value = sum(value), health_zone=as.factor("DRC total"))
#   # Reorder totals to match the core dataset
#   # totals <- totals[,c(1,2,6,5,3,4)]
#   
#   current_drc_data %<>% {bind_rows(.,totals)}
#   
#   regions <- current_drc_data %>% use_series(health_zone) %>% unique()
#   regions <- regions[!regions == "DRC total"]
#   regions %<>% c("DRC total")
#   current_drc_data$health_zone_f <- factor(current_drc_data$health_zone, levels = regions)
#   return(current_drc_data)
# })


# colour_scheme <- c(white = rgb(238, 238, 238, maxColorValue = 255),
#                    light_primary = rgb(236, 231, 216, maxColorValue = 255),
#                    dark_primary = rgb(127, 112, 114, maxColorValue = 255),
#                    accent_red = rgb(240, 97, 114, maxColorValue = 255),
#                    accent_blue = rgb(69, 82, 98, maxColorValue = 255))

generate_drc_plot <- cmpfun(function(current_drc_data, colour_scheme){
  p <- ggplot(current_drc_data, aes(x=event_date, y=value)) +
    # Title and subtitle
    ggtitle(paste("Daily EBOV status", "DRC", Sys.Date(), sep=" - ")) +
    labs(subtitle = "(c) Chris von Csefalvay/CBRD (cbrd.co) - @chrisvcsefalvay") +
    
    # This facets the plot based on the factor vector we created ear 
    facet_grid(health_zone_f ~ suspicion) +
    geom_path(aes(group = mm, colour = mm, alpha = mm), na.rm = TRUE) +
    geom_point(aes(colour = mm, alpha = mm)) +
    # Axis labels
    ylab("Cases") +
    xlab("Date") +
    # The x-axis is between the first notified case and the last
    xlim(c("2018-05-08", Sys.Date())) +
    scale_x_date(date_breaks = "7 days", date_labels = "%m/%d") +
    # Because often there's an overlap and cases that die on the day of registration
    # tend to count here as well, some opacity is useful.
    scale_alpha_manual(values = c("cases" = 0.5, "deaths" = 0.8)) +
    scale_colour_manual(values = c("cases" = colour_scheme[["accent_blue"]], "deaths" = colour_scheme[["accent_red"]])) +
    # Ordinarily, I have these derive from a theme package, but they're very good
    # defaults and starting poinnnnnntsssssts
    theme(panel.spacing.y = unit(0.6, "lines"), 
          panel.spacing.x = unit(1, "lines"),
          plot.title = element_text(colour = colour_scheme[["accent_blue"]]),
          plot.subtitle = element_text(colour = colour_scheme[["accent_blue"]]),
          axis.line = element_line(colour = colour_scheme[["dark_primary"]]),
          panel.background = element_rect(fill = colour_scheme[["white"]]),
          panel.grid.major = element_line(colour = colour_scheme[["light_primary"]]),
          panel.grid.minor = element_line(colour = colour_scheme[["light_primary"]]),
          strip.background = element_rect(fill = colour_scheme[["accent_blue"]]),
          strip.text = element_text(colour = colour_scheme[["light_primary"]])
    )
  return(p)
})

