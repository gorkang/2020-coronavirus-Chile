data_download <- function(cases_deaths = "cases") {

  # DEBUG
    # library(googlesheets4)
    # suppressPackageStartupMessages(library(dplyr))
    # suppressPackageStartupMessages(library(janitor))
    # library(readr)
    # library(tidyr)
    # source("R/download_or_load.R")
  
  download_or_load(URL = "https://docs.google.com/spreadsheets/d/1mLx2L8nMaRZu0Sy4lyFniDewl6jDcgnxB_d0lHG-boc/edit?ts=5ea7297f#gid=1828101674",
                   file_name = "outputs/raw_data.csv", 
                   hours_threshold = 6,
                   maxTimes = 5)
  
  df_raw = read_csv(here::here("outputs/raw_data.csv"), 
                    col_types = 
                      cols(
                        .default = col_character(),
                        region = col_character(),
                        habitantes = col_double(),
                        comuna = col_character(),
                        incidencia_x_100_000 = col_double(),
                        crecimiento_ultima_semana = col_double(),
                        # por_asignar = col_logical(),
                        name = col_character(),
                        value = col_double(),
                        time = col_date(format = "")
                      ))
  
  df_raw_interpolated = df_raw %>%
    # filter(comuna == "Arica") %>%
    
    select(comuna, time, value) %>%  
    
    # Calculate cumulative sum
    group_by(comuna) %>% 
    mutate(cases_sum = cumsum(value)) %>% 
    rename(country = comuna) %>% 
    select(country, time, cases_sum) %>% 
    
    filter(country != "Total") %>%  
    
    mutate(source = "minsal") %>%
    drop_na(time) %>% 
    
    # Interpolate missing values
    group_by(country) %>% 
    complete(time = seq.Date(min(time), max(time), by="day")) %>% 
    mutate(cases_sum = zoo::na.approx(cases_sum)) %>% 
    ungroup() %>% 
    
    mutate(source = 
             case_when(
               is.na(source) ~ "interpolación",
               TRUE ~ source)) 
    
    
    
  
  # write_csv(df_raw, "outputs/data_chile.csv")

  DF_write = df_raw_interpolated %>%

    # calculate new infections
    arrange(time) %>%
    group_by(country) %>%
    mutate(cases_diff = cases_sum - lag(cases_sum)) %>% 
    mutate(cases_diff = replace_na(cases_diff, 0)) %>%
    # mutate(
    #   # deaths_diff = deaths_sum - lag(deaths_sum),
    #        cases_diff_pct = cases_diff / lag(cases_sum)) %>% 
    #        # deaths_diff_pct = deaths_diff / lag(deaths_sum)) %>% 
    ungroup() %>%
    filter(!is.na(cases_diff)) %>%
    arrange(country, time) %>%
    
    mutate(deaths_sum = NA_integer_,
           deaths_diff = NA_integer_,
           CFR_sum = NA_integer_, 
           CFR_diff = NA_integer_) %>% 
    
    select(country, time, cases_sum, cases_diff, deaths_sum, deaths_diff, source) #deaths_sum, deaths_diff

  
  
    # write_data
    DF_write %>% 
      write_csv("outputs/processed_data.csv")

}

