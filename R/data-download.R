data_download <- function(cases_deaths = "cases") {

  library(googlesheets4)
  library(tidyverse)
  library(janitor)
  # country     time       cases_sum deaths_sum
  
  DF = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1mLx2L8nMaRZu0Sy4lyFniDewl6jDcgnxB_d0lHG-boc/edit?ts=5ea7297f#gid=1828101674", sheet = 5, skip = 3, na = c("-", ""))
  
  df_raw = DF %>%
    janitor::clean_names() %>%
    fill(region) %>%
    drop_na(habitantes) %>%
    mutate_if(is.list, as.numeric) %>%
    pivot_longer(6:16) %>%
    mutate(name = gsub("x", "", name),
           name = gsub("_", "-", name),
           name = gsub("(.*)", "\\1-2020", name),
           name = as.Date(name,  "%d-%m-%Y")) %>%
    mutate(value = replace_na(value, 0)) %>%
    
    group_by(comuna) %>% 
    mutate(cases_sum = cumsum(value)) %>% 
    rename(
      country = comuna,
      time = name) %>% 
    select(country, time, cases_sum) %>% 
    
    filter(country != "Total")
  
  # write_csv(df_raw, "outputs/data_chile.csv")

  DF_write = df_raw %>%

        mutate(source = "minsal") %>% 
    
    # calculate new infections
    arrange(time) %>%
    group_by(country) %>%
    mutate(cases_diff = cases_sum - lag(cases_sum)) %>% 
    mutate(cases_diff = replace_na(cases_diff, 0)) %>%
    mutate(
      # deaths_diff = deaths_sum - lag(deaths_sum),
           cases_diff_pct = cases_diff / lag(cases_sum)) %>% 
           # deaths_diff_pct = deaths_diff / lag(deaths_sum)) %>% 
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
      write_csv("outputs/raw_data.csv")


    
    
    
}

