download_or_load <- 
  
  purrr::safely(
    
    function(URL, file_name, hours_threshold = .5, maxTimes = 5) {
    
      # DEBUG
        # URL = "https://docs.google.com/spreadsheets/d/1mLx2L8nMaRZu0Sy4lyFniDewl6jDcgnxB_d0lHG-boc/edit?ts=5ea7297f#gid=1828101674"
        # file_name = "outputs/raw_data.csv"
        # hours_threshold = 6
        # maxTimes = 5

      library(dplyr)
      library(readr)
      library(tidyr)

            
      file_info = file.info(file_name)$mtime
      
      # If file does not exist, should_download
      if (is.na(file_info)) {
        
        message("File ", file_name, " does not exist, downloading")
        should_download = TRUE
        
      # If file exists check how old it is. If older than hours_threshold, should_download
      } else {
        
        time_downloaded = round(difftime(Sys.time(), file_info, units='hours'), 2)
        should_download = time_downloaded > hours_threshold
        message("File ", file_name, " downloaded ", time_downloaded, " hours ago, will ", "NOT"[!should_download], " download again")
        
      }
      
      # If should_download, try to download maxTimes times
      if (should_download == TRUE) {
        
        googlesheets4::gs4_deauth()
        DF = googlesheets4::read_sheet(URL, sheet = 5, skip = 3, na = c("-", ""))
        
        df_raw = DF %>%
          janitor::clean_names() %>%
          tidyr::fill(region) %>%
          drop_na(habitantes) %>%
          mutate_if(is.list, as.numeric) %>%
          pivot_longer(6:ncol(.)) %>%
          mutate(name = gsub("x", "", name),
                 name = gsub("_", "-", name),
                 name = gsub("(.*)", "\\1-2020", name),
                 time = as.Date(name,  "%d-%m-%Y")) %>%
          mutate(value = replace_na(value, 0))
        
        df_raw %>% write_csv(here::here(file_name))
        
        file_info_updated = file.info(file_name)$mtime
        time_downloaded_updated = round(difftime(Sys.time(), file_info_updated, units='hours'), 2)
        message("File ", file_name, " re-downloaded ", time_downloaded_updated, " hours ago\n")
        output = "downloaded"; output
        
      } else {
        
        message("Did not download again, file was ", time_downloaded, " hours old\n")
        output = "present"; output
        
      }
    }
  )

# download_or_load("temp_worldometers.html", URL = "https://www.worldometers.info/coronavirus/#countries")

# url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
# download_or_load("outputs/url_cases.csv", URL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

