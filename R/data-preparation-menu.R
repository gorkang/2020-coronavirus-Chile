
raw_data_exists = file.info(here::here("outputs/raw_data.csv"))$mtime
if (is.na(raw_data_exists)) data_download()

# data_preparation()


dta_raw = read_csv(here::here("outputs/raw_data.csv"), 
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


# Menu vars ---------------------------------------------------------------

 DF_menu <- dta_raw %>%
  # filter(value > 1) %>% 
  filter(!comuna %in% c("Total")) %>% 
  arrange(desc(value)) %>% 
  distinct(comuna)

 V1_alternatives <<- DF_menu %>% 
  pull(comuna)

top_countries <<- DF_menu %>% 
  filter(!comuna %in% c("Total")) %>% 
  slice_head(n = 8, wt = value) %>% 
  pull(comuna)

last_date <<- max(as.Date(dta_raw$name, "%d-%m-%Y"), na.rm = TRUE)
