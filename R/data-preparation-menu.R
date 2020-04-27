
dta_raw = read_csv(here::here("outputs/raw_data.csv")) 

# Menu vars ---------------------------------------------------------------

 DF_menu <- dta_raw %>%
  # filter(value > 1) %>% 
  filter(!country %in% c("Total:", "Diamond Princess")) %>% 
  arrange(desc(cases_sum)) %>% 
  distinct(country)

 V1_alternatives <<- DF_menu %>% 
  pull(country)

top_countries <<- DF_menu %>% 
  filter(!country %in% c("Total", "Cruise Ship", "China", "Diamond Princess")) %>% 
  slice_head(n = 6, wt = value) %>% 
  pull(country)
