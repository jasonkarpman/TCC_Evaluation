library(sf)
library(tidyverse)
library(readxl)

#get original format from existing files
fresno_tracts <- st_read("./craigslist_data/tracts/fresno_tracts.geojson")
la_tracts <- st_read("./craigslist_data/tracts/la_tracts.geojson")

#get all tracts
Tracts <- lapply(excel_sheets("./data/TCC Census Tract List.xlsx"),
                 read_excel,
                 path = "./data/TCC Census Tract List.xlsx",
                 range = cell_cols("A:B"),
                 col_names = TRUE) %>% 
          bind_rows() %>% 
          mutate(GEOID = paste0("0", GEOID),
                 group = sub("^(\\S*\\s+\\S+).*", "\\1", Boundary)) %>% 
          select(-Boundary)

#import all tract boundary shapefile
CA_tract <- st_read("./data/cb_2019_06_tract_500k/cb_2019_06_tract_500k.shp") %>% 
            st_transform(4326) %>% 
            select(GEOID, NAME, geometry)
            
#generate geojson
pacoima_tracts <- Tracts %>% 
                  filter(str_detect(group, "Pacoima") == TRUE) %>% 
                  mutate(Group = case_when(str_detect(group, "Site") == TRUE ~ 1,
                                           TRUE ~ 0)) %>% 
                  select(-group) %>% 
                  left_join(CA_tract,
                            by = "GEOID")
st_write(pacoima_tracts, "./craigslist_data/tracts/pacoima_tracts.geojson")

stockton_tracts <- Tracts %>% 
                  filter(str_detect(group, "Stockton") == TRUE) %>% 
                  mutate(Group = case_when(str_detect(group, "Site") == TRUE ~ 1,
                                           TRUE ~ 0)) %>% 
                  select(-group) %>% 
                  left_join(CA_tract,
                            by = "GEOID")
                
st_write(stockton_tracts, "./craigslist_data/tracts/stockton_tracts.geojson")                  

#testing                  
test <- st_read("./craigslist_data/tracts/stockton_tracts.geojson") 