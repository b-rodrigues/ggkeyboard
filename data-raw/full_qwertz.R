# layout downloaded from http://kbdlayout.info/KBDSF/
# file: XML CLDR LDML

library(dplyr)
library(xml2)
library(tidyr)

qwertz_xml <- read_xml("data-raw/KBDSF.xml")

extract_data <- function(xml_node){

  vk_sc <- xml_node %>%
    xml_attrs %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    pivot_wider(names_from = rowname, values_from = ".")

  press <- xml_node %>%
    xml_children %>%
    as.character %>%
    .[1] %>%
    str_extract("\".\"") %>%
    str_remove_all("\"")

  shift_press <- xml_node %>%
    xml_children %>%
    as.character %>%
    .[2] %>%
    str_extract("\".\"") %>%
    str_remove_all("\"")


  tribble(~Press, ~Shift_Press, press, shift_press) %>%
    bind_cols(vk_sc) 

}



qwertz_df <- xml_find_all(qwertz_xml, xpath = ".//PK") %>%
  xml_cdata %>%
  purrr::map_df(extract_data)

sc_of_interest <- readRDS("data-raw/sc_of_interest.RDS")


lookup <- read.csv("data-raw/sc_position_lookup.csv")

qwertz_df %>%
  mutate(SC = tolower(SC)) %>%  
  filter(SC %in% sc_of_interest) %>%
  left_join(lookup) %>%
  filter(!is.na(number))

# need some manual correction in some spots

qwertz_df %>% 
  mutate(key_label = paste0(Shift_Press, "\n", Press)) %>%
