library(scales)
library(dplyr)
library(purrr)
library(stringr)

french <- readLines("data-raw/french_text.txt")

min_max <- function(x){
  (x - min(x))/(max(x)-min(x))
}

characters_fr <-
  french %>%
  map(~strsplit(., split = "")) %>%
  unlist() %>%
  map(~strsplit(., split = "")) %>%
  unlist() %>%
  tolower() %>%
  #str_extract_all(pattern = "[:alpha:]") %>%
  unlist() %>%
  table() %>%  
  as.data.frame() %>%  
  filter("." != "") %>%  
  mutate(scaled = min_max(Freq)) %>%  
  mutate(frequencies =Freq/sum(Freq)) %>%
  arrange(desc(frequencies)) %>%
  rename(letter = ".",
         total = Freq)

ramp <- colour_ramp(c("light green", "red"))


characters_fr$fill <- ramp(characters_fr$scaled)

afnor_bepo2 %>%
  full_join(characters_fr, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% 
  ggkeyboard2(keyboard = ., layout = "iso")


afnor_azerty2 %>%
  full_join(characters_fr, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% 
  ggkeyboard2(keyboard = ., layout = "iso")


#data("afnor_bepo")
