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
  mutate(scaled = min_max(Freq)) %>%  
  mutate(frequencies =Freq/sum(Freq)) %>%
  arrange(desc(frequencies)) %>%
  rename(letter = ".",
         total = Freq)

ramp <- colour_ramp(c("red", "green"))


characters_fr$fill <- ramp(characters_fr$scaled)

afnor_bepo %>%
  full_join(characters_fr, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% View
  ggkeyboard2(keyboard = ., layout = "iso")
  

data("afnor_bepo")
