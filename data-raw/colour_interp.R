library(scales)
library(dplyr)
library(purrr)
library(stringr)

french <- readLines("~/six_to/ggkeyboard/data-raw/french_text.txt")

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
  rename(letter = ".",
         total = Freq) %>%  
  filter(letter != " ") %>%  
  filter(letter != "") %>%  
  mutate(scaled = min_max(total)) %>%  
  mutate(frequencies =total/sum(total)) %>%
  arrange(desc(frequencies)) 

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

german <- readLines("~/six_to/ggkeyboard/data-raw/german_text.txt")

characters_de <-
  german %>%
  map(~strsplit(., split = "")) %>%
  unlist() %>%
  map(~strsplit(., split = "")) %>%
  unlist() %>%
  tolower() %>%
                                        #str_extract_all(pattern = "[:alpha:]") %>%
  unlist() %>%
  table() %>%  
  as.data.frame() %>%  
  rename(letter = ".",
         total = Freq) %>%  
  filter(letter != " ") %>%  
  filter(letter != "") %>%  
  mutate(scaled = min_max(total)) %>%  
  mutate(frequencies =total/sum(total)) %>%
  arrange(desc(frequencies))

ramp <- colour_ramp(c("light green", "red"))


characters_de$fill <- ramp(characters_de$scaled)

afnor_bepo2 %>%
  full_join(characters_de, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% 
  ggkeyboard2(keyboard = ., layout = "iso")


afnor_azerty2 %>%
  full_join(characters_de, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% 
  ggkeyboard2(keyboard = ., layout = "iso")


lux <- readLines("~/six_to/ggkeyboard/data-raw/lb_text.txt")

characters_lb <-
  lux %>%
  map(~strsplit(., split = "")) %>%
  unlist() %>%
  map(~strsplit(., split = "")) %>%
  unlist() %>%
  tolower() %>%
  unlist() %>%
  table() %>%  
  as.data.frame() %>%  
  rename(letter = ".",
         total = Freq) %>%  
  mutate(letter = str_replace_all(letter, "ä", "à")) %>%  
  mutate(letter = str_replace_all(letter, "ü", "è")) %>%  
  filter(letter != " ") %>%  
  filter(letter != "") %>%  
  mutate(scaled = min_max(total)) %>%  
  mutate(frequencies =total/sum(total)) %>%
  arrange(desc(frequencies))

ramp <- colour_ramp(c("light green", "red"))


characters_lb$fill <- ramp(characters_lb$scaled)

afnor_bepo2 %>%
  full_join(characters_lb, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% 
  ggkeyboard2(keyboard = ., layout = "iso")


afnor_azerty2 %>%
  full_join(characters_lb, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% 
  ggkeyboard2(keyboard = ., layout = "iso")

ch_qwertz %>%
  full_join(characters_lb, by = c("key" = "letter")) %>%
  mutate(fill = coalesce(fill.y, fill.x)) %>% 
  ggkeyboard2(keyboard = ., layout = "iso")

  
