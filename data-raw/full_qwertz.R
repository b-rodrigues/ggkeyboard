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

qwertz_df <- qwertz_df %>%
  mutate(SC = tolower(SC)) %>%  
  filter(SC %in% sc_of_interest) %>%
  left_join(lookup) %>%
  filter(!is.na(number)) %>%
  mutate(Press = case_when(row == 4 & number == 12 ~ "è",
                           row == 4 & number == 13 ~ "¨",
                           row == 3 & number == 11 ~ "é",
                           row == 3 & number == 12 ~ "à",
                           row == 2 & number == 2 ~ "<",
                           row == 5 & number == 1 ~ "§",
                           TRUE ~ Press)) %>%  
  mutate(Shift_Press = case_when(row == 5 & number == 3 ~ "\"",
                                 row == 5 & number == 5 ~ "ç",
                                 row == 5 & number == 7 ~ "&",
                                 row == 4 & number == 12 ~ "ü",
                                 row == 3 & number == 13 ~ "£",
                                 row == 3 & number == 11 ~ "ö",
                                 row == 3 & number == 12 ~ "ä",
                                 row == 2 & number == 2 ~ ">",
                                 row == 5 & number == 1 ~ "°",
                                 TRUE ~ Shift_Press))

# need some manual correction in some spots

qwertz_df <- qwertz_df %>% 
  mutate(key_label = paste0(Shift_Press, "\n", Press))

data("full_iso")

#View(full_iso)

full_iso_not_alpha <- full_iso %>%
  filter(key_type != "alphanumeric"|is.na(key_type)|is.na(key_label))

full_iso_alpha <- full_iso %>%
  filter(key_type == "alphanumeric")

qwertz_df_alpha <- qwertz_df %>%
  mutate(Press = str_remove_all(Press, "@")) %>%
  mutate(key = Press,
  #mutate(key = paste0("\\u", Press),
         #key_label = paste0("\\u", Press),
         key_type = "alphanumeric",
         width = 1,
         height = 1,
         layout = "60%") %>%
  mutate(layout = ifelse(row == 5 & number == 1, "tkl", layout)) %>%  
  select(key, key_label, key_type, row, number, width, height, layout)

full_iso_qwertz <- bind_rows(qwertz_df_alpha,
                             full_iso_not_alpha)


ggkeyboard2 <- function(keyboard = tkl,
                        palette = keyboard_palette("pastel"),
                        layout = c("ansi", "iso"),
                        font_family = "Arial Unicode MS",
                        font_size = 2,
                        adjust_text_colour = TRUE,
                        measurements = keyboard_measurements("default")) {
  layout <- match.arg(layout)

  keyboard_layout <- dplyr::case_when(
    any(keyboard[["layout"]] == "full") ~ "full",
    any(keyboard[["layout"]] == "tkl") ~ "tkl",
    any(keyboard[["layout"]] == "mac") ~ "mac",
    any(keyboard[["layout"]] == "steno") ~ "steno",
    any(keyboard[["layout"]] == "tkl_dvorak") ~ "tkl_dvorak",
    all(keyboard[["layout"]] == "60%") ~ "60%"
  )

  #if (layout == "iso") {
  #  keyboard <- convert_to_iso(keyboard, keyboard_layout)
  #}

#  keyboard <- construct_keyboard(keyboard = keyboard, palette = palette, layout = layout, font_size = font_size, adjust_text_colour = adjust_text_colour, measurements = measurements, keyboard_layout = keyboard_layout)

  keyboard_full <- construct_keyboard_outline(keyboard, keyboard_colour = palette[["keyboard"]])

  construct_plot(keyboard, keyboard_full, palette = palette, layout = layout, font_family = font_family, font_size = font_size, adjust_text_colour = adjust_text_colour, measurements = measurements, keyboard_layout = keyboard_layout)
}


final_qwertz <- full_iso_qwertz %>%
  select(key, key_label, row, number)

a <- ggkeyboard::construct_keyboard(arrange(full_iso_qwertz, row, number), layout = "iso") %>%
  select(-seq(1:3))

b <- ggkeyboard::construct_keyboard(full_iso, layout = "iso") %>%
  select(-seq(1:2))

ch_qwertz <- final_qwertz %>%
  full_join(b) %>%
  mutate(size = ifelse(size == 5.25, 3, size))


usethis::use_data(ch_qwertz, overwrite = TRUE)

ggkeyboard2(keyboard = ch_qwertz, layout = "iso")

