library(dplyr)
library(stringr)

#download.file(
#  "https://raw.githubusercontent.com/springcomp/optimized-azerty-win/master/KBFRZ71.klc",
#  destfile = "data-raw/KBFRZ71.klc"
#)

azerty <- readLines("data-raw/KBFRZ71.klc")

start <- azerty %>%
  str_which("//SC")

end <- azerty %>%
  str_which("002f@")

azerty <- azerty[start:end]


azerty <- azerty[-c(1, 2, 3, 20, seq(56, 62))] %>%
  str_replace_all("OEM_MINUS", "OEM_MINUS\t") %>%
  str_replace_all("OEM_PLUS", "OEM_PLUS\t") %>%
  str_replace_all("OEM_PERIOD", "OEM_PERIOD\t") %>%
  str_replace_all("OEM_COMMA", "OEM_COMMA\t")

azerty_df <- data.table::fread(text = azerty,
                               sep = "\t") %>%
  select(-V3, -V11)

column_names <- c("SC", "VK", "Cap", "Press", "Shift_Press",
                  "Ctrl_Press", "Shift_Ctrl_Press", "Ctrl_Alt_Press",
                  "Shift_Ctrl_Alt_Press",
                  "Characters")

colnames(azerty_df) <- column_names

#View(azerty_df)

lookup <- read.csv("data-raw/sc_position_lookup.csv")

azerty_df <- azerty_df %>%
  left_join(lookup) %>%
  filter(!is.na(number))

data("full_iso")

#View(full_iso)

azerty_df <- azerty_df %>%
  mutate(key_label = str_remove_all(Characters, "// "),
         key_label = str_sub(key_label, 1, 3),
         key_label = str_replace_all(key_label, " ", "\n"),
         key_label = stringi::stri_reverse(key_label))

full_iso_not_alpha <- full_iso %>%
  filter(key_type != "alphanumeric"|is.na(key_type)|is.na(key_label))

full_iso_alpha <- full_iso %>%
  filter(key_type == "alphanumeric")

azerty_df_alpha <- azerty_df %>%
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

full_iso_azerty <- bind_rows(azerty_df_alpha,
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


final_azerty <- full_iso_azerty %>%
  select(key, key_label, row, number)

a <- ggkeyboard::construct_keyboard(arrange(full_iso_azerty, row, number), layout = "iso") %>%
  select(-seq(1:3))

b <- ggkeyboard::construct_keyboard(full_iso, layout = "iso") %>%
  select(-seq(1:2))

afnor_azerty <- final_azerty %>%
  full_join(b) %>%
  mutate(size = ifelse(size == 5.25, 3, size))

afnor_azerty2 <- afnor_azerty  %>%  
  mutate(key = str_replace(key, "00e0", "à"),
         key = str_replace(key, "00e9", "é"),
         key = str_replace(key, "00e8", "è"),
         key = str_replace(key, "00ea", "ê"),
         key = str_replace(key, "0028", "("),
         key = str_replace(key, "0029", ")"),
         key = str_replace(key, "2018", "‘"),
         key = str_replace(key, "2019", "’"),
         key = str_replace(key, "00ab", "«"),
         key = str_replace(key, "00bb", "»"),
         key = str_replace(key, "0027", "'"),
         key = str_replace(key, "005e", "^"),
         #key = str_replace(key, "003d", "°"),
         key = str_replace(key, "002d", "-"),
         #key = str_replace(key, "0025", "`"),
         key = str_replace(key, "002b", "+"),
         key = str_replace(key, "002f", "/"),
         key = str_replace(key, "\\*", "Numpad \\*"),
         key = str_replace(key, "002a", "\\*"),
         #key = str_replace(key, "005e", "!"),
         key = str_replace(key, "003c", "<"),
         key = str_replace(key, "002c", ","),
         key = str_replace(key, "003a", ":"),
         key = str_replace(key, "003b", ";"),
         key = str_replace(key, "0040", "@"),
         key = str_replace(key, "\\.", "Numpad \\."),
         key = str_replace(key, "-", "Numpad -"),
         key = str_replace(key, "/", "Numpad /"),
         key = str_replace(key, "002e", "."))


usethis::use_data(afnor_azerty, overwrite = TRUE)
usethis::use_data(afnor_azerty2, overwrite = TRUE)

ggkeyboard2(keyboard = afnor_azerty, layout = "iso")


