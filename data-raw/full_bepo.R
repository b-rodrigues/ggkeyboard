library(dplyr)
library(stringr)

bepo <- readLines("data-raw/bepo.klc")


#bepo <- bepo %>%
#  str_replace_all("OEM_MINUS", "OEM_MINUS\t") %>%
#  str_replace_all("OEM_PLUS", "OEM_PLUS\t") %>%
#  str_replace_all("OEM_PERIOD", "OEM_PERIOD\t") %>%
#  str_replace_all("OEM_COMMA", "OEM_COMMA\t")

bepo_df <- data.table::fread(text = bepo,
                             sep = "\t",
                             fill = TRUE) %>%
  select(-V3) %>%
  filter(VK_ != "")

column_names <- c("SC", "VK", "Cap", "Press", "Shift_Press",
                  "Crap", "Alt_Press", "Shift_Alt_Press",
                  "Characters")

colnames(bepo_df) <- column_names

#View(bepo_df)

lookup <- read.csv("data-raw/sc_position_lookup.csv")

bepo_df <- bepo_df %>%
  left_join(lookup) %>%
  filter(!is.na(number))

data("full_iso")

bepo_df <- bepo_df %>%
  mutate(key_label = str_remove_all(Characters, "// "),
         key_label = str_sub(key_label, 1, 3),
         key_label = str_replace_all(key_label, " ", "\n"),
         key_label = stringi::stri_reverse(key_label))

full_iso_not_alpha <- full_iso %>%
  filter(key_type != "alphanumeric"|is.na(key_type)|is.na(key_label))

full_iso_alpha <- full_iso %>%
  filter(key_type == "alphanumeric")

bepo_df_alpha <- bepo_df %>%
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

full_iso_bepo <- bind_rows(bepo_df_alpha,
                             full_iso_not_alpha)


final_bepo <- full_iso_bepo %>%
  select(key, key_label, row, number)

a <- ggkeyboard::construct_keyboard(arrange(full_iso_bepo, row, number), layout = "iso") %>%
  select(-seq(1:3))

b <- ggkeyboard::construct_keyboard(full_iso, layout = "iso") %>%
  select(-seq(1:2))

afnor_bepo <- final_bepo %>%
  full_join(b) %>%
  mutate(size = ifelse(size == 5.25, 3, size))

usethis::use_data(afnor_bepo, overwrite = TRUE)

ggkeyboard2(keyboard = afnor_bepo, layout = "iso")
