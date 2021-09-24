library(tidyverse)

list_langs <- read_csv("data/RO_polygons_grouped_with_languages.csv")  %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) %>% 
  distinct(glottocode, Smallest_Island_group, Marck_group, Medium_island_group_language_merged ) %>% 
  group_by(glottocode) %>% 
  summarise(Spoken_in_smallest_island_group = paste0(Smallest_Island_group, collapse = ", "),
            Spoken_in_overnight_group = paste0(unique(Marck_group), collapse = ", "),
            Spoken_in_language_merged_island_group = paste0(unique(Medium_island_group_language_merged), collapse = ", "))

glottolog_df <- read_tsv("../../personal-cldf-cookbook/R/output_tables/cldf_wide_df.tsv") %>% 
  dplyr::select(glottocode = Glottocode, Countries, Name)

list_langs %>% 
  left_join(glottolog_df) %>% 
  dplyr::select(Name, glottocode, Countries, everything()) %>% 
  write_tsv("data/language_table.tsv")

