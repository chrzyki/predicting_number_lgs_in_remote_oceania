source("01_requirements.R")

subregions <- read_tsv("data/oceania_subregions.tsv") %>% 
  dplyr::select(-Smallest_Island_group)

read_csv("data/RO_polygons_grouped_with_languages.csv") %>% 
  filter(!is.na(glottocodes)) %>%
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) %>% 
  distinct(Smallest_Island_group, Glottocode) %>% 
  group_by(Glottocode) %>% 
  summarise(Smallest_Island_group = paste(Smallest_Island_group, collapse = ", ")) %>% 
  full_join(subregions) %>% 
  write_tsv("data/oceania_subregions.tsv")
  
#read_csv("data/Remote_Oceania_Political_complex_and_more/RO_polygons_grouped_with_languages.csv") %>%
#  filter(!is.na(glottocodes)) %>%
#  group_by(Smallest_Island_group, glottocodes, Medium_only_merged_for_shared_language, Marck_group) %>% 
#  summarise(`Longitude (mean)` = mean(Longitude), 
#            `Latitude (mean)` = mean(Latitude)) %>% 
#  write_tsv("output/sheets/grouping_hierarchy_for_latex.tsv")
  
