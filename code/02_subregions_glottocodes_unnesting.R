source("01_requirements.R")

subregions <- read.delim("data/oceania_subregions.tsv", sep = "\t") %>% 
  dplyr::select(-Smallest_Island_group)

read.delim("data/RO_polygons_grouped_with_languages.csv", sep = ",") %>% 
  filter(!is.na(glottocodes)) %>%
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(Glottocode = trimws(glottocodes)) %>% 
  distinct(Smallest_Island_group, Glottocode) %>% 
  group_by(Glottocode) %>% 
  summarise(Smallest_Island_group = paste(Smallest_Island_group, collapse = ", ")) %>% 
  full_join(subregions, by = "Glottocode") %>% 
  write_tsv("data/oceania_subregions.tsv")
  
#read_csv("data/Remote_Oceania_Political_complex_and_more/RO_polygons_grouped_with_languages.csv") %>%
#  filter(!is.na(glottocodes)) %>%
#  group_by(Smallest_Island_group, glottocodes, Medium_only_merged_for_shared_language, Marck_group) %>% 
#  summarise(`Longitude (mean)` = mean(Longitude), 
#            `Latitude (mean)` = mean(Latitude)) %>% 
#  write_tsv("output/sheets/grouping_hierarchy_for_latex.tsv")
  
