source("requirements.R")

source("subregions_glottocodes_unnesting.R")

Glottolog <- read_tsv("../Glottolog_look_up_table/Glottolog_lookup_table_Hedvig_output/Glottolog_lookup_table_Heti_edition.tsv") %>% 
  dplyr::select(glottocode, Language_level_ID, level)

Polygon_lgs_glottocodes_unnested <- read_csv("data/Remote_Oceania_Political_complex_and_more/RO_polygons_grouped_with_languages.csv") %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) %>% 
  distinct() 

Polygon_lgs_count_smallest <- Polygon_lgs_glottocodes_unnested %>% 
  distinct(Smallest_Island_group, glottocode) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(lg_count_smallest = n()) 

Polygon_lgs_count_medium <- Polygon_lgs_glottocodes_unnested %>% 
  distinct(Medium_only_merged_for_shared_language, glottocode) %>% 
  group_by(Medium_only_merged_for_shared_language) %>%  
  summarise(lg_count_medium = n()) 

Polygon_lgs_count_Marck <- Polygon_lgs_glottocodes_unnested %>%   
  distinct(Marck_group, glottocode) %>% 
  group_by(Marck_group) %>% 
  summarise(lg_count_Marck = n())

water_areas <- read_tsv("output/sheets/Water_area.tsv")

Dplace_soc <- read_csv("../dplace-data/datasets/Binford/societies.csv") %>% 
  full_join(read_csv("../dplace-data/datasets/SCCS/societies.csv" )) %>% 
  full_join(read_csv("../dplace-data/datasets/WNAI/societies.csv"))  %>% 
  dplyr::select("society_id" = "id", Long, Lat, glottocode) %>% 
  distinct()

isolation_marck <- read_tsv("data/Remote_Oceania_Political_complex_and_more/isolation_marck_group.tsv") %>% 
  dplyr::select(Marck_group = Marck_group_left, dist) %>% 
  mutate(Marck_group =str_replace_all(Marck_group, "Vanuatu \\(greater\\)", "Vanuatu and Temotu"))

isolation_medium <- read_tsv("data/Remote_Oceania_Political_complex_and_more/isolation_medium_island.tsv") %>%
  dplyr::select(Medium_only_merged_for_shared_language = Medium_only_merged_for_shared_language_left, dist)



polygon_geo <- read_csv("data/Remote_Oceania_Political_complex_and_more/RO_polygons_grouped_with_languages.csv", na = "NA") %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(Unique_ID = as.character(Unique_ID)) %>% 
  full_join(water_areas)  %>% 
  distinct(Smallest_Island_group, Unique_ID, .keep_all = T) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(sum_area = sum(`AREA (sq km)`), 
            sum_shoreline = sum(`COASTLINE (km) (perimeter)`), 
            mean_lat = mean(Latitude), 
            mean_long = mean(Longitude), 
            sum_water_area = sum(water_area), 
            Melanesia_or_not = dplyr::first(Melanesia_or_not), 
            max_long = max(Longitude), 
            min_long = min(Longitude), 
            max_lat = max(Latitude), 
            min_lat = min(Latitude))

polygon_geo_grouping_hierarchy <- read_csv("data/Remote_Oceania_Political_complex_and_more/RO_polygons_grouped_with_languages.csv") %>% 
  filter(!is.na(glottocodes)) %>% 
  distinct(Marck_group, Medium_only_merged_for_shared_language, Smallest_Island_group, glottocodes) %>% 
  full_join(Polygon_lgs_count_medium) %>% 
  full_join(Polygon_lgs_count_Marck) %>% 
  full_join(Polygon_lgs_count_smallest) %>% 
  full_join(polygon_geo)

#distinct colors
n <- length(unique(polygon_geo_grouping_hierarchy$Smallest_Island_group))
color_vector <- sample(distinctColorPalette(n), size = n)
polygon_geo_grouping_hierarchy$smallest_island_color <- color_vector[as.factor(polygon_geo_grouping_hierarchy$Smallest_Island_group)]

n <- length(unique(polygon_geo_grouping_hierarchy$Marck_group))
color_vector <- sample(distinctColorPalette(n), size = n)
polygon_geo_grouping_hierarchy$Marck_group_color <- color_vector[as.factor(polygon_geo_grouping_hierarchy$Marck_group)]

n <- length(unique(polygon_geo_grouping_hierarchy$Medium_only_merged_for_shared_language))
color_vector <- sample(distinctColorPalette(n), size = n)
polygon_geo_grouping_hierarchy$medium_group_color <- color_vector[as.factor(polygon_geo_grouping_hierarchy$Medium_only_merged_for_shared_language)]

write_tsv(polygon_geo_grouping_hierarchy, "output/sheets/Polygon_hierarchy_stats.tsv")

subregions <- read_tsv("data/Remote_Oceania_Political_complex_and_more/oceania_subregions.tsv") %>% 
  filter(Near_Remote_Oceania == "Remote Oceania") %>% 
  filter(level == "language") %>% 
  filter(Family_name == "Austronesian") %>%
  filter(Glottocode != "Ngaapuhi") %>% 
  filter(Glottocode != "Ngaati Kohungunu") %>% 
  filter(Glottocode != "Ngaati Porou") %>% 
  filter(Glottocode != "sout2867") %>% 
  filter(Glottocode != "tara1315") %>% 
  filter(Glottocode != "Te Ao Puri") %>% 
  filter(Glottocode != "Tuuhoe") %>% 
  filter(Glottocode != "Tuuwharetoa") %>% 
  filter(Glottocode != "Waikato") %>% 
  filter(Glottocode != "fiji1243") %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  dplyr::rename(Glottocode_spec = Glottocode) %>% 
  dplyr::rename(Glottocode = Glottocode_language_level) %>% 
  distinct(Glottocode_spec, Glottocode, Smallest_Island_group, Smallest_Island_group_main) %>% 
  full_join(polygon_geo_grouping_hierarchy)

library(readODS)
pol_complex <- readODS::read_ods("data/Remote_Oceania_Political_complex_and_more/Remote_oceania_pol_complex_hedvig_code_latex.ods", sheet = 1) %>%
  dplyr::select(Glottocode = glottocode, pol_complex_code_Hedvig = "Political complexity (EA033)" ) %>% 
#pol_complex <- read_csv("data/Remote_Oceania_Political_complex_and_more/Remote_oceania_pol_complex_hedvig_code.csv") %>% 
#  dplyr::select(Glottocode = Language_level_ID, pol_complex_code_Hedvig) %>%
  left_join(subregions) %>% 
  filter(!is.na(Smallest_Island_group)) %>% 
  distinct(Smallest_Island_group, pol_complex_code_Hedvig) %>% 
  filter(!is.na(pol_complex_code_Hedvig))

EA032 <- read_csv("data/Remote_Oceania_Political_complex_and_more/EA032.csv") %>% 
  dplyr::select(glottocode = language_glottocode, EA032_code = code) %>% 
  left_join(Glottolog) %>% 
  dplyr::select(-glottocode) %>% 
  dplyr::rename(Glottocode = Language_level_ID) %>% 
  inner_join(subregions) %>% 
  inner_join(subregions) %>%
  dplyr::select(-Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = Smallest_Island_group_main)  %>% 
   mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
   unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(EA032_code = mean(EA032_code))

Dplace_npp <- read_csv("data/Remote_Oceania_Political_complex_and_more/dplace_NPP.csv") %>% 
  dplyr::select(glottocode = language_glottocode, NPP = code) %>% 
  left_join(Glottolog) %>% 
  dplyr::select(-glottocode) %>% 
  dplyr::rename(Glottocode = Language_level_ID) %>% 
  inner_join(subregions) %>% 
  inner_join(subregions) %>%
  dplyr::select(-Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = Smallest_Island_group_main)  %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(NPP = mean(NPP))

NetPrimaryProductionPredictability <- read_csv("data/Remote_Oceania_Political_complex_and_more/NetPrimaryProductionPredictability.csv") %>% 
  dplyr::select(glottocode = language_glottocode, Predictability_code = code) %>% 
  left_join(Glottolog) %>% 
  dplyr::select(-glottocode) %>% 
  dplyr::rename(Glottocode = Language_level_ID) %>% 
  inner_join(subregions) %>% 
  inner_join(subregions) %>%
  dplyr::select(-Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = Smallest_Island_group_main)  %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(Predictability_code = mean(Predictability_code))


ecoClimate_data <- read_tsv("output/sheets/RO_polygons_grouped_with_languages_with_climate_grouped.tsv")

#dplace rainfall = ml/m2/month
#rolett_diamon rainfall = mm/y

##dates
dates <- read_xlsx("data/Remote_Oceania_Political_complex_and_more/island_group_settlement_date.xlsx") %>% 
  rename(settlement_date_grouping_finer = "Time depth settlement group", Smallest_Island_group = `Smaller specific island group`, `Settlement date oldest date` = `Oldest date`) %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>%
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  group_by(Smallest_Island_group) %>% 
  summarise(oldest_date = max(`Settlement date oldest date`), settlement_date_grouping_finer = min(settlement_date_grouping_finer, na.rm = T), settlement_date_grouping_coarser = min(settlement_date_grouping_coarser, na.rm = T)) 

dates$settlement_date_grouping_finer = (max(dates$settlement_date_grouping_finer)+1) - dates$settlement_date_grouping_finer

dates$settlement_date_grouping_coarser = (max(dates$settlement_date_grouping_coarser)+1) - dates$settlement_date_grouping_coarser

##All
Island_group_all_sep <- polygon_geo_grouping_hierarchy %>% 
  full_join(polygon_geo) %>% 
  full_join(dates) %>% 
  full_join(Dplace_npp) %>%
  full_join(pol_complex)  %>% 
  full_join(NetPrimaryProductionPredictability) %>% 
  full_join(ecoClimate_data) %>% 
  full_join(EA032) %>% 
  distinct()   

write_tsv(Island_group_all_sep, "output/sheets/RO_Hedvig_aggregate_all_obs_sep.tsv")

#Aggregation over smallest unit
Island_group_summarised_smallest <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Smallest_Island_group) %>%
  dplyr::summarise(mean_NetPrimaryProductionPredictability = mean(Predictability_code, na.rm = T), 
            max_NetPrimaryProductionPredictability = max(Predictability_code),
            min_NetPrimaryProductionPredictability = min(Predictability_code),
            mean_NPP = mean(NPP, na.rm = T),  
            mean_pol_complex = mean(pol_complex_code_Hedvig, na.rm = T), 
            mean_EA032 = mean(EA032_code, na.rm = T),
            sum_area = sum(sum_area, na.rm = T), 
            sum_water_area = sum(sum_water_area, na.rm = T), 
            Melanesia_or_not = dplyr::first(Melanesia_or_not),
            lg_count_smallest = mean(lg_count_smallest, na.rm = T),
            sum_shoreline = sum(sum_shoreline, na.rm = T),
            mean_lat = mean(mean_lat),
            mean_long = mean(mean_long),
            settlement_date_grouping_finer = max(settlement_date_grouping_finer), 
            settlement_date_grouping_coarser = max(settlement_date_grouping_coarser),
            oldest_date = max(oldest_date),
            color = dplyr::first(smallest_island_color), 
            lg_count = dplyr::first(lg_count_smallest),
            mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
            mean_CCSM_piControl_1760_bio2 = mean(mean_CCSM_piControl_1760_bio2),
            mean_CCSM_piControl_1760_bio3 = mean(mean_CCSM_piControl_1760_bio3),
            mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
            mean_CCSM_piControl_1760_bio5 = mean(mean_CCSM_piControl_1760_bio5),
            mean_CCSM_piControl_1760_bio6 = mean(mean_CCSM_piControl_1760_bio6),
            mean_CCSM_piControl_1760_bio7 = mean(mean_CCSM_piControl_1760_bio7),
            mean_CCSM_piControl_1760_bio8 = mean(mean_CCSM_piControl_1760_bio8),
            mean_CCSM_piControl_1760_bio9 = mean(mean_CCSM_piControl_1760_bio9),
            mean_CCSM_piControl_1760_bio10 = mean(mean_CCSM_piControl_1760_bio10),
            mean_CCSM_piControl_1760_bio11 = mean(mean_CCSM_piControl_1760_bio11),
            mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
            mean_CCSM_piControl_1760_bio13 = mean(mean_CCSM_piControl_1760_bio13),
            mean_CCSM_piControl_1760_bio14 = mean(mean_CCSM_piControl_1760_bio14),
            mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
            mean_CCSM_piControl_1760_bio16 = mean(mean_CCSM_piControl_1760_bio16),
            mean_CCSM_piControl_1760_bio17 = mean(mean_CCSM_piControl_1760_bio17),
            mean_CCSM_piControl_1760_bio18 = mean(mean_CCSM_piControl_1760_bio18),
            mean_CCSM_piControl_1760_bio19 = mean(mean_CCSM_piControl_1760_bio19)) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area)

write_tsv(Island_group_summarised_smallest, "output/sheets/RO_Hedvig_aggregate_smallest_island.tsv")

Island_group_summarised_smallest$Smallest_Island_group <- fct_reorder(Island_group_summarised_smallest$Smallest_Island_group, Island_group_summarised_smallest$lg_count)

Island_group_summarised_smallest %>% 
  ggplot() +
  geom_bar(aes(x = Smallest_Island_group, y = lg_count, fill = lg_count), stat = "Identity") +
  theme_classic() +
  theme(axis.text.x = element_blank(), legend.position = "None")

ggsave("output/plots/Lg_distrubition_smallest_island_group.png", width = 20, height = 5)

#impute smallest_island_group
# Island_group_summarised_smallest_imputed <- Island_group_summarised_smallest %>% 
#   dplyr::select(-Smallest_Island_group, -Melanesia_or_not, -color) %>% 
#   as.matrix() %>%
#   data.frame() %>%
#   missForest()
# 
# Island_group_summarised_smallest_imputed_df <- Island_group_summarised_smallest_imputed$ximp
# 
# Island_group_summarised_smallest_imputed_df$Smallest_Island_group <- Island_group_summarised_smallest$Smallest_Island_group
# 
# Island_group_summarised_smallest_imputed_df$Melanesia_or_not <- Island_group_summarised_smallest$Melanesia_or_not
# 
# Island_group_summarised_smallest_imputed_df %>%
#   dplyr::select(Smallest_Island_group, mean_pol_complex, everything()) %>% 
#   write_tsv("output/sheets/RO_Hedvig_aggregate_smallest_island_imputed.tsv")
# 
#medium_group
Island_group_summarised_medium <- Island_group_all_sep %>%
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  summarise(mean_NetPrimaryProductionPredictability = mean(Predictability_code, na.rm = T), 
            max_NetPrimaryProductionPredictability = max(Predictability_code),
            min_NetPrimaryProductionPredictability = min(Predictability_code),
            mean_NPP = mean(NPP, na.rm = T),  
            mean_pol_complex = mean(pol_complex_code_Hedvig, na.rm = T), 
            mean_EA032 = mean(EA032_code, na.rm = T),
            sum_area = sum(sum_area, na.rm = T), 
            sum_water_area = sum(sum_water_area, na.rm = T),
            Melanesia_or_not = dplyr::first(Melanesia_or_not),
            sum_shoreline = sum(sum_shoreline, na.rm = T),
            mean_lat = mean(mean_lat),
            mean_long = mean(mean_long),
            settlement_date_grouping_finer = max(settlement_date_grouping_finer), 
            settlement_date_grouping_coarser = max(settlement_date_grouping_coarser),
            oldest_date = max(oldest_date),
            color = dplyr::first(medium_group_color), 
            lg_count = dplyr::first(lg_count_medium),
            mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
            mean_CCSM_piControl_1760_bio2 = mean(mean_CCSM_piControl_1760_bio2),
            mean_CCSM_piControl_1760_bio3 = mean(mean_CCSM_piControl_1760_bio3),
            mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
            mean_CCSM_piControl_1760_bio5 = mean(mean_CCSM_piControl_1760_bio5),
            mean_CCSM_piControl_1760_bio6 = mean(mean_CCSM_piControl_1760_bio6),
            mean_CCSM_piControl_1760_bio7 = mean(mean_CCSM_piControl_1760_bio7),
            mean_CCSM_piControl_1760_bio8 = mean(mean_CCSM_piControl_1760_bio8),
            mean_CCSM_piControl_1760_bio9 = mean(mean_CCSM_piControl_1760_bio9),
            mean_CCSM_piControl_1760_bio10 = mean(mean_CCSM_piControl_1760_bio10),
            mean_CCSM_piControl_1760_bio11 = mean(mean_CCSM_piControl_1760_bio11),
            mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
            mean_CCSM_piControl_1760_bio13 = mean(mean_CCSM_piControl_1760_bio13),
            mean_CCSM_piControl_1760_bio14 = mean(mean_CCSM_piControl_1760_bio14),
            mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
            mean_CCSM_piControl_1760_bio16 = mean(mean_CCSM_piControl_1760_bio16),
            mean_CCSM_piControl_1760_bio17 = mean(mean_CCSM_piControl_1760_bio17),
            mean_CCSM_piControl_1760_bio18 = mean(mean_CCSM_piControl_1760_bio18),
            mean_CCSM_piControl_1760_bio19 = mean(mean_CCSM_piControl_1760_bio19)) %>% 
  left_join(isolation_medium) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area)

write_tsv(Island_group_summarised_medium, "output/sheets/RO_Hedvig_aggregate_medium_island.tsv")

Island_group_summarised_medium$Medium_only_merged_for_shared_language <- fct_reorder(Island_group_summarised_medium$Medium_only_merged_for_shared_language, Island_group_summarised_medium$lg_count)

Island_group_summarised_medium%>% 
  rename(`Language count` = lg_count) %>% 
  rename(`Island group (merged for language)` = Medium_only_merged_for_shared_language) %>% 
  ggplot() +
  geom_bar(aes(x = `Island group (merged for language)`, y = `Language count`), stat = "Identity", fill = Island_group_summarised_medium$color) +
  theme_classic() +
  theme(axis.text.x = element_blank(), legend.position = "None")
#  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "None")

ggsave("output/plots/Lg_distrubition_medium_island_group_lg_merged.png", width = 10, height = 5)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/Lg_distrubition_medium_island_group_lg_merged.png", width = 10, height = 5)

# small_to_medium <- polygon_geo_grouping_hierarchy %>% 
#   dplyr::select(Smallest_Island_group, "Medium_only_merged_for_shared_language", lg_count_medium)
# 
# Island_group_summarised_medium_imputed <- Island_group_summarised_smallest_imputed_df %>%
#   left_join(small_to_medium) %>%
#   group_by(Medium_only_merged_for_shared_language) %>% 
#     summarise(mean_NetPrimaryProductionPredictability = mean(mean_NetPrimaryProductionPredictability, na.rm = T), 
#             max_NetPrimaryProductionPredictability = max(max_NetPrimaryProductionPredictability),
#             min_NetPrimaryProductionPredictability = min(min_NetPrimaryProductionPredictability),
#             mean_NPP = mean(mean_NPP, na.rm = T),  
#             mean_pol_complex = mean(mean_pol_complex, na.rm = T), 
#             mean_EA032 = mean(mean_EA032, na.rm = T),
#             sum_area = sum(sum_area, na.rm = T), 
#             sum_water_area = sum(sum_water_area, na.rm = T),
#             Melanesia_or_not = dplyr::first(Melanesia_or_not),
#             sum_shoreline = sum(sum_shoreline, na.rm = T),
#             mean_lat = mean(mean_lat),
#             mean_long = mean(mean_long),
#             settlement_date_grouping_finer = max(settlement_date_grouping_finer),
#             lg_count = dplyr::first(lg_count_medium),
#             mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
#             mean_CCSM_piControl_1760_bio2 = mean(mean_CCSM_piControl_1760_bio2),
#             mean_CCSM_piControl_1760_bio3 = mean(mean_CCSM_piControl_1760_bio3),
#             mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
#             mean_CCSM_piControl_1760_bio5 = mean(mean_CCSM_piControl_1760_bio5),
#             mean_CCSM_piControl_1760_bio6 = mean(mean_CCSM_piControl_1760_bio6),
#             mean_CCSM_piControl_1760_bio7 = mean(mean_CCSM_piControl_1760_bio7),
#             mean_CCSM_piControl_1760_bio8 = mean(mean_CCSM_piControl_1760_bio8),
#             mean_CCSM_piControl_1760_bio9 = mean(mean_CCSM_piControl_1760_bio9),
#             mean_CCSM_piControl_1760_bio10 = mean(mean_CCSM_piControl_1760_bio10),
#             mean_CCSM_piControl_1760_bio11 = mean(mean_CCSM_piControl_1760_bio11),
#             mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
#             mean_CCSM_piControl_1760_bio13 = mean(mean_CCSM_piControl_1760_bio13),
#             mean_CCSM_piControl_1760_bio14 = mean(mean_CCSM_piControl_1760_bio14),
#             mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
#             mean_CCSM_piControl_1760_bio16 = mean(mean_CCSM_piControl_1760_bio16),
#             mean_CCSM_piControl_1760_bio17 = mean(mean_CCSM_piControl_1760_bio17),
#             mean_CCSM_piControl_1760_bio18 = mean(mean_CCSM_piControl_1760_bio18),
#             mean_CCSM_piControl_1760_bio19 = mean(mean_CCSM_piControl_1760_bio19))
# 
# Island_group_summarised_medium_imputed %>%
#   dplyr::select(Medium_only_merged_for_shared_language, mean_pol_complex, everything()) %>% 
#   write_tsv("output/sheets/RO_Hedvig_aggregate_medium_island_imputed.tsv")



#Marck
Island_group_summarised_Marck_group <- Island_group_all_sep %>% 
  filter(!is.na(settlement_date_grouping_finer)) %>% 
  group_by(Marck_group) %>% 
  summarise(mean_NetPrimaryProductionPredictability = mean(Predictability_code, na.rm = T), 
            max_NetPrimaryProductionPredictability = max(Predictability_code),
            min_NetPrimaryProductionPredictability = min(Predictability_code),
            mean_NPP = mean(NPP, na.rm = T),  
            mean_pol_complex = mean(pol_complex_code_Hedvig, na.rm = T), 
            mean_EA032 = mean(EA032_code, na.rm = T),
            sum_area = sum(sum_area, na.rm = T), 
            sum_water_area = sum(sum_water_area, na.rm = T), 
            Melanesia_or_not = dplyr::first(Melanesia_or_not),
            lg_count_smallest = mean(lg_count_smallest, na.rm = T),
            sum_shoreline = sum(sum_shoreline, na.rm = T),
            mean_lat = mean(mean_lat),
            mean_long = mean(mean_long),
            settlement_date_grouping_finer = max(settlement_date_grouping_finer),
            settlement_date_grouping_coarser = max(settlement_date_grouping_coarser),
            oldest_date = max(oldest_date),
            color = dplyr::first(Marck_group_color), 
            lg_count = dplyr::first(lg_count_Marck),
            mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
            mean_CCSM_piControl_1760_bio2 = mean(mean_CCSM_piControl_1760_bio2),
            mean_CCSM_piControl_1760_bio3 = mean(mean_CCSM_piControl_1760_bio3),
            mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
            mean_CCSM_piControl_1760_bio5 = mean(mean_CCSM_piControl_1760_bio5),
            mean_CCSM_piControl_1760_bio6 = mean(mean_CCSM_piControl_1760_bio6),
            mean_CCSM_piControl_1760_bio7 = mean(mean_CCSM_piControl_1760_bio7),
            mean_CCSM_piControl_1760_bio8 = mean(mean_CCSM_piControl_1760_bio8),
            mean_CCSM_piControl_1760_bio9 = mean(mean_CCSM_piControl_1760_bio9),
            mean_CCSM_piControl_1760_bio10 = mean(mean_CCSM_piControl_1760_bio10),
            mean_CCSM_piControl_1760_bio11 = mean(mean_CCSM_piControl_1760_bio11),
            mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
            mean_CCSM_piControl_1760_bio13 = mean(mean_CCSM_piControl_1760_bio13),
            mean_CCSM_piControl_1760_bio14 = mean(mean_CCSM_piControl_1760_bio14),
            mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
            mean_CCSM_piControl_1760_bio16 = mean(mean_CCSM_piControl_1760_bio16),
            mean_CCSM_piControl_1760_bio17 = mean(mean_CCSM_piControl_1760_bio17),
            mean_CCSM_piControl_1760_bio18 = mean(mean_CCSM_piControl_1760_bio18),
            mean_CCSM_piControl_1760_bio19 = mean(mean_CCSM_piControl_1760_bio19)) %>% 
  left_join(isolation_marck) %>% 
  mutate(ratio_coastline_to_area = sum_shoreline / sum_area)


write_tsv(Island_group_summarised_Marck_group, "output/sheets/RO_Hedvig_aggregate_marck_group.tsv")

Island_group_summarised_Marck_group$Marck_group <- fct_reorder(Island_group_summarised_Marck_group$Marck_group, Island_group_summarised_Marck_group$lg_count)

Island_group_summarised_Marck_group %>% 
  rename(`Language count` = lg_count) %>% 
  rename(`Marck group` = Marck_group) %>% 
  ggplot() +
  geom_bar(aes(x = `Marck group`, y = `Language count`), stat = "Identity", fill = Island_group_summarised_Marck_group$color) +
  theme_classic() +
  theme(axis.text.x = element_blank(), legend.position = "None")
#  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "None")

ggsave("output/plots/Lg_distrubition_Marck_group.png", width = 10, height = 5)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/Lg_distrubition_Marck_group.png", width = 10, height = 5)

# 
# small_to_Marck <- polygon_geo_grouping_hierarchy %>% 
#   dplyr::select(Smallest_Island_group, Marck_group, lg_count_Marck)
# 
# 
# Island_group_summarised_Marck_imputed <- Island_group_summarised_smallest_imputed_df %>%
#   left_join(small_to_Marck) %>%
#   group_by(Marck_group) %>% 
#   summarise(mean_NetPrimaryProductionPredictability = mean(mean_NetPrimaryProductionPredictability, na.rm = T), 
#             max_NetPrimaryProductionPredictability = max(max_NetPrimaryProductionPredictability),
#             min_NetPrimaryProductionPredictability = min(min_NetPrimaryProductionPredictability),
#             mean_NPP = mean(mean_NPP, na.rm = T),  
#             mean_pol_complex = mean(mean_pol_complex, na.rm = T), 
#             mean_EA032 = mean(mean_EA032, na.rm = T),
#             sum_area = sum(sum_area, na.rm = T), 
#             sum_water_area = sum(sum_water_area, na.rm = T),
#             Melanesia_or_not = dplyr::first(Melanesia_or_not),
#             sum_shoreline = sum(sum_shoreline, na.rm = T),
#             mean_lat = mean(mean_lat),
#             mean_long = mean(mean_long),
#             settlement_date_grouping_finer = max(settlement_date_grouping_finer),
#             lg_count = dplyr::first(lg_count_Marck),
#             mean_CCSM_piControl_1760_bio1 = mean(mean_CCSM_piControl_1760_bio1),
#             mean_CCSM_piControl_1760_bio2 = mean(mean_CCSM_piControl_1760_bio2),
#             mean_CCSM_piControl_1760_bio3 = mean(mean_CCSM_piControl_1760_bio3),
#             mean_CCSM_piControl_1760_bio4 = mean(mean_CCSM_piControl_1760_bio4),
#             mean_CCSM_piControl_1760_bio5 = mean(mean_CCSM_piControl_1760_bio5),
#             mean_CCSM_piControl_1760_bio6 = mean(mean_CCSM_piControl_1760_bio6),
#             mean_CCSM_piControl_1760_bio7 = mean(mean_CCSM_piControl_1760_bio7),
#             mean_CCSM_piControl_1760_bio8 = mean(mean_CCSM_piControl_1760_bio8),
#             mean_CCSM_piControl_1760_bio9 = mean(mean_CCSM_piControl_1760_bio9),
#             mean_CCSM_piControl_1760_bio10 = mean(mean_CCSM_piControl_1760_bio10),
#             mean_CCSM_piControl_1760_bio11 = mean(mean_CCSM_piControl_1760_bio11),
#             mean_CCSM_piControl_1760_bio12 = mean(mean_CCSM_piControl_1760_bio12),
#             mean_CCSM_piControl_1760_bio13 = mean(mean_CCSM_piControl_1760_bio13),
#             mean_CCSM_piControl_1760_bio14 = mean(mean_CCSM_piControl_1760_bio14),
#             mean_CCSM_piControl_1760_bio15 = mean(mean_CCSM_piControl_1760_bio15),
#             mean_CCSM_piControl_1760_bio16 = mean(mean_CCSM_piControl_1760_bio16),
#             mean_CCSM_piControl_1760_bio17 = mean(mean_CCSM_piControl_1760_bio17),
#             mean_CCSM_piControl_1760_bio18 = mean(mean_CCSM_piControl_1760_bio18),
#             mean_CCSM_piControl_1760_bio19 = mean(mean_CCSM_piControl_1760_bio19))
# 
# Island_group_summarised_Marck_imputed %>%
#   dplyr::select(Marck_group, mean_pol_complex, everything()) %>%
#   write_tsv("output/sheets/RO_Hedvig_aggregate_Marck_island_imputed.tsv")


###VENN
# 
# Island_group_summarised_all_venn <- Island_group_summarised_all %>% 
#   mutate(mean_pol_complex = replace(mean_pol_complex, !is.na(mean_pol_complex), T)) %>% 
#   mutate(mean_pol_complex = replace(mean_pol_complex, is.na(mean_pol_complex), F)) %>% 
#   mutate(mean_NPP = replace(mean_NPP, !is.na(mean_NPP), T)) %>% 
#   mutate(mean_NPP = replace(mean_NPP, is.na(mean_NPP), F)) %>% 
#   mutate(mean_tephra = replace(mean_tephra, !is.nan(mean_tephra), T)) %>% 
#   mutate(mean_tephra = replace(mean_tephra, is.na(mean_tephra), F)) %>% 
#   mutate(mean_makatea = replace(mean_makatea, !is.nan(mean_makatea), T)) %>% 
#   mutate(mean_makatea = replace(mean_makatea, mean_makatea == "NaN", F)) %>% 
#   mutate(mean_dust = replace(mean_dust, !is.nan(mean_dust), T)) %>% 
#   mutate(mean_dust = replace(mean_dust, mean_dust == "NaN", F)) %>% 
#   mutate(mean_rainfall = replace(mean_rainfall, !is.na(mean_rainfall), T)) %>% 
#   mutate(mean_rainfall = replace(mean_rainfall, is.na(mean_rainfall), F)) %>% 
#   mutate(settlement_date_grouping = replace(settlement_date_grouping, !is.na(settlement_date_grouping), T)) %>% 
#   mutate(settlement_date_grouping = replace(settlement_date_grouping, is.na(settlement_date_grouping), F)) %>%   
#   mutate(mean_Isolation = replace(mean_Isolation, !is.nan(mean_Isolation), T)) %>% 
#   mutate(mean_Isolation = replace(mean_Isolation, mean_Isolation == "NaN", F)) %>% 
#   mutate(mean_elevation = replace(mean_elevation, !is.nan(mean_elevation), T)) %>% 
#   mutate(mean_elevation = replace(mean_elevation, mean_elevation == "NaN", F))   
# 
# Island_group_summarised_all_venn %>% 
#    dplyr::select(mean_pol_complex, mean_NPP, settlement_date_grouping) %>% 
#    venn() 
