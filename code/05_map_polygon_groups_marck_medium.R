source("01_requirements.R")

Polygon_lgs_glottocodes_sep <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) 

#read in glottolog
glottolog_lat_long_shifted <- read_tsv("data/glottolog_language_table_wide_df.tsv", show_col_types = F)  %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#Basemap
basemap <- ggplot(glottolog_lat_long_shifted) +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", size = 0.3)  + 
  theme(panel.grid.major = element_blank(), #all of these lines are just removing default things like grid lines, axises etc
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") +
  xlim(c(90, 252)) +
  ylim(c(-56, 25))

##Per Marck_group

Marck_groups <- read_tsv("output/processed_data/RO_Hedvig_aggregate_marck_group.tsv", show_col_types = F) %>% 
  dplyr::select(Marck_group, color, lg_count) %>% 
  full_join(Polygon_lgs_glottocodes_sep, by = "Marck_group")


basemap + 
  geom_point(data = Marck_groups, size = 1.5, aes(x=Longitude, y=Latitude, fill = log10(lg_count)),
              shape = 21, 
             color = Marck_groups$color, 
             inherit.aes = F) +
  theme(legend.position = "None") 

h_load("ggalt")

mean_var_long_lat_df <-  Marck_groups %>% 
  group_by(Marck_group) %>%
  summarise(var_latitude = var(Latitude, na.rm = T),
            var_longitude = var(Longitude, na.rm = T), 
            .groups = "drop") %>% 
  summarise(mean_var_latitude = mean(var_latitude, na.rm = T),
            mean_var_longitude = mean(var_longitude, na.rm = T))
  
two_few_points_df <- Marck_groups %>% 
  group_by(Marck_group) %>% 
  summarise(n = n()) %>% 
  filter(n < 3)


Marck_groups_subset <- Marck_groups %>% 
  filter(Marck_group == "Kosrae")


basemap + 
  geom_encircle(data = Marck_groups_subset, mapping = aes(x=Longitude, y=Latitude, color = Marck_group), 
                expand = 0, 
                s_shape = 0)  




  geom_point(data = Marck_groups_subset, size = 1.5, aes(x=Longitude, y=Latitude, fill = log10(lg_count)),
             shape = 21, 
          #   color = Marck_groups$color, 
             inherit.aes = F) +
  theme(legend.position = "None") 


ggsave("output/plots/polygon_Marck_group_map.png", width = 15, height = 8)

#per medium group

medium_groups <- read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_island.tsv", show_col_types = F) %>% 
  dplyr::select(Medium_only_merged_for_shared_language, color) %>% 
  full_join(Polygon_lgs_glottocodes_sep, by = "Medium_only_merged_for_shared_language")

medium_groups_labels <- read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_island.tsv", show_col_types = F) %>% 
  mutate(mean_long = if_else(mean_long <= -25, mean_long + 360, mean_long)) %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  summarise(mean_long = mean(mean_long),
            mean_lat = mean(mean_lat))

basemap + 
  geom_jitter(data = medium_groups, size = 1.5, aes(x=Longitude, y=Latitude),
              shape = 19, stroke = 0, color = medium_groups$color) +
  theme(legend.position = "None") 

ggsave("output/plots/polygon_medium_group_map.png", width = 15, height = 8)