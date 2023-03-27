source("01_requirements.R")

Polygon_lgs_glottocodes_sep <- read_csv("data/RO_polygons_grouped_with_languages.csv", show_col_types = F) %>% 
  dplyr::select(Marck_group, Medium_only_merged_for_shared_language, Longitude, Latitude) %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))


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
  full_join(Polygon_lgs_glottocodes_sep, by = "Marck_group") %>% 
  filter(!is.na(lg_count)) %>% 
  filter(!is.na(Marck_group))

color_vec_marck <- unique(Marck_groups$color)

#ggalt::geom_encircle cannot handle groups with 1 or 3 members. So for those island groups, we're adding rows with points that are super-near
groups_two_few_points <- Marck_groups %>% 
  group_by(Marck_group) %>% 
  summarise(n = n()) %>% 
  filter(n < 3)

Marck_groups_too_few_points_df<- Marck_groups %>% 
  filter(Marck_group %in% groups_two_few_points$Marck_group)

Marck_groups_too_few_points_df_minus <- Marck_groups %>% 
  mutate(Longitude = Longitude - 0.05, 
         Latitude = Latitude - 0.05)

Marck_groups_too_few_points_df_plus <- Marck_groups %>% 
  mutate(Longitude = Longitude + 0.05, 
         Latitude = Latitude + 0.05)

Marck_groups_for_encircle_plotting_df <- Marck_groups %>% 
  rbind(Marck_groups_too_few_points_df_minus) %>% 
  rbind(Marck_groups_too_few_points_df_plus)

#label df with lg_count
Marck_groups_for_encircle_plotting_df_labels <- Marck_groups_for_encircle_plotting_df %>% 
  group_by(Marck_group, lg_count) %>% 
  summarise(mean_lat = mean(Latitude, na.rm = T),
            mean_long = mean(Longitude, na.rm = T),
            .groups = "drop")

basemap + 
  geom_encircle(data = Marck_groups_for_encircle_plotting_df, mapping = aes(x=Longitude, y=Latitude, color = Marck_group), size = 3,
                expand = 0.005, 
                s_shape = 1)  +
  geom_label(data = Marck_groups_for_encircle_plotting_df_labels, mapping =aes(x = mean_long, 
                                                                               y = mean_lat,
                                                                               label = lg_count, 
                                                                               size = 0.01, 
                                                                               color = Marck_group),
             label.padding = unit(0.2, "lines"))+
  theme(legend.position = "None") +
  scale_color_manual(values = color_vec_marck)

ggsave("output/plots/polygon_Marck_group_map.png", width = 15, height = 8)

#per medium group

medium_groups <- read_tsv("output/processed_data/RO_Hedvig_aggregate_medium_island.tsv", show_col_types = F) %>% 
  dplyr::select(Medium_only_merged_for_shared_language, color) %>% 
  full_join(Polygon_lgs_glottocodes_sep, by = "Medium_only_merged_for_shared_language")


#ggsave("output/plots/polygon_medium_group_map.png", width = 15, height = 8)