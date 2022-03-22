source("01_requirements.R")

Polygon_lgs_glottocodes_sep <- read_csv("data/RO_polygons_grouped_with_languages.csv") %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) 

#read in glottolog
glottolog_lat_long_shifted <- read_tsv("data/glottolog_language_table_wide_df.tsv")  %>% 
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

Marck_groups <- read_tsv("output/sheets/RO_Hedvig_aggregate_marck_group.tsv") %>% 
  dplyr::select(Marck_group, color) %>% 
  full_join(Polygon_lgs_glottocodes_sep)


basemap + 
  geom_point(data = Marck_groups, size = 1.5, aes(x=Longitude, y=Latitude),
              shape = 19, stroke = 0, color = Marck_groups$color, inherit.aes = F) +
  coord_map(xlim = c(120, 252)) +
  theme(legend.position = "None") 

ggsave("output/plots/polygon_Marck_group_map.png", width = 15, height = 8)



#per medium group

medium_groups <- read_tsv("output/sheets/RO_Hedvig_aggregate_medium_island.tsv") %>% 
  dplyr::select(Medium_only_merged_for_shared_language, color) %>% 
  full_join(Polygon_lgs_glottocodes_sep)

medium_groups_labels <- read_tsv("output/sheets/RO_Hedvig_aggregate_medium_island.tsv") %>% 
  mutate(mean_long = if_else(mean_long <= -25, mean_long + 360, mean_long)) %>% 
  group_by(Medium_only_merged_for_shared_language) %>% 
  summarise(mean_long = mean(mean_long),
            mean_lat = mean(mean_lat))

basemap + 
  geom_jitter(data = medium_groups, size = 1.5, aes(x=Longitude, y=Latitude),
              shape = 19, stroke = 0, color = medium_groups$color) +
  coord_map(xlim = c(120, 252)) +
  theme(legend.position = "None") #+
#  geom_label(data = medium_groups_labels, aes(x = mean_long, y = mean_lat, label = Medium_only_merged_for_shared_language), size = 0.6)

ggsave("output/plots/polygon_medium_group_map.png", width = 15, height = 8)




# 
# #Per glottocode
# Polygon_lgs_textlabel <- Polygon_lgs_glottocodes_sep %>% 
#   group_by(glottocode) %>% 
#   summarise(mean_lat = mean(Latitude), mean_long = mean(Longitude))
# 
# Polygon_lgs_check <- basemap + 
#   geom_jitter(data = Polygon_lgs_glottocodes_sep, size = 1.5, aes(x=Longitude, y=Latitude, color=glottocode),
#               shape = 19, alpha = 0.6, stroke = 0) +
#   coord_map(xlim = c(120, 252)) +
#   theme(legend.position = "None") +
#   geom_label_repel(data = Polygon_lgs_textlabel, aes(x = mean_long, y = mean_lat, label = glottocode))
# 
# plot(Polygon_lgs_check)
# 
# ggsave("output/plots/polygon_lgs_mapping_check.pdf", width = 30, height = 30)
# 
# #Not Vanuatu or New Caledonia
# Polygon_lgs_glottocodes_sep_not_NC_Vanuatu <- Polygon_lgs_glottocodes_sep %>% 
#   filter(Marck_group != "Vanuatu (greater)") %>% 
#   filter(Marck_group != "Kanaky")
# 
# Polygon_lgs_textlabel_not_NC_Vanuatu <- Polygon_lgs_glottocodes_sep_not_NC_Vanuatu %>% 
#   group_by(glottocode) %>% 
#   summarise(mean_lat = mean(Latitude), mean_long = mean(Longitude))
# 
# 
# Polygon_lgs_check_not_vanuatu_NC <- basemap + 
#   geom_jitter(data = Polygon_lgs_glottocodes_sep_not_NC_Vanuatu, size = 1.5, aes(x=Longitude, y=Latitude, color=glottocode),
#               shape = 19, alpha = 0.6, stroke = 0) +
#   coord_map(xlim = c(120, 252)) +
#   theme(legend.position = "None") +
#   geom_label_repel(data = Polygon_lgs_textlabel_not_NC_Vanuatu, aes(x = mean_long, y = mean_lat, label = glottocode))
# 
# #Vanuatu or New Caledonia
# Polygon_lgs_glottocodes_sep_NC_Vanuatu <- Polygon_lgs_glottocodes_sep %>% 
#   filter(Marck_group == "Vanuatu (greater)"|Marck_group == "Kanaky")
# 
# Polygon_lgs_textlabel_NC_Vanuatu <- Polygon_lgs_glottocodes_sep_NC_Vanuatu %>% 
#   group_by(glottocode) %>% 
#   summarise(mean_lat = mean(Latitude), mean_long = mean(Longitude))
# 
# Polygon_lgs_check_vanuatu_NC <- basemap + 
#   geom_jitter(data = Polygon_lgs_glottocodes_sep_NC_Vanuatu, size = 1.5, aes(x=Longitude, y=Latitude, color=glottocode),
#               shape = 19, alpha = 0.6, stroke = 0) +
#   coord_map(xlim = c(160, 180)) +
#   theme(legend.position = "None") +
#   geom_label_repel(data = Polygon_lgs_textlabel_NC_Vanuatu, aes(x = mean_long, y = mean_lat, label = glottocode))
# 
# plot(Polygon_lgs_check_vanuatu_NC)
# 
# ggsave("output/plots/polygon_lgs_mapping_check_vanuatu_NC.pdf", width = 30, height = 30)
# 
# plot(Polygon_lgs_check_not_vanuatu_NC)
# 
# ggsave("output/plots/polygon_lgs_mapping_check_not_vanuatu_NC.pdf", width = 30, height = 30)


