source("requirements.R")

#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#read in
glottolog <- read_tsv("../Glottolog_look_up_table/Glottolog_lookup_table_Hedvig_output/Glottolog_lookup_table_Heti_edition.tsv")  %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) 


polygon_grouping_hierachy <- read_tsv("output/sheets/Polygon_hierarchy_stats.tsv")
  
#shifting the longlat of the dataframe to match the pacific centered map
All_polygons <- read_csv("data/Remote_Oceania_Political_complex_and_more/RO_polygons_grouped_with_languages.csv") %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  mutate(area = as.numeric(`AREA (sq km)`)) %>% 
  arrange(area) %>% 
  full_join(polygon_grouping_hierachy) %>% 
  filter(!is.na(glottocodes)) %>% 
  mutate(glottocodes = str_split(glottocodes, ",")) %>%
  unnest(glottocodes) %>% 
  mutate(glottocode = trimws(glottocodes)) %>% 
  distinct() 


#Basemap
basemap <- ggplot(All_polygons) +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", size = 0.3)  + 
  theme(legend.position="none",
        panel.grid.major = element_blank(), #all of these lines are just removing default things like grid lines, axises etc
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
#  coord_map(projection = "vandergrinten", xlim = c(130, 255), ylim = c(-56, 27)) +
  xlim(c(110, 255)) +
  ylim(c(-56, 27))



Map_plot_all_polygons <- basemap + 
  geom_point(aes(x=Longitude, y=Latitude), color = All_polygons$smallest_island_color, size = 0.5) 

plot(Map_plot_all_polygons)
ggsave("output/plots/maps/Map_RO_Smallest.png", width = 5, height = 4)


##Marck grouping
Map_plot_marck <- basemap + 
  geom_point(aes(x=Longitude, y=Latitude), color = All_polygons$Marck_group_color, size = 0.5) #+
#  scale_color_manual(values = color_vector_smallest)

plot(Map_plot_marck)
ggsave("output/plots/maps/Map_RO_Marck.png", width = 5, height = 4)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/polygon_marck_group_map.png", width = 5, height = 3)

##Medium_group
Map_plot_medium <- basemap + 
  geom_point(aes(x=Longitude, y=Latitude), color = All_polygons$medium_group_color, size = 0.5) #+
#  scale_color_manual(values = color_vector_smallest)

plot(Map_plot_medium)
ggsave("output/plots/maps/Map_RO_Medium.png", width = 5, height = 4)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/polygon_medium_group_map.png", width = 5, height =3)


#pol_complex
col_vector_3 <- c("#91bfdb", "#ffffbf", "#fc8d59", "#d7191c")

pol_complex_data <- read_csv("data/Remote_Oceania_Political_complex_and_more/Remote_oceania_pol_complex_hedvig_code.csv") %>% 
  filter(!is.na(pol_complex_code_Hedvig)) %>% 
  mutate(glottocode = Glottocode_spec) %>% 
  mutate(`Political complexity (EA033)` = as.character(pol_complex_code_Hedvig)) %>% 
  left_join(All_polygons) %>% 
    group_by(`Political complexity (EA033)`, glottocode) %>% 
    summarise(Latitude = mean(Latitude),
              Longitude = mean(Longitude)) 

#pol_complex_data_labels <- pol_complex_data %>% 
#  group_by(Marck_group, `Political complexity (EA033)`) %>% 
#  summarise(Latitude = mean(Latitude),
#            Longitude = mean(Longitude))

basemap + 
  geom_jitter(data = pol_complex_data, aes(x=Longitude, y=Latitude, fill = `Political complexity (EA033)`), size = 2, alpha = 0.8, shape = 21, stroke = 0.4, width = 0.5) +
  scale_fill_manual(values = col_vector_3) +
  theme(legend.position = "bottom") #+
#  geom_label_repel(data = pol_complex_data_labels, aes(x=Longitude, y=Latitude, label = `Political complexity (EA033)`, fill = `EA033 code`), size = 2, label.padding = unit(0.1, "lines"), position = "jitter") 
  
ggsave("output/plots/maps/map_pol_complex.png", width = 9, height = 6)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/map_pol_complex.png", width = 9, height = 6)

#dates
dates <- read_xlsx("data/Remote_Oceania_Political_complex_and_more/island_group_settlement_date.xlsx") %>% 
  rename(settlement_date_grouping_finer = "Time depth settlement group", Smallest_Island_group = `Smaller specific island group`, `Settlement date oldest date` = `Oldest date`) %>% 
  dplyr::select(Smallest_Island_group, settlement_date_grouping_finer, `Settlement date oldest date`) %>% 
  full_join(All_polygons) %>% 
  filter(!is.na(Smallest_Island_group)) %>% 
  filter(!is.na(settlement_date_grouping_finer))

dates_labels <- dates %>% 
  group_by(Marck_group, settlement_date_grouping_finer) %>% 
  summarise(Longitude = mean(Longitude), 
            Latitude = mean(Latitude))

basemap + 
  geom_point(data = dates, aes(x=Longitude, y=Latitude, colour = settlement_date_grouping_finer), size = 0.5, alpha = 0.8) +
  geom_label(data = dates_labels, aes(x= Longitude, 
                                      y= Latitude, 
                                      label = `settlement_date_grouping_finer`, 
                                      fill = settlement_date_grouping_finer), size = 2, label.padding = unit(0.1, "lines"), position = "jitter") +
  scale_fill_viridis() +
  scale_color_viridis() 


ggsave("output/plots/maps/Map_RO_dates.png", width = 9, height = 6)
ggsave("../../Hedvigs_academia/Hedvigs PhD thesis/tex/illustrations/plots_from_R/Map_RO_dates.png", width = 9, height = 6)


