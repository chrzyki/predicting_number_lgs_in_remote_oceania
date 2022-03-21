source("01_requirements.R")

data <- read_tsv("output/sheets/RO_Hedvig_aggregate_marck_group.tsv") %>% 
  dplyr::filter(!is.na(Marck_group)) %>% 
  dplyr::select(group = Marck_group, everything()) %>% 
  dplyr::mutate(Latitude_abs_mean = abs(mean_lat)) %>% 
  filter(!is.na(mean_pol_complex))   %>% 
  rename(Annual_precipitation_mean = mean_CCSM_piControl_1760_bio12) %>% 
  rename(Precipitation_seasonality_mean = mean_CCSM_piControl_1760_bio15) %>% 
  rename(Annual_temperature_mean = mean_CCSM_piControl_1760_bio1) %>% 
  rename(Temperature_seasonality_mean = mean_CCSM_piControl_1760_bio4) %>% 
  rename(EA033 = mean_pol_complex) %>% 
  rename(Area_incl_water = sum_water_area) %>% 
  rename(Area_land = sum_area) %>% 
  rename(Shoreline = sum_shoreline) %>% 
  rename(Settlement_order_oldest = settlement_date_grouping_finer) %>% 
  rename(Net_Primary_Production_mean = mean_NPP) %>% 
  rename(Net_Primary_Production_Predictability_mean = mean_NetPrimaryProductionPredictability) %>% 
  rename(Isolation = dist) %>% 
  mutate(group = str_replace_all(group, "Kanaky", "New Caledonia (incl loyalties)")) %>% 
  mutate(group = str_replace_all(group, "and", "+")) %>% 
  arrange(-lg_count) %>% 
  dplyr::select(group, lg_count, Latitude_abs_mean, Annual_precipitation_mean, Precipitation_seasonality_mean, Annual_temperature_mean, Temperature_seasonality_mean, EA033, Area_land, Shoreline, Isolation, Settlement_order_oldest, ratio_coastline_to_area)

##full model

options(na.action = "na.fail")
##full model
test <- dredge(global.model =  glm.nb(data = data, lg_count  ~   Latitude_abs_mean + 
                                     Annual_precipitation_mean * Precipitation_seasonality_mean +
                                     Annual_temperature_mean * Temperature_seasonality_mean +
                                     EA033 + 
                                     Isolation + 
                                     #                       Net_Primary_Production_mean +
                                     #                       Net_Primary_Production_Predictability_mean +
                                     #                       Area_incl_water * Settlement_order_oldest +
                                     Shoreline * Settlement_order_oldest +
                                     Area_land * Settlement_order_oldest  +
                                   ratio_coastline_to_area* Settlement_order_oldest   ,  
                                   control = list(maxit = 2000, epsilon = 1e-7, trace = 3)))
               

write_tsv(test, "dfmumin_marck_groups_wo_ratio.tsv")


df_wo_ratio <- read_tsv("dfmumin_marck_groups_wo_ratio.tsv")

df_wo_ratio$weight <- round(df_wo_ratio$weight, 4)


DF_1_t <- df_wo_ratio[1,1:15] %>% 
  t()

DF_1_t[,1] <- round(DF_1_t[,1], 5)
