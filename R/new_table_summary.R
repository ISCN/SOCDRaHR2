
profile_lat_summary <- ISCN3.ls$profile %>%
  select(dataset_name_sub, 'lat (dec. deg)') %>%
  unique()%>%
  group_by(dataset_name_sub) %>% 
  tally(name = 'profile_lat_count')

profile_long_summary <- ISCN3.ls$profile %>%
  select(dataset_name_sub, 'long (dec. deg)') %>%
  unique()%>%
  group_by(dataset_name_sub) %>% 
  tally(name = 'profile_long_count')

layer_lat_summary <- ISCN3.ls$layer %>%
  select(dataset_name_sub, 'lat (dec. deg)') %>%
  unique()%>%
  group_by(dataset_name_sub) %>% 
  tally(name = 'layer_lat_count')

layer_long_summary <- ISCN3.ls$layer %>%
  select(dataset_name_sub, 'long (dec. deg)') %>%
  unique()%>%
  group_by(dataset_name_sub) %>% 
  tally(name = 'layer_long_count')

table_summary <- ISCN3.ls$study %>% 
  select(dataset_name) %>%
  unique() %>%
  left_join(layer_summary, by=c('dataset_name' = 'dataset_name_sub'))%>%
  left_join(profile_summary, by=c('dataset_name' = 'dataset_name_sub'))%>%
  left_join(profile_lat_summary, by=c('dataset_name' = 'dataset_name_sub'))%>%
  left_join(profile_long_summary, by=c('dataset_name' = 'dataset_name_sub'))%>%
  left_join(layer_lat_summary, by=c('dataset_name' = 'dataset_name_sub'))%>%
  left_join(layer_long_summary, by=c('dataset_name' = 'dataset_name_sub'))


temp <- ISCN3.ls$layer %>%
  select(dataset_name_sub, `lat (dec. deg)`, `long (dec. deg)`) %>%
  unique()%>%
  filter(!is.na(`lat (dec. deg)`) && !is.na(`long (dec. deg)`)) %>%
  group_by(dataset_name_sub)%>%
  tally(name = 'Lat/Long count')

temp2 <- ISCN3.ls$layer %>%
  select(dataset_name_sub, site_name, `lat (dec. deg)`, `long (dec. deg)`) %>%
  unique()%>%
  #filter(is.na(`lat (dec. deg)`) && is.na(`long (dec. deg)`))
  mutate(has_lat_long = is.finite(`lat (dec. deg)` + `long (dec. deg)`)) %>%
  group_by(dataset_name_sub, has_lat_long) %>%
  tally()%>%
  mutate(my_label = if_else(has_lat_long, "geolocated_layer", "unlocated_layer"))%>%
  select(-has_lat_long)%>%
  pivot_wider(names_from = my_label, values_from = n)
  
  
temp1 <- ISCN3.ls$profile %>%
  select(dataset_name_profile = dataset_name_sub) %>%
  unique() %>%
  mutate(dataset_name = dataset_name_profile) %>%
  full_join(ISCN3.ls$study %>%
              select(dataset_name) %>%
              unique()) %>%
  full_join(ISCN3.ls$layer %>%
              select(dataset_name_layer = dataset_name_sub) %>%
              unique() %>%
              mutate(dataset_name = dataset_name_layer)) %>%
  select(dataset_name, dataset_name_profile, dataset_name_layer)
