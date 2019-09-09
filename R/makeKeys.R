makeKeys <- function(){
  ##data keys should have
  ##... table, header, variable, type, entry
  
  # ISCNtarget.key <- rbind(tibble::tibble(table = 'study',
  #                                  variable = c('dataset_name_l1', 'dataset_name_l0',
  #                                               'data_type', "curator_name", 
  #                                               "curator_organization", "curator_email",
  #                                               "modification_date", "reference", "citation",
  #                                               "citation_usage", "acknowledgement", 
  #                                               "acknowledgement_usage", "dataset_description",
  #                                               "contact_name", "contact_email" )),
  #                         tibble::tibble(table='site',
  #                                        variable = c('dataset_name_l1', 'dataset_name_l0', 'site_name', 'latitude', 'longitude', 'datum', 'geopolitical_unit_1', 'geopolitical_unit_2','vegitation')))
  # 
  ISCN5.target <- tibble::tribble(~variable, ~table,
                                  'dataset_name', 'study', #id
                                  'dataset_name', 'profile', #id
                                  'site_name', 'profile', #id
                                  'profile_name', 'profile', #id
                                  '2d_position', 'profile', #id
                                  'acknowledgement', 'study',
                                  'acknowledgement_usage', 'study',
                                  'aspect_deg', 'profile',
                                  'citation', 'study',
                                  'citation_usage', 'study',
                                  'contact_email', 'study',
                                  'contact_name', 'study',
                                  'country', 'profile',
                                  'curator_email', 'study',
                                  'curator_name', 'study',
                                  'curator_organization', 'study',
                                  'data_collection', 'study',
                                  'dataset_description', 'study',
                                  'dataset_type', 'study',
                                  'datum', 'profile',
                                  'drainagecl', 'profile',
                                  'ecoregion', 'profile',
                                  'elevation', 'profile',
                                  'landform', 'profile',
                                  'landform_note', 'profile',
                                  'landscape', 'profile',
                                  'landuse', 'profile',
                                  'latitude', 'profile',
                                  'profile_bottom', 'profile',
                                  'layer_carbon_count', 'profile',
                                  'layer_count', 'profile',
                                  'layer_ISCN_soc_count', 'profile',
                                  'layer_soc_count', 'profile',
                                  'profile_top', 'profile',
                                  'locator_alias', 'profile',
                                  'locator_parent_alias', 'profile',
                                  'longitude', 'profile',
                                  'map', 'profile',
                                  'mat', 'profile',
                                  'modification_date', 'study',
                                  'observation_date', 'profile',
                                  'profile_count', 'study',
                                  'profile_ISCN_soc_count', 'study', 
                                  'profile_soc', 'profile',
                                  'profile_soc_carbon_flag', 'profile',
                                  'profile_soc_count', 'study',
                                  'profile_soc_depth', 'profile',
                                  'profile_soc_spatial_flag', 'profile',
                                  'profile_zero_ref', 'profile',
                                  'reference', 'study',
                                  'runoff', 'profile',
                                  'site_count', 'study',
                                  'site_ISCN_soc_count', 'study',
                                  'site_note', 'profile',
                                  'site_perm', 'profile',
                                  'site_soc_count', 'study',
                                  'slope', 'profile',
                                  'soil_series', 'profile',
                                  'soil_taxon', 'profile',
                                  'state', 'profile',
                                  'surface_veg', 'profile',
                                  'thaw_depth_profile', 'profile',
                                  'vegclass_local',  'profile')
  
#   ####ISCN.key#####
#   ISCN.key <- dplyr::bind_rows(
#     tibble::tibble(table='study', header='data collection', variable = 'dataset_name_l1', type='id'),
#     tibble::tibble(table='study', header='dataset name', variable = 'dataset_name_l0', type='id'),
#     tibble::tibble(table = 'study', header = 'dataset type', variable = 'dataset_type', type='value_str'),
#     tibble::tibble(table = 'study', header = 'dataset description', variable = 'dataset_description', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'curator name', variable = 'curator_name', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'curator organization', variable = 'curator_organization', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'curator email', variable = 'curator_email', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'modification date (dec yr)', variable = 'modification_date', type = 'value_num'),
#     tibble::tibble(table = 'study', header = 'contact name', variable = 'contact_name', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'contact email', variable = 'contact_email', type= 'value_str'),
#     tibble::tibble(table = 'study', header = 'reference', variable = 'reference', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'citation', variable = 'citation', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'citation_usage', variable = 'citation_usage', type = 'value_str'),
#     tibble::tibble(table = 'study', header = 'acknowledgement', variable = 'acknowledgement', type = 'value_str'), 
#     tibble::tibble(table = 'study', header = 'acknowledgement_usage', variable = 'acknowledgement_usage', type = 'value_str'),
#     ##site data
#     tibble::tibble(table='site', header='data collection', variable = 'dataset_name_l1', type='id'),
#     tibble::tibble(table='site', header='dataset name', variable = 'dataset_name_l0', type='id'),
#     tibble::tibble(table='site', header='site', variable="site_name", type='id'),
#     tibble::tibble(table='site', header='latitude', variable="latitude", type='value_num'),
#     tibble::tibble(table='site', header='latitude', variable="longitude", type='value_num'),
#     tibble::tibble(table='site', header='datum', variable = 'datum', type='value_str'),
#     tibble::tibble(table='site', header='geopoltical 1', variable="geopolitical_unit_1", type='value_str'),
#     tibble::tibble(table='site', header='geopoltical 2', variable="geopolitical_unit_2", type='value_str'),
#     tibble::tibble(table='site', header='vegitation class', variable="vegitation", type='value_str'),
#     ##Profile
#     tibble::tibble(table='profile', header='data collection', variable = 'dataset_name_l1', type='id'),
#     tibble::tibble(table='profile', header='dataset', variable = 'dataset_name_l0', type='id'),
#     tibble::tibble(table='profile', header='site', variable="site_name", type='id'),
#     tibble::tibble(table='profile', header = 'profile', variable = 'profile_name', type = 'id'),
#     tibble::tibble(table='profile', header='observation date', variable="observation_date", type='value_num'),
#     tibble::tibble(table='profile', header = 'taxon', variable="soil_taxon"),
#     tibble::tibble(table='profile', header = 'order', variable="soil_order"),
#     ##Layer
#     tibble::tibble(table='layer', header='data collection', variable = 'dataset_name_l1', type='id'),
#     tibble::tibble(table='layer', header='dataset', variable = 'dataset_name_l0', type='id'),
#     tibble::tibble(table='layer', header='site', variable="site_name", type='id'),
#     tibble::tibble(table='layer', header ='profile', variable="profile_name", type='id'),
#     tibble::tibble(table='layer', header = 'layer', variable="layer_name", type='id'),
#     tibble::tibble(table='layer', header ='layer top', variable="layer_top", type='value_num'),
#     tibble::tibble(table='layer', header ='layer bottom', variable="layer_bottom", type='value_num'),
#     tibble::tibble(table='layer', header = 'horizon', variable="horizon_designation_other", type='value_str'),
#     tibble::tibble(table='layer', header ='horizon', variable="horizon", type='value_str'),
#     tibble::tibble(table='layer', header = 'horizon', variable="horizon_designation", type='value_str'),
#     tibble::tibble(table='layer', header = 'layer note', variable="layer_note", type='value_str'),
#     tibble::tibble(table='layer', header = 'color', variable="soil_color", type='value_str'),
#     tibble::tibble(table='layer', header = 'texture_class', variable = 'texture_class', type = 'value_str'),
#     ##Bulk density
#     tibble::tibble(table='bulk_density', header='dataset', variable = 'dataset', type = 'value_str'),
#     tibble::tibble(table='bulk_density', header ='site', variable="site_name", type = 'id'),
#     tibble::tibble(table='bulk_density', header ='profile', variable="profile_name", type = 'value_str'),
#     tibble::tibble(table='bulk_density', header = 'layer', variable="layer_name", type = 'value_str'),
#     tibble::tibble(table = 'bulk_density', header = 'bulk_density_sample', variable = 'bulk_density_sample', type = 'value_num'),
#     tibble::tibble(table = 'bulk_density', header = 'bulk_density_total', variable = 'bulk_density_total', type = 'value_num'),
#     tibble::tibble(table = 'bulk_density', header = 'bulk_density_whole', variable = 'bulk_density_whole', type = 'value_num'),
#     tibble::tibble(table = 'bulk_density', header = 'bulk_density_other', variable = 'bulk_density_other', type = 'value_num'),
#     tibble::tibble(table = 'bulk_density', header = 'bulk_density_method', variable = c('bulk_density_sample',
# 'bulk_density_total', 'bulk_density_whole', 'bulk_density_other'), type = 'method'),
#     tibble::tibble(table = 'bulk_density', entry = 'g cm-3', variable = c('bulk_density_sample',
# 'bulk_density_total', 'bulk_density_whole', 'bulk_density_other'), type = 'unit')
#     ## fraction mass
# # sand_tot_psa
# # silt_tot_psa
# # clay_tot_psa
# # wpg2
#     ##Carbon and Nitrogen
# # c_tot
# # oc
# # loi
# # n_tot
# # c_to_n
#     ##Phosporous, metals
# # al_dith
# # al_ox
# # al_other
# # fe_dith
# # fe_ox
# # fe_other
# # mn_dith
# # mn_ox
# # mn_other
# # ca_al
# # ca_ext
# # k_ext
# # mg_ext
# # na_ext
# # h_ext
# # zn_ext
# # p_bray
# # p_ox
# # p_meh
# # p_other
#     ##Isotope
# # 15n
# # 13c
# # 14c
# # 14c_age
# # fraction_modern
#     ##Vegitation and roots
# # root_quant_size
# # root_weight
#     ##pH and CEC
# # cat_exch
# # ph_cacl
# # ph_h2o
# # ph_other
# # base_sum
# # cec_sum
# # ecec
# # bs
# # bs_sum
#     ##
#   )
#   

  ##### ISCN3 key ####
  
  ISCN3Key <- dplyr::bind_rows(
    tibble::tibble(table='citation',
                         header = c('ISCN 1-1 (2015-12-10)', 'dataset_name', 'dataset_type (dataset_type)', 'curator_name', 'curator_organization', 'curator_email', 'reference', 'citation', 'citation_usage', 'acknowledgement', 'acknowledgement_usage', 'modification_date (YYYY-MM-DD)'),
                         variable = c('', 'dataset_name', 'dataset_type', 'curator_name', 'curator_organization', 'curator_email', 'reference', 'citation', 'citation_usage', 'acknowledgement', 'acknowledgement_usage', 'modification_date'),
                         type = c('' , 'id', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr')),
    tibble::tribble(~variable, ~type, ~entry,
                    'data_collection', 'valueStr', 'ISCN 1-1 (2015-12-10)'),
    tibble::tibble(table = 'dataset',
                    header = c('ISCN 1-1 (2015-12-10)', 'dataset_name', 'dataset_type (dataset_type)', 'curator_name', 'curator_organization', 'curator_email', 'contact_name', 'contact_email', 'dataset_description', 'total_scount', 'total_pcount', 'total_lcount', 'soc_scount', 'soc_pcount', 'soc_lcount', 'soc_scount_ISCN', 'soc_pcount_ISCN', 'soc_lcount_ISCN', 'modification_date (YYYY-MM-DD)'),
                    variable = c('', 'dataset_name', 'dataset_type', 'curator_name', 'curator_organization', 'curator_email', 'contact_name', 'contact_email', 'dataset_description', 'site_count', 'profile_count', 'layer_count', 'site_soc_count', 'profile_soc_count', 'layer_soc_count', 'site_ISCN_soc_count', 'profile_ISCN_soc_count', 'layer_ISCN_soc_count', 'modification_date'),
                    type=c('', 'id', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'value', 'value', 'value', 'value', 'value', 'value', 'value', 'value', 'value', 'valueStr')),
    tibble::tibble(table='profile',
                    header = c('ISCN 1-1 (2015-12-10)', 'dataset_name_sub', 'dataset_name_soc', 'lat (dec. deg)', 'long (dec. deg)', 'datum (datum)', 'state (state_province)', 'country (country)', 'observation_date (YYYY-MM-DD)', 'site_name', 'profile_name', 'profile_zero_ref (profile_zero_ref)', 'layer_top (cm)', 'layer_bot (cm)', 'total_lcount', 'carbon_lcount', 'soc_lcount', 'soc_depth (cm)', 'soc (g cm-2)', 'soc_method', 'soc_spatial_flag', 'soc_carbon_flag', 'soil_taxon', 'soil_series', 'elevation (m)', 'ecoregion', 'landuse (landsat)', 'vegclass_local', 'surface_veg', 'landscape (landscape)', 'landform (landform)', '2d_position (2d_position)', 'landform_note', 'aspect_deg (degree)', 'slope (percent)', 'drainagecl (drainage)', 'map (mm)', 'mat (°C)', 'runoff (runoff)', 'site_perm (site_perm)', 'site_note', 'thaw_depth_profile (cm)', 'locator_alias', 'locator_parent_alias'),
                    variable = c('', 'dataset_name', 'profile_soc', 'latitude', 'longitude', 'datum', 'state', 'country', 'observation_date', 'site_name', 'profile_name', 'profile_zero_ref', 'profile_top', 'profile_bottom', 'layer_count', 'layer_carbon_count', 'layer_soc_count', 'profile_soc_depth', 'profile_soc', 'profile_soc', 'profile_soc_spatial_flag', 'profile_soc_carbon_flag', 'soil_taxon', 'soil_series', 'elevation', 'ecoregion', 'landuse', 'vegclass_local', 'surface_veg', 'landscape', 'landform', '2d_position', 'landform_note', 'aspect_deg', 'slope', 'drainagecl', 'map', 'mat', 'runoff', 'site_perm', 'site_note', 'thaw_depth_profile', 'locator_alias', 'locator_parent_alias'),
                    type = c('', 'id', 'method02', 'value', 'value', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'id', 'id', 'valueStr', 'value', 'value', 'value', 'value', 'value', 'value', 'value', 'method', 'valueStr', 'valueStr', 'value', 'value', 'value', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'valueStr', 'value', 'value', 'valueStr', 'value', 'value', 'valueStr', 'valueStr', 'valueStr', 'value', 'valueStr', 'valueStr')),
    tibble::tribble(~variable, ~type, ~entry,
                    'latitude', 'unit', 'dec. deg.',
                    'longitude', 'unit', 'dec. deg.',
                    'profile_top', 'unit', 'cm',
                    'profile_bottom', 'unit', 'cm',
                    'profile_soc_depth', 'unit', 'cm',
                    'profile_soc', 'unit', 'g cm-2',
                    'elevation', 'unit', 'm',
                    'aspect_deg', 'unit', 'deg.',
                    'slope', 'unit', 'percent',
                    'map', 'unit', 'mm',
                    'mat', 'unit', 'deg. C',
                    'thaw_depth_profile', 'unit', 'cm'))
  
#   ISCN3Key <- dplyr::bind_rows(
#     tibble::tibble(table = c('layer', 'citation', 'dataset'), 
#                    entry = "ISCN 1-1 (2015-12-10)", variable = 'dataset_name_l1', type = 'value_str'),
#     tibble::tibble(table = 'layer',
#                    header = "dataset_name_sub", variable = 'dataset_name_l0', type='value_str'),
#     tibble::tibble(table = c('citation', 'dataset'),
#                    header = 'dataset_name', variable = 'dataset_name_l0', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = 'dataset_name_soc', variable = 'soc', type='method'),
#     tibble::tibble(table = 'layer', header = "lat (dec. deg)", variable = 'latitude', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = 'decimal degrees', variable = 'latitude', type = 'unit'),
#     tibble::tibble(table = 'layer', header = 'long (dec. deg)', variable = 'longitude', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = 'decimal degrees', variable = 'longitude', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "datum (datum)", variable = 'datum', type='value_num'),
#     tibble::tibble(table = 'layer', entry = 'datum', variable = 'datum', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "state (state_province)", variable = 'geopolitical_unit_2', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "country (country)", variable = 'geopolitical_unit_1', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "site_name", variable = 'site_name', type='value_str'),
#     tibble::tibble(table = 'layer', header = "observation_date (YYYY-MM-DD)", variable = 'observation_date', type = 'value_date'),
#     tibble::tibble(table = 'layer', header = "profile_name", variable = 'profile_name', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "layer_name", variable = 'layer_name', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "layer_top (cm)", variable = 'layer_top', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "layer_bot (cm)", variable = 'layer_bottom', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "hzn_desgn_other", variable = 'horizon_designation_other', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "hzn", variable = 'horizon', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "hzn_desgn", variable = 'horizon_designation', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "layer_note", variable = 'layer_name', type = 'note'),
#     tibble::tibble(table = 'layer', header = "color", variable = 'color', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "vegtype_local", variable = 'vegitation_type', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "soil_taxon", variable = 'soil_taxon', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "soil_series", variable = 'soil_series', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_sample', type = 'method'),
#     tibble::tibble(table = 'layer', header = "bd_samp (g cm-3)", variable = 'bulk_density_sample', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_sample', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_total', type = 'method'),
#     tibble::tibble(table = 'layer', header = "bd_tot (g cm-3)", variable = 'bulk_density_total', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_total', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_whole', type = 'method'),
#     tibble::tibble(table = 'layer', header = "bd_whole (g cm-3)", variable = 'bulk_density_whole', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_whole', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_other', type = 'method'),
#     tibble::tibble(table = 'layer', header = "bd_other (g cm-3)", variable = 'bulk_density_other', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_other', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_sample', type = 'method'),
#     tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_total', type = 'method'),
#     tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_whole', type = 'method'),
#     tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_other', type = 'method'),
#     tibble::tibble(table = 'layer', header = "cNRCS_prep_code", variable = 'c_tot', type = 'method'),
#     tibble::tibble(table = 'layer', header = "cNRCS_prep_code", variable = 'oc', type = 'method'),
#     tibble::tibble(table = 'layer', header = "c_method", variable = 'c_tot', type = 'method'),
#     tibble::tibble(table = 'layer', header = "c_method", variable = 'oc', type = 'method'),
#     tibble::tibble(table = 'layer', header = "c_tot (percent)", variable = 'c_tot', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "oc (percent)", variable = 'oc', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'c_tot', type = 'unit'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'oc', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "loi (percent)", variable = 'loi', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'loi', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "n_tot (percent)", variable = 'n_tot', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'n_tot', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "c_to_n (mass ratio)", variable = 'c_to_n', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = 'mass ratio', variable = 'c_to_n', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "soc (g cm-2)", variable = 'soc', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = 'g cm-2', variable = 'soc', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "soc_carbon_flag", variable = 'soc', type = 'method'),
#     tibble::tibble(table = 'layer', header = "soc_method", variable = 'soc', type = 'method'),
#     tibble::tibble(table = 'layer', header = "ph_method", variable = 'ph_cacl', type = 'method'),
#     tibble::tibble(table = 'layer', header = "ph_method", variable = 'ph_h2o', type = 'method'),
#     tibble::tibble(table = 'layer', header = "ph_method", variable = 'ph_other', type = 'method'),
#     tibble::tibble(table = 'layer', header = "ph_cacl", variable = 'ph_cacl', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "ph_h2o", variable = 'ph_h2o', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "ph_other", variable = 'ph_other', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "caco3 (percent)", variable = 'caco3', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = 'percent', variable = 'caco3', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "sand_tot_psa (percent)", variable = 'sand_tot_psa', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'sand_tot_psa', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "silt_tot_psa (percent)", variable = 'silt_tot_psa', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'silt_tot_psa', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "clay_tot_psa (percent)", variable = 'clay_tot_psa', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'clay_tot_psa', type = 'unit'),
#     tibble::tibble(table = 'layer', header = 'wpg2_method', variable = 'wpg2', type = 'method'),
#     tibble::tibble(table = 'layer', header = 'wpg2 (percent)', variable = 'wpg2', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = 'percent', variable = 'wpg2', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "cat_exch (cmol H+ kg-1)", variable = 'cat_exch', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "cmol H+ kg-1", variable = 'cat_exch', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_dith (specified by al_fe_units)", variable = 'al_dith', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "al_ox (specified by al_fe_units)", variable = 'al_ox', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "al_other (specified by al_fe_units)", variable = 'al_other', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "fe_dith (specified by al_fe_units)", variable = 'fe_dith', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "fe_ox (specified by al_fe_units)", variable = 'fe_ox', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "fe_other (specified by al_fe_units)", variable = 'fe_other', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "mn_dith (specified by al_fe_units)", variable = 'mn_dith', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "mn_ox (specified by al_fe_units)", variable = 'mn_ox', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "mn_other (specified by al_fe_units)", variable = 'mn_other', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'al_dith', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'al_ox', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'al_other', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'fe_dith', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'fe_ox', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'fe_other', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'mn_dith', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'mn_ox', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'mn_other', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "al_fe_method", variable = c('al_dith',  'al_ox', 'al_other', 
#                                                          'fe_dith','fe_ox', 'fe_other', 
#                                                          'mn_dith', 'mn_ox', 'mn_other'), 
#                    type = 'method'),
#     tibble::tibble(table = 'layer', header = "ca_al (specified by bc_units)", variable = 'ca_al', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "ca_ext (specified by bc_units)", variable = 'ca_ext', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "k_ext (specified by bc_units)", variable = 'k_ext', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "mg_ext (specified by bc_units)", variable = "mg_ext", type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "na_ext (specified by bc_units)", variable = 'na_ext', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = 'bc_units (extract_units)', variable = c('ca_al', 'ca_ext', 'k_ext', 'mg_ext', 'na_ext'), type = 'unit'),
#     tibble::tibble(table = 'layer', header = 'bc_method', variable = c('ca_al', 'ca_ext', 'k_ext', 'mg_ext', 'na_ext'), type = 'unit'),
#     tibble::tibble(table = 'layer', header = "base_sum (specified by cec_h_units)", variable = 'base_sum', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "cec_sum (specified by cec_h_units)", variable = 'cec_sum', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "ecec (specified by cec_h_units)", variable = 'ecec', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "cec_h_units (extract_units)", variable = c('base_sum', 'cec_sum', 'ecec'), type = 'unit'),
#     tibble::tibble(table = 'layer', header = "bs (percent)", variable = 'bs', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'bs', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "bs_sum (percent)", variable = 'bs_sum', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = "percent", variable = 'bs_sum', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "h_ext (specified by metal_ext_units)", variable = 'h_ext', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "zn_ext (specified by metal_ext_units)", variable = 'zn_ext', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "metal_ext_units (extract_units)", variable = c('h_ext', 'zn_ext'), type = 'unit'),
#     tibble::tibble(table = 'layer', header = "metal_ext_method", variable = c('h_ext', 'zn_ext'), type = 'method'),
#     tibble::tibble(table = 'layer', header = "p_bray (specified by p_units)", variable = 'p_bray', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "p_ox (specified by p_units)", variable = 'p_ox', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "p_meh (specified by p_units)", variable = 'p_meh', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "p_other (specified by p_units)", variable = 'p_other', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "p_units (extract_units)", variable=c('p_bray', 'p_ox', 'p_meh', 'p_other'), type = 'unit'),
#     tibble::tibble(table = 'layer', header = "p_method", variable=c('p_bray', 'p_ox', 'p_meh', 'p_other'), type = 'method'),
#     tibble::tibble(table = 'layer', header = "root_quant_size", variable = "root_quant_size", type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "root_weight (g)", variable = 'root_weight', type = 'value_num'),
#     tibble::tibble(table = 'layer', entry = 'g', variable = 'root_weight', type = 'unit'),
#     tibble::tibble(table = 'layer', header = "15n (‰)", variable = "15n", type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "13c (‰)", variable = "13c", type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "14c (‰)", variable = "14c", type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "14c_sigma (‰)", variable = "14c", type = 'sigma'),
#     tibble::tibble(table = 'layer', entry = 'permille', variable = c('15n', '13c', '14c'), type = 'unit'),
#     tibble::tibble(table = 'layer', header = "14c_age (BP)", variable = "14c_age", type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "14c_age_sigma (BP)", variable = "14c_age", type = 'sigma'),
#     tibble::tibble(table = 'layer', entry = 'BP', variable = "14c_age", type = 'unit'),                        
#     tibble::tibble(table = 'layer', header = "fraction_modern", variable = 'fraction_modern', type = 'value_num'),
#     tibble::tibble(table = 'layer', header = "fraction_modern_sigma", variable = 'fraction_modern', type = 'sigma'),
#     tibble::tibble(table = 'layer', header = "texturetype", variable = 'texture_type', type = 'value_str'),
#     tibble::tibble(table = 'layer', header = "locator_parent_alias", variable = 'locator_parent_alias', type = 'value_str'),
#     tibble::tibble(table = c('dataset', 'citation'), header = 'dataset_type (dataset_type)', variable = 'dataset_type', type = 'value_str'),
#     tibble::tibble(table = c('dataset', 'citation'), header = 'curator_name', variable = 'curator_name', type = 'value_str'),
#     tibble::tibble(table = c('dataset', 'citation'), header = 'curator_organization', variable = 'curator_organization', type = 'value_str'),
#     tibble::tibble(table = c('dataset', 'citation'), header = 'curator_email', variable = 'curator_email', type = 'value_str'),
#     tibble::tibble(table = c('dataset', 'citation'), header = "modification_date (YYYY-MM-DD)", variable = "modification_date", type = 'value_str'),
#     tibble::tibble(table = 'dataset', 
#                    header = c("contact_name", "contact_email",
#                               "dataset_description"), 
#                    variable = c("contact_name", "contact_email",
#                               "dataset_description"), type = 'value_str'),
#     tibble::tibble(table = 'citation', 
#                    header = c('reference', "citation", "citation_usage", 
#                               "acknowledgement", "acknowledgement_usage"), 
#                    variable = c('reference', "citation", "citation_usage", 
#                               "acknowledgement", "acknowledgement_usage"), type = 'value_str')
#     
#     ##Ignore the following rom dataset.df
# # [16] "total_scount"                   "total_pcount"                   "total_lcount"
# # [19] "soc_scount"                     "soc_pcount"                     "soc_lcount"
# # [22] "soc_scount_ISCN"                "soc_pcount_ISCN"                "soc_lcount_ISCN"
#   )
#   ##Check key notes for manual check
#   #unique(ISCN3Key$table)
#   #(ISCN3Key %>% filter(table == 'dataset'))$header[!(ISCN3Key %>% filter(table == 'dataset'))$header %in% names(dataset.df)]
#   #names(dataset.df)[!names(dataset.df) %in% (ISCN3Key %>% filter(table == 'dataset'))$header]
#   #(ISCN3Key %>% filter(table == 'citation'))$header[!(ISCN3Key %>% filter(table == 'citation'))$header %in% names(citation.df)]
#   #names(citation.df)[!names(citation.df) %in% (ISCN3Key %>% filter(table == 'citation'))$header]
#   #names(allData)[!names(allData) %in% (ISCN3Key %>% filter(table == 'layer'))$header]
#   #(ISCN3Key %>% filter(table == 'layer'))$header[!(ISCN3Key %>% filter(table == 'layer'))$header %in% names(allData)]
#   
  
  return(list(ISCN=ISCN5.target, ISCN3=ISCN3Key))
}