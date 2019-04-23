#' Load ISCN Layer and Meta data
#'
#' This function first downloads the layer and meta data from the ISCN website. Presevers the orginal data structure.
#' ISCN (http://iscn.fluxdata.org/data/access-data/database-reports/) data available: ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C1_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C2_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C3_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C4_1-1.xlsx
#'
#' @param dataDir path to the folder contianing ISCN_ALL_DATA_LAYER_C*_1-1.xlsx, ISCN_ALL-DATA-CITATION_1-1.xlsx and ISCN_ALL_DATA_DATASET_1-1.xlsx files. If this is left NULL then files will be downloaded to a temporary directory from the ISCN website and then deleted.
#' @param verbose boolean flag denoting whether or not to print lots of status messages
#' 
#' @return list of data.table with layer, dataset and citation information
#'
#' @importFrom data.table rbindlist
#' @importFrom readxl read_excel
#' @importFrom tibble tibble
#' @export
#' 
ISCN3 <- function(dataDir=NULL, verbose=FALSE){
  
  ## ISCN key ####
  
  ISCN3Key <- dplyr::bind_rows(
    tibble::tibble(table = c('layer', 'citation', 'dataset'), 
                   entry = "ISCN 1-1 (2015-12-10)", variable = 'dataset_name_l0', class = 'value_str'),
    tibble::tibble(table = 'layer',
                   header = "dataset_name_sub", variable = 'dataset_name_l1', class='value_str'),
    tibble::tibble(table = c('citation', 'dataset'),
                   header = 'dataset_name', variable = 'dataset_name_l1', class = 'value_str'),
    tibble::tibble(table = 'layer', header = 'dataset_name_soc', variable = 'soc', class='method'),
    tibble::tibble(table = 'layer', header = "lat (dec. deg)", variable = 'latitude', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = 'decimal degrees', variable = 'latitude', class = 'unit'),
    tibble::tibble(table = 'layer', header = 'long (dec. deg)', variable = 'longitude', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = 'decimal degrees', variable = 'longitude', class = 'unit'),
    tibble::tibble(table = 'layer', header = "datum (datum)", variable = 'datum', class='value_num'),
    tibble::tibble(table = 'layer', entry = 'datum', variable = 'datum', class = 'unit'),
    tibble::tibble(table = 'layer', header = "state (state_province)", variable = 'political_unit_2', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "country (country)", variable = 'political_unit_1', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "site_name", variable = 'site_name', class='value_str'),
    tibble::tibble(table = 'layer', header = "observation_date (YYYY-MM-DD)", variable = 'observation_date', class = 'value_date'),
    tibble::tibble(table = 'layer', header = "profile_name", variable = 'profile_name', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "layer_name", variable = 'layer_name', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "layer_top (cm)", variable = 'layer_top', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "layer_bot (cm)", variable = 'layer_bottom', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "hzn_desgn_other", variable = 'horizon_designation_other', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "hzn", variable = 'horizon', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "hzn_desgn", variable = 'horizon_designation', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "layer_note", variable = 'layer_name', class = 'note'),
    tibble::tibble(table = 'layer', header = "color", variable = 'color', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "vegclass_local", variable = 'vegitation_class', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "soil_taxon", variable = 'soil_taxon', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "soil_series", variable = 'soil_series', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_sample', class = 'method'),
    tibble::tibble(table = 'layer', header = "bd_samp (g cm-3)", variable = 'bulk_density_sample', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_sample', class = 'unit'),
    tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_total', class = 'method'),
    tibble::tibble(table = 'layer', header = "bd_tot (g cm-3)", variable = 'bulk_density_total', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_total', class = 'unit'),
    tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_whole', class = 'method'),
    tibble::tibble(table = 'layer', header = "bd_whole (g cm-3)", variable = 'bulk_density_whole', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_whole', class = 'unit'),
    tibble::tibble(table = 'layer', header = "bd_method", variable = 'bulk_density_other', class = 'method'),
    tibble::tibble(table = 'layer', header = "bd_other (g cm-3)", variable = 'bulk_density_other', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "g cm-3", variable = 'bulk_density_other', class = 'unit'),
    tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_sample', class = 'method'),
    tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_total', class = 'method'),
    tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_whole', class = 'method'),
    tibble::tibble(table = 'layer', header = "bdNRCS_prep_code", variable = 'bulk_density_other', class = 'method'),
    tibble::tibble(table = 'layer', header = "cNRCS_prep_code", variable = 'c_tot', class = 'method'),
    tibble::tibble(table = 'layer', header = "cNRCS_prep_code", variable = 'oc', class = 'method'),
    tibble::tibble(table = 'layer', header = "c_method", variable = 'c_tot', class = 'method'),
    tibble::tibble(table = 'layer', header = "c_method", variable = 'oc', class = 'method'),
    tibble::tibble(table = 'layer', header = "c_tot (percent)", variable = 'c_tot', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "oc (percent)", variable = 'oc', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'c_tot', class = 'unit'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'oc', class = 'unit'),
    tibble::tibble(table = 'layer', header = "loi (percent)", variable = 'loi', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'loi', class = 'unit'),
    tibble::tibble(table = 'layer', header = "n_tot (percent)", variable = 'n_tot', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'n_tot', class = 'unit'),
    tibble::tibble(table = 'layer', header = "c_to_n (mass ratio)", variable = 'c_to_n', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = 'mass ratio', variable = 'c_to_n', class = 'unit'),
    tibble::tibble(table = 'layer', header = "soc (g cm-2)", variable = 'soc', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = 'g cm-2', variable = 'soc', class = 'unit'),
    tibble::tibble(table = 'layer', header = "soc_carbon_flag", variable = 'soc', class = 'method'),
    tibble::tibble(table = 'layer', header = "soc_method", variable = 'soc', class = 'method'),
    tibble::tibble(table = 'layer', header = "ph_method", variable = 'ph_cacl', class = 'method'),
    tibble::tibble(table = 'layer', header = "ph_method", variable = 'ph_h2o', class = 'method'),
    tibble::tibble(table = 'layer', header = "ph_method", variable = 'ph_other', class = 'method'),
    tibble::tibble(table = 'layer', header = "ph_cacl", variable = 'ph_cacl', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "ph_h2o", variable = 'ph_h2o', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "ph_other", variable = 'ph_other', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "caco3 (percent)", variable = 'caco3', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = 'percent', variable = 'caco3', class = 'unit'),
    tibble::tibble(table = 'layer', header = "sand_tot_psa (percent)", variable = 'sand_tot_psa', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'sand_tot_psa', class = 'unit'),
    tibble::tibble(table = 'layer', header = "silt_tot_psa (percent)", variable = 'silt_tot_psa', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'silt_tot_psa', class = 'unit'),
    tibble::tibble(table = 'layer', header = "clay_tot_psa (percent)", variable = 'clay_tot_psa', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'clay_tot_psa', class = 'unit'),
    tibble::tibble(table = 'layer', header = 'wpg2_method', variable = 'wpg2', class = 'method'),
    tibble::tibble(table = 'layer', header = 'wpg2 (percent)', variable = 'wpg2', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = 'percent', variable = 'wpg2', class = 'unit'),
    tibble::tibble(table = 'layer', header = "cat_exch (cmol H+ kg-1)", variable = 'cat_exch', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "cmol H+ kg-1", variable = 'cat_exch', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_dith (specified by al_fe_units)", variable = 'al_dith', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "al_ox (specified by al_fe_units)", variable = 'al_ox', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "al_other (specified by al_fe_units)", variable = 'al_other', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "fe_dith (specified by al_fe_units)", variable = 'fe_dith', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "fe_ox (specified by al_fe_units)", variable = 'fe_ox', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "fe_other (specified by al_fe_units)", variable = 'fe_other', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "mn_dith (specified by al_fe_units)", variable = 'mn_dith', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "mn_ox (specified by al_fe_units)", variable = 'mn_ox', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "mn_other (specified by al_fe_units)", variable = 'mn_other', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'al_dith', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'al_ox', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'al_other', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'fe_dith', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'fe_ox', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'fe_other', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'mn_dith', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'mn_ox', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_units (extract_units)", variable = 'mn_other', class = 'unit'),
    tibble::tibble(table = 'layer', header = "al_fe_method", variable = c('al_dith',  'al_ox', 'al_other', 
                                                         'fe_dith','fe_ox', 'fe_other', 
                                                         'mn_dith', 'mn_ox', 'mn_other'), 
                   class = 'method'),
    tibble::tibble(table = 'layer', header = "ca_al (specified by bc_units)", variable = 'ca_al', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "ca_ext (specified by bc_units)", variable = 'ca_ext', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "k_ext (specified by bc_units)", variable = 'k_ext', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "mg_ext (specified by bc_units)", variable = "mg_ext", class = 'value_num'),
    tibble::tibble(table = 'layer', header = "na_ext (specified by bc_units)", variable = 'na_ext', class = 'value_num'),
    tibble::tibble(table = 'layer', header = 'bc_units (extract_units)', variable = c('ca_al', 'ca_ext', 'k_ext', 'mg_ext', 'na_ext'), class = 'unit'),
    tibble::tibble(table = 'layer', header = 'bc_method', variable = c('ca_al', 'ca_ext', 'k_ext', 'mg_ext', 'na_ext'), class = 'unit'),
    tibble::tibble(table = 'layer', header = "base_sum (specified by cec_h_units)", variable = 'base_sum', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "cec_sum (specified by cec_h_units)", variable = 'cec_sum', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "ecec (specified by cec_h_units)", variable = 'ecec', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "cec_h_units (extract_units)", variable = c('base_sum', 'cec_sum', 'ecec'), class = 'unit'),
    tibble::tibble(table = 'layer', header = "bs (percent)", variable = 'bs', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'bs', class = 'unit'),
    tibble::tibble(table = 'layer', header = "bs_sum (percent)", variable = 'bs_sum', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = "percent", variable = 'bs_sum', class = 'unit'),
    tibble::tibble(table = 'layer', header = "h_ext (specified by metal_ext_units)", variable = 'h_ext', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "zn_ext (specified by metal_ext_units)", variable = 'zn_ext', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "metal_ext_units (extract_units)", variable = c('h_ext', 'zn_ext'), class = 'unit'),
    tibble::tibble(table = 'layer', header = "metal_ext_method", variable = c('h_ext', 'zn_ext'), class = 'method'),
    tibble::tibble(table = 'layer', header = "p_bray (specified by p_units)", variable = 'p_bray', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "p_ox (specified by p_units)", variable = 'p_ox', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "p_meh (specified by p_units)", variable = 'p_meh', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "p_other (specified by p_units)", variable = 'p_other', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "p_units (extract_units)", variable=c('p_bray', 'p_ox', 'p_meh', 'p_other'), class = 'unit'),
    tibble::tibble(table = 'layer', header = "p_method", variable=c('p_bray', 'p_ox', 'p_meh', 'p_other'), class = 'method'),
    tibble::tibble(table = 'layer', header = "root_quant_size", variable = "root_quant_size", class = 'value_num'),
    tibble::tibble(table = 'layer', header = "root_weight (g)", variable = 'root_weight', class = 'value_num'),
    tibble::tibble(table = 'layer', entry = 'g', variable = 'root_weight', class = 'unit'),
    tibble::tibble(table = 'layer', header = "15n (‰)", variable = "15n", class = 'value_num'),
    tibble::tibble(table = 'layer', header = "13c (‰)", variable = "13c", class = 'value_num'),
    tibble::tibble(table = 'layer', header = "14c (‰)", variable = "14c", class = 'value_num'),
    tibble::tibble(table = 'layer', header = "14c_sigma (‰)", variable = "14c", class = 'sigma'),
    tibble::tibble(table = 'layer', entry = 'permille', variable = c('15n', '13c', '14c'), class = 'unit'),
    tibble::tibble(table = 'layer', header = "14c_age (BP)", variable = "14c_age", class = 'value_num'),
    tibble::tibble(table = 'layer', header = "14c_age_sigma (BP)", variable = "14c_age", class = 'sigma'),
    tibble::tibble(table = 'layer', entry = 'BP', variable = "14c_age", class = 'unit'),                        
    tibble::tibble(table = 'layer', header = "fraction_modern", variable = 'fraction_modern', class = 'value_num'),
    tibble::tibble(table = 'layer', header = "fraction_modern_sigma", variable = 'fraction_modern', class = 'sigma'),
    tibble::tibble(table = 'layer', header = "textureClass", variable = 'texture_class', class = 'value_str'),
    tibble::tibble(table = 'layer', header = "locator_parent_alias", variable = 'locator_parent_alias', class = 'value_str'),
    tibble::tibble(table = c('dataset', 'citation'), header = 'dataset_type (dataset_type)', variable = 'dataset_type', class = 'value_str'),
    tibble::tibble(table = c('dataset', 'citation'), header = 'curator_name', variable = 'curator_name', class = 'value_str'),
    tibble::tibble(table = c('dataset', 'citation'), header = 'curator_organization', variable = 'curator_organization', class = 'value_str'),
    tibble::tibble(table = c('dataset', 'citation'), header = 'curator_email', variable = 'curator_email', class = 'value_str'),
    tibble::tibble(table = c('dataset', 'citation'), header = "modification_date (YYYY-MM-DD)", variable = "modification_date (YYYY-MM-DD)", class = 'value_str'),
    tibble::tibble(table = 'dataset', 
                   header = c("contact_name", "contact_email",
                              "dataset_description"), 
                   variable = c("contact_name", "contact_email",
                              "dataset_description"), class = 'value_str'),
    tibble::tibble(table = 'citation', 
                   header = c('reference', "citation", "citation_usage", 
                              "acknowledgement", "acknowledgement_usage"), 
                   variable = c('reference', "citation", "citation_usage", 
                              "acknowledgement", "acknowledgement_usage"), class = 'value_str')
    
    ##Ignore the following rom dataset.df
# [16] "total_scount"                   "total_pcount"                   "total_lcount"
# [19] "soc_scount"                     "soc_pcount"                     "soc_lcount"
# [22] "soc_scount_ISCN"                "soc_pcount_ISCN"                "soc_lcount_ISCN"
  )
  ##Check key notes for manual check
  #unique(ISCN3Key$table)
  #(ISCN3Key %>% filter(table == 'dataset'))$header[!(ISCN3Key %>% filter(table == 'dataset'))$header %in% names(dataset.df)]
  #names(dataset.df)[!names(dataset.df) %in% (ISCN3Key %>% filter(table == 'dataset'))$header]
  #(ISCN3Key %>% filter(table == 'citation'))$header[!(ISCN3Key %>% filter(table == 'citation'))$header %in% names(citation.df)]
  #names(citation.df)[!names(citation.df) %in% (ISCN3Key %>% filter(table == 'citation'))$header]
  #names(allData)[!names(allData) %in% (ISCN3Key %>% filter(table == 'layer'))$header]
  #(ISCN3Key %>% filter(table == 'layer'))$header[!(ISCN3Key %>% filter(table == 'layer'))$header %in% names(allData)]
  
  
  ## construct file paths ####
  delete_dataDir <- is.null(dataDir)
  if(is.null(dataDir)){
    dataDir <- tempdir()
  }
  
  layerDataFiles.arr <- file.path(dataDir, c('ISCN_ALL_DATA_LAYER_C1_1-1.xlsx', 
                                             'ISCN_ALL_DATA_LAYER_C2_1-1.xlsx',
                                             'ISCN_ALL_DATA_LAYER_C3_1-1.xlsx', 
                                             'ISCN_ALL_DATA_LAYER_C4_1-1.xlsx'))
  dataFiles.arr <- c(layerDataFiles.arr, 
                         file.path(dataDir, c(
                                            'ISCN_ALL-DATA-CITATION_1-1.xlsx', 
                                            'ISCN_ALL_DATA_DATASET_1-1.xlsx')))
  
  ## Download the data ####
  if(verbose) print('Download file.')
  
  for(dataFiles.arr in dataFiles.arr){
    if(!file.exists(dataFiles.arr)){
      download.file(sprintf('ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/%s', basename(dataFiles.arr)), 
                    dataFiles.arr, quiet=FALSE)
    }
  }

  ## Read data files ####
  if(verbose) print('Layer data read in.')

  layer.dt <- data.table::rbindlist(lapply(layerDataFiles.arr, 
                                                function(xx){
                                  readxl::read_excel(path=xx, sheet='layer', col_types='text')}))
  
  if(verbose) print('Meta data read in.')
  
  citation.dt <- readxl::read_excel(path=paste(dataDir, 'ISCN_ALL-DATA-CITATION_1-1.xlsx', sep='/'),
                                    sheet='citation')
  
  dataset.dt <- readxl::read_excel(path=paste(dataDir, 'ISCN_ALL_DATA_DATASET_1-1.xlsx', sep='/'), 
                                   sheet='dataset')
  
  return(list(citation=citation.dt, dataset=dataset.dt, layer = layer.dt, key = ISCN3Key))

}
