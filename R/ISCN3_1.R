#' International Soil Carbon Network 3_1
#' 
#' First, download ISCN3 data and export files to csv format.
#' This function first loads in the layer, profile, citation, and dataset tables from this pre-downloaded ISCN3 folder.
#' It cleans these tables by recasting columns to appropriate data types, and then replacing ISCN computations with NA values, and removing columns of all NA values.
#' For each dataset, it generates tables, maps, and histograms using this cleaned data.
#' It then returns the reformatted data.
#'
#' @param data_dir 
#' @param datasets_exclude 
#' @param verbose 

#' @importFrom dplyr bind_rows filter full_join group_by intersect mutate mutate_at select ungroup
#' @importFrom lubridate as_date ymd
#' @importFrom readr read_delim
#' @importFrom tidyr fill
#' @importFrom vroom vroom
#' @import magrittr
#'
#' @return
#' @export
ISCN3_1 <- function(data_dir, datasets_exclude = c(), verbose = FALSE){
  
  # TODO: change modification dates
  # TODO: Specify in function description where ISCN data comes from. Probably these websites: (http://iscn.fluxdata.org/data/access-data/database-reports/) data available: ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/* 
  # TODO: Clean up thaw-depth profile to remove coercion NA

  if(!is.character(data_dir)) {
    stop("`data_dir` not set to character value")
  }
  if(!is.character(datasets_exclude) && !is.null(datasets_exclude)) {
    stop(("`dataset_exclude` is not set to vector data structure"))
  }
  if(!is.logical(verbose)) {
    stop("`verbose` is not set to logical value")
  }
  
  
  # #load in library
  # library(data.table)
  # #library(SOCDRaH2)
  # library(tidyverse)
  # library(lubridate)
  # library(tibble)
  # library(ggmap)
  # library(maps)
  # library(mapdata)
  # library(knitr)
  # library(tidyr)
  # library(dplyr)


  # data_dir <- 'ISCN3' #change to location of ISCN3
  # #data_dir <- '~/Documents/Datasets/ISCN' #change to location of ISCN3
  # datasets_exclude <- c() #c('NRCS Sept/2014', 'NRCS 2014:2011 name aliasing')
  # verbose <- TRUE
  # #ISCN3 <- SOCDRaH2::ISCN3(orginalFormat=TRUE)
  # #citation_raw <- data.frame(ISCN3$citation)
  # #dataset_raw <- data.frame(ISCN3$dataset)
  # #profile_raw <- data.frame(ISCN3$profile)
  # #layer_raw <- data.frame(ISCN3$layer)
  # #rm(ISCN3)
  
  
  type_cols <- list(num_cols  = c("lat (dec. deg)", "long (dec. deg)",
                                  "layer_top (cm)", "layer_bot (cm)",
                                  "oc (percent)", 'c_tot (percent)', 'loi (percent)',
                                  'bd_samp (g cm-3)',  'bd_tot (g cm-3)', "bd_whole (g cm-3)", 'bd_other (g cm-3)',
                                  'soc (g cm-2)', "soc_depth (cm)",
                                  'wpg2 (percent)',
                                  'caco3 (percent)',
                                  'sand_tot_psa (percent)', 'silt_tot_psa (percent)', 'clay_tot_psa (percent)', 
                                  'n_tot (percent)', "c_to_n (mass ratio)",
                                  'cat_exch (cmol H+ kg-1)',
                                  "al_dith (specified by al_fe_units)", "al_ox (specified by al_fe_units)", "al_other (specified by al_fe_units)",
                                  "fe_dith (specified by al_fe_units)", "fe_ox (specified by al_fe_units)", "fe_other (specified by al_fe_units)",
                                  "mn_dith (specified by al_fe_units)", "mn_ox (specified by al_fe_units)", "mn_other (specified by al_fe_units)",
                                  "ca_ext (specified by bc_units)", "k_ext (specified by bc_units)", "mg_ext (specified by bc_units)", 
                                  "na_ext (specified by bc_units)",
                                  "ca_al (specified by bc_units)",
                                  'ph_h2o', 'ph_cacl', 'ph_other',
                                  "p_bray (specified by p_units)", "p_ox (specified by p_units)", "p_meh (specified by p_units)",
                                  "p_other (specified by p_units)", 
                                  "base_sum (specified by cec_h_units)", "bs (percent)", "bs_sum (percent)",
                                  "h_ext (specified by metal_ext_units)", "zn_ext (specified by metal_ext_units)",
                                  "cec_sum (specified by cec_h_units)", "ecec (specified by cec_h_units)",  
                                  "13c (‰)", "14c (‰)", '15n (‰)',
                                  "root_weight (g)", "14c_sigma (‰)", "14c_age (BP)", "14c_age_sigma (BP)", 
                                  "fraction_modern", "fraction_modern_sigma",
                                  "elevation (m)", 
                                  "aspect_deg (degree)", "slope (percent)",
                                  "thaw_depth_profile (cm)",
                                  'map (mm)', 'mat (°C)'), 
                    factor_cols = c('dataset_name_sub', "datum (datum)", 
                                    "country (country)", "state (state_province)",
                                    "hzn", "hzn_desgn", "hzn_desgn_other",
                                    "site_perm (site_perm)", "runoff (runoff)",
                                    "soil_series", 'color', 'soil_taxon',
                                    'textureClass',
                                    "profile_zero_ref (profile_zero_ref)", 
                                    "ecoregion", "surface_veg",
                                    'vegclass_local', 
                                    "landuse (landsat)", 'landform (landform)', 'landscape (landscape)', 
                                    '2d_position (2d_position)', 
                                    'drainagecl (drainage)',
                                    'root_quant_size',
                                    "al_fe_units (extract_units)", "metal_ext_units (extract_units)", "p_units (extract_units)",
                                    "bc_units (extract_units)", "cec_h_units (extract_units)",
                                    "bdNRCS_prep_code", "cNRCS_prep_code", 
                                    'dataset_type (dataset_type)'), 
                    date_cols = c("observation_date (YYYY-MM-DD)", 
                                  "modification_date (YYYY-MM-DD)"),
                    char_cols = c("dataset_name", 
                                  'site_name', 'profile_name', 'layer_name',
                                  "curator_name", "curator_organization",
                                  "curator_email",
                                  "contact_name", "contact_email",
                                  "reference","dataset_description", 
                                  "c_method", 'soc_method', "soc_spatial_flag", 'soc_carbon_flag',
                                  'bd_method', 'ph_method', 
                                  'wpg2_method', "p_method",
                                  "al_fe_method", "bc_method", "metal_ext_method", 
                                  'site_note', 'landform_note', 'layer_note',
                                  "citation", "citation_usage", "acknowledgement", "acknowledgement_usage"),
                    discard_cols = c("total_lcount", "carbon_lcount", "soc_lcount", "soc_lcount_ISCN",
                                     "total_pcount", "soc_pcount", "soc_pcount_ISCN",
                                     'total_scount', "soc_scount", "soc_scount_ISCN"),
                    id_cols = c("dataset_name", 
                                'site_name', 'profile_name', 'layer_name',
                                'dataset_name_sub'),
                    noAction_cols = c("ISCN 1-1 (2015-12-10)", "locator_alias", "locator_parent_alias", "dataset_name_soc"))
  
  #missingCols <- setdiff(unique(c(names(citation_raw), names(dataset_raw), names(profile_raw), names(layer_raw))), unlist(type_cols))
  #if(length(missingCols) > 0){
  #  cat(paste('Column names unspecified:', paste(missingCols, collapse = '", "')))
 # }
  
  
  #### Read in the data ####
  citation_raw <- readr::read_delim(file.path(data_dir, 'ISCN3_citation.csv'), delim = ';', col_types = strrep('c', times = 12)) %>% 
    #round all modification dates to their nearest day (ie whole number)
    dplyr::mutate(`modification_date (YYYY-MM-DD)` = as.character(round(as.numeric(`modification_date (YYYY-MM-DD)`))))
  
  dataset_raw <- readr::read_delim(file.path(data_dir, 'ISCN3_dataset.csv'), delim = ';', col_types = strrep('c', times = 19)) %>% 
    #round all modification dates to their nearest day (ie whole number)
    dplyr::mutate(`modification_date (YYYY-MM-DD)` = as.character(round(as.numeric(`modification_date (YYYY-MM-DD)`))))
  
  profile_raw <-  vroom::vroom(file.path(data_dir, 'ISCN3_profile.csv'), col_types = strrep('c', times = 44))
  
  layer_raw <- vroom::vroom(file.path(data_dir, 'ISCN3_layer.csv'), col_types = strrep('c', times = 95))
  
  #initialize list
  
 # ISCN3_1_List <-list(citation_raw, dataset_raw, profile_raw, layer_raw) 
  

  
  #### Defining standardCast() ####
  #Cast the columns in a standard way
  #
  # This function removes the columns that are entirely NA then goes through and uses a list of columns names to cast these to either numeric factor or date. Note that character columns are left alone since we assume all the columns were characters coming into the function.
  #
  # @param data A data frame with column names that match the columns identified in the list `type_cols`
  # @param column_types A list with four character vectors named `num_cols`, `factor_cols`, `date_cols`, `discard_cols` that correspond to columns in `data` that are cast as numeric, factor, lubridate::Date, or discarded from the data frame.
  # 
  # @return a data frame that matches the `data` argument with the column types modified or dropped if specified as discarded or discarded because they started as being NA columns.
  standardCast <- function(data, column_types = type_cols){
    return(data %>%
             dplyr::select(where(function(xx){!all(is.na(xx))})) %>%
             dplyr::mutate_at(dplyr::intersect(c(column_types$num_cols, column_types$date_cols),
                                 names(.)), as.numeric) %>%
             dplyr::mutate_at(dplyr::intersect(column_types$factor_cols, names(.)), as.factor) %>%
             dplyr::mutate_at(dplyr::intersect(column_types$date_cols, names(.)), function(xx){
               ##Both conditions will be run but things throw warnings for the wrong conditional... supressing this function
               suppressWarnings(
                 ans <- case_when(is.na(xx) ~ NA_Date_,
                                  as.numeric(xx) < 2020 ~ lubridate::ymd(paste0(xx, '-01-01')),
                                  as.numeric(xx) >= 2020 ~ lubridate::as_date(as.numeric(xx), 
                                                                              origin = lubridate::ymd('1899-12-31')),
                                  TRUE ~ NA_Date_)
               )
               return(ans)
             }) %>%
             dplyr::select(-any_of(column_types$discard_cols)))
  }
  
  
  #define dataset name from dataframe
  
 # datasetName <- "dataset Name" 
  
  ##### Extract the study information ####
  dataset_study <- citation_raw %>% 
   # filter(dataset_name == datasetName) %>%
    dplyr::select(where(function(xx){!all(is.na(xx))})) %>%
    dplyr::full_join(dataset_raw %>% 
    #            filter(dataset_name == datasetName) %>%
                dplyr::select(where(function(xx){!all(is.na(xx))})), suffix = c('_citation', '_dataset'),
                by = c("dataset_name", "dataset_type (dataset_type)", "curator_name", "curator_organization", "curator_email", "modification_date (YYYY-MM-DD)")) %>%
    standardCast() %>%
    dplyr::group_by(dataset_name) %>%
    tidyr::fill(-dataset_name, .direction = "updown") #replace missing values with known values based on dataset_name grouping
 
  
  #taking profile and layer info
  ##### Extract the profile information ####
  
  #comparison for pre ISCN soc stock correction

  dataset_profile <- profile_raw  %>%
    dplyr::filter(!(dataset_name_sub %in% datasets_exclude)) #make smaller
  
  if(verbose){message('Removing ISCN gap-filled profile-level SOC...')}
  dataset_profile[grepl('ISCN', dataset_profile$dataset_name_soc), 
              c("soc_depth (cm)", "soc (g cm-2)", "soc_carbon_flag", "soc_spatial_flag", "soc_method")] <- NA   #if rows contain "ISCN" in dataset_name_soc, filling set columns (`soc_depth (cm)`, `soc (g cm-2)`, soc_carbon_flag, soc_spatial_flag, soc_method) with NA, otherwise leaving value as is
  
  #remove the soc dataset since we've taken care of the ISCN notation
  dataset_profile <- dataset_profile %>%
    dplyr::select(-dataset_name_soc) 
  if(verbose){message('done')}
 
  #reduces run time by removing single rows from processing 
  if(verbose){message('Make sure profiles fill in duplicate groups...')}
  temp <- dataset_profile %>%
    dplyr::group_by(dataset_name_sub, site_name, profile_name) %>%
    dplyr::filter(length(profile_name) > 1) %>%
    tidyr::fill(-dplyr::group_vars(.), .direction = 'updown') %>%
    base::unique()
  
  dataset_profile <- dataset_profile %>%
    dplyr::filter(length(profile_name) == 1) %>%
    dplyr::bind_rows(temp)
    #dplyr::group_by(dataset_name_sub, site_name, profile_name) %>%
    #tidyr::fill(-dplyr::group_vars(.), .direction = 'updown')
  if(verbose){message('done')}
  
  #hardcoding country name
  replacecountry <- c("Heckman/Swanston Biscuit Burn", "Oak Ridge National Lab_Lolly_DWJ", "Lehmann Soil C&BC #1", "Schuur", "Lehmann NE US soils", "USGS Harden Yazoo", "UMBS_FASET", "Oak Ridge National Lab_TDE", "USDA-FS NRS Landscape Carbon Inventory", "USGS_S3C")
  dataset_profile[dataset_profile$dataset_name_sub %in% replacecountry, 'country (country)'] <- 'United States'
  
  #filling citations
 
  # fillciteHardenYazoo <- c("USGS Harden Yazoo")
  # dataset_profile[dataset_profile$dataset_name_sub %in% fillciteHardenYazoo, 'site_note'] <- 
  
    dataset_profile <- dataset_profile %>%
    ungroup() %>%
    mutate(`site_note` = case_when(
              site_note == 'see Parr and Hughes 2006' ~ 'Parr, P., & Hughes, J.F. (2006). OAK RIDGE RESERVATION PHYSICAL CHARACTERISTICS AND NATURAL RESOURCES.', #from Oak Ridge National Lab_Loblolly_DWJ
              site_note == 'see Muhs et al., 2003, Stratigraphy and palaeoclimatic signficance …' ~ 'Muhs et al.(2003).Stratigraphy and palaeoclimatic significance of Late Quaternary loess-palaeosol sequences of the Last Interglacial-Glacial cycle in central Alaska.', #from USGS Muhs
             site_note == 'Harden et al., 1999' ~ 'Harden J, Fries T, Huntington T. 1999. MS Basin Carbon Project: Upland soil database for sites in Yazoo Basin, northern MS. USGS Open file report 99-319.', #from USGS Harden Yazoo
             site_note == 'Harden et al., 1999; Huntington et al. 1998' ~ 'Harden J, Fries T, Huntington T. 1999. MS Basin Carbon Project: Upland soil database for sites in Yazoo Basin, northern MS. USGS Open file report 99-319; Huntington, T.G., Harden, J. W., Dabney, S. M. , Marion, D. A. , Alonso, C., Sharpe, J.M. , 1998. Soil, Environmental, and Watershed Measurements in support of carbon cycling studies in northwestern Mississippi. U.S. Geological Survey Open-File Report 98-501.', #from USGS Harden Yazoo
              TRUE ~ site_note))
  
  #  test2 <- list("a" = 1, "b" = 2, "d" = 3)
      
  if(verbose){message('Cast datatypes in profile-level...')}
  dataset_profile <- dataset_profile %>%
    dplyr::ungroup() %>% #groups are not needed in casting and it runs faster ungrouped
    standardCast()
  if(verbose){message('done')}

   #kept for legacy incase the above code doesn't work for one of the testcases
  # if(any(count(dataset_profile, dataset_name_sub, site_name, profile_name)$n > 1)){
  #   #if the rows are duplicated then fill in missing values by group
  #   dataset_profile <- dataset_profile %>%
  #     filter(!grepl("NRCS", dataset_name_sub)) %>%
  #     group_by(dataset_name_sub, site_name, profile_name) %>%
  #     mutate_at(vars(-group_cols()),
  #               function(xx){ifelse(sum(!is.na(xx)) == 1, rep(xx[!is.na(xx)], length(xx)),xx)}) %>% #if there is one value that isn't NA then populate the rest of the entry, this fills in the
  #     ungroup() %>%
  #     unique() %>% #collapse rows that are non-unique
  #     standardCast()
  # }

  
  
  
  
  
  ##### Extract the layer information ####
  
  #comparison for pre ISCN soc stock correction
  dataset_layer <- layer_raw  
  
  if(verbose){message('Removing ISCN gap-filled layer-level SOC...')}
  #Deal with ISCN gap-filled values
  dataset_layer[grepl('ISCN', dataset_layer$dataset_name_soc), 
                  c("soc (g cm-2)", "soc_carbon_flag", "soc_method")] <- NA   #if rows contain "ISCN" in dataset_name_soc, filling set columns (`soc (g cm-2)`, soc_carbon_flag, soc_method) with NA, otherwise leaving value as is

  #remove the soc dataset since we've taken care of the ISCN notation
  dataset_layer <- dataset_layer %>%
    dplyr::filter(!(dataset_name_sub %in% datasets_exclude)) %>% #make smaller
    dplyr::select(-dataset_name_soc) %>%
    dplyr::group_by(dataset_name_sub, site_name, profile_name, layer_name)
  if(verbose){message('done.')}
  
  if(verbose){message('Fill in missing values in repeat layer-level rows then remove duplicates...')}
  temp <- dataset_layer %>%
    dplyr::group_by(dataset_name_sub, site_name, profile_name, layer_name) %>%
    dplyr::filter(length(layer_name) > 1) %>%
    tidyr::fill(-dplyr::group_vars(.), .direction = 'updown') %>%
    base::unique()
  
  dataset_layer <- dataset_layer %>%
    dplyr::filter(length(layer_name) == 1) %>%
    dplyr::bind_rows(temp)
  if(verbose){message('done.')}
  
  #replacing special characters with NA
  replacequestion <- c("Oak Ridge National Lab_Loblolly_DWJ", "Lehmann Soil C&BC #1", "schuur", "USGS Harden Yazoo", "Bonanza LTER", "UMBS_FASET", "Oak Ridge NationalLab_TDE", "USDA-FS NRS Landscape Carbon Inventory", "Northern Circumpolar Soil Carbon Database (NCSCD)")
  dataset_layer[dataset_layer$dataset_name_sub %in% replacequestion, 'hzn'] <- NA_character_
  replaceunknown <- c("Bonanza LTER", "USDA-FS NRS Landscape Carbon Inventory")
  dataset_layer[dataset_layer$dataset_name_sub %in% replaceunknown, 'hzn_desgn'] <- NA_character_
  
  if(verbose){message('Cast data types in layer-level...')}
  dataset_layer <- dataset_layer %>%
    dplyr::ungroup() %>% #groups are not needed in casting and it runs faster ungrouped
    standardCast()
  if(verbose){message('done.')}
  
  #correcting missing country info in layer data, for when in USA
  # dataset_layer <- dataset_layer %>%
  #   dplyr::filter(`country (country)` == "Unknown") %>%
  #   dplyr::filter(!is.na(`state (state_province)`)) %>%
  #   mutate(`country (country)` = as.factor(c("United States")))
  # 
  # #doing the same but with profile data
  # dataset_profile <- dataset_profile %>%
  #   dplyr::filter(`country (country)` == "Unknown") %>%
  #   dplyr::filter(!is.na(`state (state_province)`)) %>%
  #   mutate(`country (country)` = as.factor(c("United States")))
  
  dplyr::if_else(dataset_layer$`country (country)`== "Unkown",
                 dataset_layer <- dataset_layer %>%
                   mutate(`country (country)` = as.factor(c("United States"))),
                 dataset_layer <- dataset_layer)

  # fixedCountryLayerData <- 
  #   if(is.na(datasetLayer$`country (country)`[1])) {
  #     datasetLayer %>%
  #       mutate(`country (country)` = as.factor(c("United States")))
  #   } else if(datasetLayer$`country (country)`[1] == "Unknown") {
  #     datasetLayer %>%
  #       mutate(`country (country)` = as.factor(c("United States")))
  #   } else{datasetLayer}

  
  
  #put if statements to catch if it's a particular dataset/frame which will perform special functions to do what we need to
  
  
  return(list(study = dataset_study, 
              profile = dataset_profile,
              layer = dataset_layer,
              type_columns = type_cols))
}


