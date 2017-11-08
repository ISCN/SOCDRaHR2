#' Load ISCN Layer and Meta data
#'
#' ISCN (http://iscn.fluxdata.org/data/access-data/database-reports/) data available: ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C1_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C2_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C3_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C4_1-1.xlsx
#'
#' @param layersDir path to the folder contianing ISCN_ALL_DATA_LAYER_C*_1-1.csv files; R doesn't play nicely with large xlsx files so we fall back on csv exports
#' @param metaDir path to the folder contianingISCN_ALL-DATA-CITATION_1-1.xlsx and ISCN_ALL_DATA_DATASET_1-1.xlsx files
#' @param verbose boolean flag denoting whether or not to print lots of status messages
#' @param onlyPullKey Only return the ISCN key not the data
#' @param loadVars an array of characters to read in only certain variables
#'
#' @export
processData_ISCN3 <- function(layersDir=NULL, metaDir=NULL, verbose=FALSE, onlyISCNKey=FALSE, loadVars=NULL){

  #debug.ls <- list(layersDir = '../soils-long-tail-recovery/repoData/ISCN_3/Layers',
  #                  metaDir = '../soils-long-tail-recovery/repoData/ISCN_3/Meta/',
  #                  verbose = TRUE, onlyISCNKey=FALSE)
  # attach(debug.ls)

  library(dplyr)
  library(tidyr)
  library(readr)

  #### Make ISCN Key ####
  ISCNKey <- bind_rows(##In meta files
    list(header = 'dataset_name', var='dataset_name', dataframe='study', class='factor'),
    list(header = 'dataset_type (dataset_type)', var='dataset_type', dataframe='study', class='factor'),
    list(header = 'curator_name', var='curator_name', dataframe='study', class='factor'),
    list(header = 'curator_organization', var='curator_organization', dataframe='study', class='factor'),
    list(header = 'curator_email', var='curator_email', dataframe='study', class='factor'),
    list(header = 'contact_name', var='contact_name', dataframe='study', class='factor'),
    list(header = 'contact_email', var='contact_email', dataframe='study', class='factor'),
    list(header = 'reference', var='reference', dataframe='study', class='factor'),
    list(header = 'citation', var='citation', dataframe='study', class='factor'),
    list(header = 'citation_usage', var='citation_usage', dataframe='study', class='factor'),
    list(header = 'acknowledgement', var='acknowledgement', dataframe='study', class='factor'),
    list(header = 'acknowledgement_usage', var='acknowledgement_usage', dataframe='study', class='factor'),
    list(header = 'modification_date (YYYY-MM-DD)', var='modification_date', dataframe='study',
         class='factor'),
    list(header = 'dataset_description', var='dataset_description', dataframe='study', class='factor'),
    ##orphaned columsn from meta that we are ignoring
    list(header='total_scount', class='numeric'),
    list(header='total_pcount', class='numeric'),
    list(header='total_lcount', class='numeric'),
    list(header='soc_scount', class='numeric'),
    list(header='soc_pcount', class='numeric'),
    list(header='soc_lcount', class='numeric'),
    list(header='soc_scount_ISCN', class='numeric'),
    list(header='soc_pcount_ISCN', class='numeric'),
    list(header='soc_lcount_ISCN', class='numeric'),
    ##Sub-Study data
    #list(header="ISCN 1-1 (2015-12-10)", var='dataset_provider', dataframe = 'study', class='factor'),
    list(header="dataset_name_sub", var='dataset_name', dataframe='study',
         class='factor'),
    list(header="dataset_name_soc", var='soc', type='method', dataframe='sample',
         class='character'), ##include in soc methods
    ##Field data
    list(header="lat (dec. deg)", var='lat', dataframe='field', unit='dec. deg',
         class='numeric'),
    list(header="long (dec. deg)", var='lon', dataframe='field', unit='dec. deg',
         class='numeric'),
    list(header="datum (datum)", var='datum', dataframe='field', controlVocab=TRUE,
         class='factor'),
    list(header="state (state_province)", var='state', dataframe='field',
         class='factor'),
    list(header="country (country)", var='country', dataframe='field',
         class='factor'),
    list(header="site_name", var='site_name', dataframe='field', class='factor'),
    list(header="observation_date (YYYY-MM-DD)", var='observation_date',
         dataframe='field', unit = 'YYYY-MM-DD',
         class='factor'),
    list(header="profile_name", var='profile_name', dataframe='field', class='factor'),
    list(header="layer_name", var='layer_name', dataframe='field', class='character'),
    list(header="layer_top (cm)", var='layer_top', dataframe='field', unit='cm',
         class='numeric'),
    list(header="layer_bot (cm)", var='layer_bottom', dataframe='field', unit='cm',
         class='numeric'),
    list(header="hzn_desgn_other", var='hzn_desgn_other', dataframe='field',
         class='factor'),
    list(header="hzn", var='hzn', dataframe='field',
         class='factor'),
    list(header="hzn_desgn", var='hzn_desgn', dataframe='field',
         class='factor'),
    list(header="layer_note", var='layer_note', dataframe='field',
         class='factor'),
    list(header="color", var='color', dataframe='field',
         class='factor'),
    list(header="vegclass_local", var='vegclass_local', dataframe='field',
         class='factor'),
    list(header="soil_taxon", var='soil_taxon', dataframe='field',
         class='factor'),
    list(header="soil_series", var='soil_series', dataframe='field',
         class='factor'),
    ##measurements
    #### Bulk density
    list(header="bd_method", var='(bd_sample)|(bd_tot)|(bd_other)|(bd_whole)',
         type='method', dataframe='sample', class='character'),
    list(header="bd_samp (g cm-3)", var='bd_sample', type='value', unit='g cm-3',
         dataframe='sample', method='(bd_method)|(bdNRCS_prep_code)', class='numeric'),
    list(header="bd_tot (g cm-3)", var='bd_tot', type='value', unit='g cm-3',
         dataframe='sample', method='(bd_method)|(bdNRCS_prep_code)', class='numeric'),
    list(header="bd_whole (g cm-3)", var='bd_whole', type='value', unit='g cm-3',
         dataframe='sample', method='(bd_method)|(bdNRCS_prep_code)', class='numeric'),
    list(header="bd_other (g cm-3)", var='bd_other', type='value', unit='g cm-3',
         dataframe='sample', method='(bd_method)|(bdNRCS_prep_code)', class='numeric'),
    list(header="bdNRCS_prep_code", var='(bd_sample)|(bd_tot)|(bd_other)|(bd_whole)',
         type='method', dataframe='sample', class='factor'),
    #### Carbon and Nitrogen
    list(header="cNRCS_prep_code", var='c_tot', type='method', dataframe='sample',
         class='factor'),
    list(header="c_method", var='c_tot', type='method', dataframe='sample',
         class='character'),
    list(header="c_tot (percent)", var='c_tot', type='value', unit='percent',
         dataframe='sample', method='(c_method)|(cNRCS_prep_code)', class='numeric'),
    list(header="oc (percent)", var='oc', type='value', unit='percent', dataframe='sample',
         class='numeric'),
    list(header="loi (percent)", var='loi', type='value', unit='percent',
         dataframe='sample', class='numeric'),
    list(header="n_tot (percent)", var='n_tot', type='value', unit ='percent', dataframe='sample', class='numeric'),
    list(header="c_to_n (mass ratio)", var='c_to_n', type='value', unit='mass ratio', dataframe='sample', class='numeric'),
    ##SOC methods are uniquely constructed to include dataset_name_soc, bd, and oc methods
    list(header="soc (g cm-2)", var='soc', type='value', unit='g cm-2', dataframe='sample',
         method='(soc_carbon_flag)|(soc_method)', class='numeric'),
    list(header="soc_carbon_flag", var='soc', type='method', dataframe='sample',
         class='factor'),
    list(header="soc_method", var='soc', type='method', dataframe='sample',
         class='character'),
    ###pH
    list(header="ph_method", var='(ph_cacl)|(ph_h2o)|(ph_other)', type='method',
         dataframe='sample', class='character'),
    list(header="ph_cacl", var='ph_cacl', type='value', dataframe='sample',
         method='ph_method', class='numeric'),
    list(header="ph_h2o", var='ph_h2o', type='value', dataframe='sample',
         method='ph_method', class='numeric'),
    list(header="ph_other", var='ph_other', type='value', dataframe='sample',
         method='ph_method', class='numeric'),
    list(header="caco3 (percent)", var='caco3', type='value', dataframe='sample',
         class='numeric'),
    ###Texture
    list(header="sand_tot_psa (percent)", var='sand_tot_psa', type='value', unit='percent',
         dataframe='sample', class='numeric'),
    list(header="silt_tot_psa (percent)", var='silt_tot_psa', type='value', unit='percent',
         dataframe='sample', class='numeric'),
    list(header="clay_tot_psa (percent)", var='clay_tot_psa', type='value', unit='percent',
         dataframe='sample', class='numeric'),
    list(header="wpg2_method", var='wpg2', type='method', dataframe='sample',
         class='character'),
    list(header="wpg2 (percent)", var='wpg2', type='value', unit='percent',
         dataframe='sample', method='wpg2_method', class='numeric'),
    ###Metals
    #### al fe mn
    list(header="cat_exch (cmol H+ kg-1)", var='cat_exch', type='value',
         unit='cmol H+ kg-1', dataframe='sample', class='numeric'),
    list(header="al_dith (specified by al_fe_units)", var='al_dith', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="al_ox (specified by al_fe_units)", var='al_ox', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="al_other (specified by al_fe_units)", var='al_other', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="fe_dith (specified by al_fe_units)", var='fe_dith', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="fe_ox (specified by al_fe_units)", var='fe_ox', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="fe_other (specified by al_fe_units)", var='fe_other', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="mn_dith (specified by al_fe_units)", var='mn_dith', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="mn_ox (specified by al_fe_units)", var='mn_ox', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="mn_other (specified by al_fe_units)", var='mn_other', type='value',
         unit='al_fe_units', method='al_fe_method', dataframe='sample', class='numeric'),
    list(header="al_fe_units (extract_units)", var='(^al_)|(^fe_)|(^mn_)', type='unit',
         dataframe='sample', class='factor'),
    list(header="al_fe_method",var='(^al_)|(^fe_)|(^mn_)', type='method',
         dataframe='sample', class='character'),
    #### bc
    list(header="ca_al (specified by bc_units)", var='ca_al', type='value',
         unit='bc_units', method='bc_method', dataframe='sample', class='numeric'),
    list(header="ca_ext (specified by bc_units)", var='ca_ext', type='value',
         unit='bc_units',  method='bc_method', dataframe='sample', class='numeric'),
    list(header="k_ext (specified by bc_units)", var='k_ext', type='value',
         unit='bc_units',  method='bc_method', dataframe='sample', class='numeric'),
    list(header="mg_ext (specified by bc_units)", var='mg_ext', type='value',
         unit='bc_units',  method='bc_method', dataframe='sample', class='numeric'),
    list(header="na_ext (specified by bc_units)", var='na_ext', type='value',
         unit='bc_units',  method='bc_method', dataframe='sample', class='numeric'),
    list(header="bc_units (extract_units)", var='(^ca_)|(^k_)|(^mg_)|(^na_)', type='unit',
         dataframe='sample', class='factor'),
    list(header="bc_method", var= '(^ca_)|(^k_)|(^mg_)|(^na_)', type='method',
         dataframe='sample', class='character'),
    #### cec_h
    list(header = "base_sum (specified by cec_h_units)", var='base_sum',
         unit='cec_h_units', type='value', dataframe='sample', class='numeric'),
    list(header = "cec_sum (specified by cec_h_units)", var='cec_sum',
         unit='cec_h_units', type='value', dataframe='sample', class='numeric'),
    list(header = "ecec (specified by cec_h_units)", var='ecec',
         unit='cec_h_units', type='value', dataframe='sample', class='numeric'),
    list(header="cec_h_units (extract_units)", var='(base_sum)|(cec_sum)|(ecec)',
         type='unit', dataframe='sample', class='numeric'),
    list(header="bs (percent)", var='bs', type='value', unit='percent', dataframe='sample',
         class='numeric'),
    list(header="bs_sum (percent)", var='bs_sum', type='value', unit='percent',
         dataframe='sample', class='numeric'),
    #### metal_ext
    list(header="h_ext (specified by metal_ext_units)", var='h_ext',
         type='value', unit='metal_ext_units', dataframe='sample',
         method="metal_ext_method", class='numeric'),
    list(header="zn_ext (specified by metal_ext_units)", var='zn_ext',
         type='value', unit='metal_ext_units', dataframe='sample',
         method="metal_ext_method", class='numeric'),
    list(header="metal_ext_units (extract_units)", var='(h_ext)|(zn_ext)',
         type='unit', dataframe='sample', class='factor'),
    list(header="metal_ext_method",  var='(h_ext)|(zn_ext)',
         type='method', dataframe='sample', class='character'),
    ###P
    list(header="p_bray (specified by p_units)", var='p_bray',
         type='value', unit='p_units', method='p_method', dataframe='sample',
         class='numeric'),
    list(header="p_ox (specified by p_units)", var='p_ox',
         type='value', unit='p_units', method='p_method', dataframe='sample',
         class='numeric'),
    list(header="p_meh (specified by p_units)", var='p_meh',
         type='value', unit='p_units', method='p_method', dataframe='sample',
         class='numeric'),
    list(header="p_other (specified by p_units)", var='p_other',
         type='value', unit='p_units', method='p_method', dataframe='sample',
         class='numeric'),
    list(header="p_units (extract_units)", var='^p_',
         type='unit', dataframe='sample', class='factor'),
    list(header="p_method", var='^p_',
         type='method', dataframe='sample', class='character'),
    ###Roots
    list(header="root_quant_size", var='root_weight', class='character', type='method', dataframe='sample'),
    list(header="root_weight (g)", var='root_weight', type='value',
         unit='g', dataframe='sample', class='numeric'),
    ###isotopes
    list(header="15n (‰)", var='15n', type='value', unit='permille', dataframe='sample',
         class='numeric'),
    list(header="13c (‰)", var='13c', type='value', unit='permille', dataframe='sample',
         class='numeric'),
    list(header="14c (‰)", var='14c', type='value', unit='permille', dataframe='sample',
         class='numeric'),
    list(header="14c_sigma (‰)", var='14c', type='sigma', unit='permille',
         dataframe='sample', class='numeric'),
    list(header="14c_age (BP)", var='14c_age', type='value', unit='BP', dataframe='sample',
         class='numeric'),
    list(header="14c_age_sigma (BP)", var='14c_age', type='sigma', unit='BP',
         dataframe='sample', class='numeric'),
    list(header='fraction_modern', var='fraction_modern', type='value', class='numeric', dataframe='sample'),
    list(header="fraction_modern_sigma", var='fraction_modern', type='sigma', dataframe='sample',
         class='numeric'),
    ###texture
    list(header="textureClass", var='textureClass', dataframe='field', class='factor'),
    list(header="locator_parent_alias", var='locator_parent_alias', dataframe='field',
         class='factor'))

  #### Only read in variables of interest ####
  if(!is.null(loadVars)){
    ISCNKey <- ISCNKey %>%
      filter(dataframe == 'sample', var %in% loadVars)
  }

  #### Fill in the regular expression variables ####
  unitVars <- filter(ISCNKey, type == 'value')$var

  ISCNKey <- ISCNKey %>%
    group_by(header, dataframe, class, type, unit, method) %>%
    do((function(xx){
      if(grepl('\\^|\\|', xx$var)) #check for regular expression
        #return all variables that match
        return(data.frame(var=as.character(unitVars[grepl(xx$var, unitVars)]), stringsAsFactors=FALSE))
      else
        #do nothing
        return(data.frame(var=xx$var, stringsAsFactors=FALSE))
    })(.)) %>%
    group_by(header, dataframe, class, type, unit, method, var) %>%
    arrange(var) %>%
    ##seperate the unit type as either hard coded (hardUnit) or references (unitCol) based on
    ##...starting match
    mutate(hardUnit = if_else(any(grepl(paste0('^',unit), ISCNKey$header)), as.character(NA), unit),
           unitCol =  if_else(any(grepl(paste0('^',unit), ISCNKey$header)),
                              ISCNKey$header[grepl(paste0('^',unit), ISCNKey$header)][1],
                              as.character(NA))) %>%
    ungroup()

  if(onlyISCNKey) return(ISCNKey)

  #### Read data files ####
  if(verbose) print(paste('Maybe go get a cup of coffee... this takes a while.\nGet file names, looking for csv files in layersDir:', layersDir))
  files.arr <- list.files(path=layersDir, pattern='\\.csv$', full.names=TRUE)

  ans <- list(study=data.frame(),
              field=data.frame(),
              sample=data.frame(),
              measure=data.frame())
  for(fileNum in 1:length(files.arr)){
    if(verbose) print(paste('Reading', files.arr[fileNum]))

    #actualHeaders <- read_csv(file=files.arr[fileNum],
    #                          n_max=1, col_names=FALSE, col_types=paste0(rep('c', 95), collapse=''))

    all.temp <-
      read_csv(file=files.arr[fileNum], col_types = cols(.default = "c")) %>%
      filter(!is.na(dataset_name_sub)) %>% #remove empty lines
      mutate(rowNum = 1:nrow(.)) ##Adding row numbers because dataset_name_soc breaks the soc variable,
    ##...the layer name is no longer a unique row identifier

    ##Pull the formal name for the ISCN data provider version
    datasetName <- as.character(names(all.temp)[1])
    #all.temp %>% select(-contains(datasetName)) %>% mutate(dataset_provider = datasetName)

    ans$study <- all.temp %>%
      select(one_of(intersect(names(all.temp), (ISCNKey %>% filter(dataframe == 'study'))$header))) %>%
      rename(dataset_name = dataset_name_sub) %>% ##Key the study ids on the dataset_name
      mutate(dataset_name_super = datasetName) %>%
      unique %>%
      bind_rows(ans$study)

    ans$field <- all.temp %>%
      rename(dataset_name = dataset_name_sub) %>%
      select(dataset_name,##pull the study IDs... and all the field variables
             one_of(intersect(names(all.temp), (ISCNKey %>% filter(dataframe == 'field'))$header))) %>%
      unique %>%
      bind_rows(ans$field)

    ans$sample <- all.temp %>%
      rename(dataset_name = dataset_name_sub) %>%
      select(dataset_name, layer_name, ##pull the study and field IDS... and all the sample variables
             rowNum,
             one_of(intersect(names(all.temp), (ISCNKey %>% filter(dataframe == 'sample'))$header))) %>%
      gather(header, entry, ##make the samples long table format
             one_of(intersect(names(all.temp), (ISCNKey %>% filter(dataframe == 'sample'))$header)),
             na.rm=TRUE) %>%
      left_join(select(ISCNKey, header, var, type), by=c('header')) %>% ##trace all headers to a variable by their type
      group_by(dataset_name, layer_name, rowNum, var) %>% ##for each variable
      spread(type, entry) %>% ##spread out the method, unit, value, or sigma associated with it
      filter(!all(is.na(value))) %>% #remove NA data
      summarize(method=paste0(paste(header, method, sep=':')[!is.na(method)],
                              collapse=';'), #glom multiple methods together
                unit = ifelse(all(is.na(unit)), as.character(NA), unit[!is.na(unit)]) ,
                value = ifelse(all(is.na(value)), as.numeric(NA), as.numeric(value[!is.na(value)]))) %>%
      ungroup() %>%
      select(dataset_name, layer_name, var, method, unit, value) %>%
      unique() %>%
      bind_rows(ans$sample)

  }
  rm(all.temp)

  ####Rename the headers for field####
  ##TODO key.ls <- rename_(df, .dots = setNames(names(key.ls), key.ls))
  #     rename_(df, .dots = setNames(names(key.ls), key.ls))
  renameNonSampleHeaders <- ISCNKey %>% filter(dataframe != 'sample', header != var)
  renameNonSampleHeaders.ls <- as.list(renameNonSampleHeaders$var)
  names(renameNonSampleHeaders.ls) <- renameNonSampleHeaders$header

  names(ans$field)[names(ans$field) %in% names(renameNonSampleHeaders.ls)] <-
    renameNonSampleHeaders.ls[names(ans$field)[names(ans$field) %in% names(renameNonSampleHeaders.ls)]]

  #### Add field and measure IDs and reindex samples to save space####
  if(verbose) print('Adding field/measure IDs and factoring')
  ans$field <- ans$field %>%
    mutate_at(vars(one_of(intersect(unique(ISCNKey$var[ISCNKey$class == 'factor']), names(ans$field)))),
              funs(factor))%>% ##Need to rename headers as var
    arrange(dataset_name) %>%
    mutate(fieldID = 1:nrow(.))

  ans$measure <- ans$sample %>% ungroup() %>%
    select(var, method, unit) %>% unique %>%
    arrange(var) %>%
    mutate(measureID = 1:nrow(.))

  ans$sample <- ans$sample %>%
    left_join(select(ans$field, dataset_name, layer_name, fieldID),
              by=c('dataset_name', 'layer_name')) %>%
    left_join(ans$measure, by=c('var', 'method', 'unit')) %>%
    select(fieldID, measureID, value)

  #### read in meta files ####
  if(verbose) print('reading in meta files')
  ans$study <- readxl::read_excel(path=paste(metaDir, 'ISCN_ALL-DATA-CITATION_1-1.xlsx', sep='/'),
                                   sheet='citation') %>%
      full_join(readxl::read_excel(path=paste(metaDir, 'ISCN_ALL_DATA_DATASET_1-1.xlsx', sep='/'),
                                   sheet='dataset')) %>%
      mutate(`modification_date (YYYY-MM-DD)` =
               as.POSIXct(round(`modification_date (YYYY-MM-DD)`, unit='day'))) %>%
      select(-`ISCN 1-1 (2015-12-10)`) %>%
      group_by(dataset_name) %>%
      gather(header, value, -dataset_name,na.rm=TRUE) %>% unique() %>%
      full_join(ans$study) %>%
      select(dataset_name_super, dataset_name, header, value) %>%
      arrange(dataset_name_super, dataset_name, header)

  ####Rename the headers for the study df####
  renameNonSampleHeaders <- ISCNKey %>% filter(dataframe != 'sample', header != var)
  renameNonSampleHeaders.ls <- as.list(renameNonSampleHeaders$var)
  names(renameNonSampleHeaders.ls) <- renameNonSampleHeaders$header

  names(ans$study)[names(ans$study) %in% names(renameNonSampleHeaders.ls)] <-
    renameNonSampleHeaders.ls[names(ans$study)[names(ans$study) %in% names(renameNonSampleHeaders.ls)]]

    ans$ISCNKey <- ISCNKey

  return(ans)
}
