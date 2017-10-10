#' Load ISCN vs 3 Layer data
#'
#' ISCN vs 3 (http://iscn.fluxdata.org/data/access-data/database-reports/) data available: ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C1_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C2_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C3_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C4_1-1.xlsx
#'
processData_ISCN3 <- function(dir=NULL, verbose=FALSE, onlyPullKey=FALSE){

  library(dplyr)
  library(tidyr)

  #### Make ISCN Key ####
  ISCNKey <- bind_rows( ##Sub-Study data
    list(header="ISCN 1-1 (2015-12-10)", var='dataset_name', dataframe = 'study',
         class='factor'),
    list(header="dataset_name_sub", var='dataset_name_sub', dataframe='study',
         class='factor'),
    list(header="dataset_name_soc", var='dataset_name_soc', class='factor'), ##include in soc methods
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
    list(header="bd_other (g cm-3)", var='bd_other', type='value', unit='g cm=3',
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
    list(header="root_quant_size", var='root_quant_size', class='character'), ##TODO
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
    list(header='fraction_modern', var='fraction_modern', class='numeric'),
    list(header="fraction_modern_sigma", var='fraction_modern', type='sigma',
         class='numeric'),
    ###texture
    list(header="textureClass", var='textureClass', dataframe='field', class='factor'),
    list(header="locator_parent_alias", var='locator_parent_alias', dataframe='field',
         class='factor'))

  ##Be very careful changing any of this since it was coded by hand, the permill ascii
  ##... is throwing off some of the dplyr functions
  ISCNKey <- ISCNKey %>%
    mutate(colIndex = 1:nrow(ISCNKey),
           #Create a safe header name
           headerName = if_else(grepl('^\\d', header), sprintf('X%s',#put x's on digits
                                                               gsub('_+$', '',
                                                                    gsub('\\W', '_',
                                                                         header))),
                                gsub('_+$', '', gsub('\\W', '_', header))))
  #replace non alnum w/ _
  defaultDatasetName <- as.character(ISCNKey[1, 'header'])
  ISCNKey[1, 'headerName'] <- 'dataset_name'

  ###Collapse variables, methods and units ###
  SMU.df <- ISCNKey %>%
    filter(dataframe == 'sample' & type == 'value') %>%
    select(header, headerName, var, method, unit) %>%
    group_by(var) %>%
    mutate(methodCount = length(unlist(strsplit(method, '|', fixed=TRUE)))) %>%
    mutate(method.headerName.1 = gsub('\\(|\\)', '',
                                      if_else(methodCount == 1, method,
                                              unlist(strsplit(method, '|',
                                                              fixed=TRUE))[1])),
           method.headerName.2 = gsub('\\(|\\)', '',
                                      if_else(methodCount == 1, as.character(NA),
                                              unlist(strsplit(method, '|',
                                                              fixed=TRUE))[2])),
           unit.headerName = if_else(sum(grepl(sprintf('^%s', unit),
                                               ISCNKey$headerName)) == 0,
                                     as.character(NA),
                                     paste0('', ISCNKey$headerName[grepl(
                                       sprintf('^%s', unit),
                                       ISCNKey$headerName)])))
  #Hack: no idea why we need paste on unit.headerNaem.
  #...Was throwing a length ==0 error otherwise

  simpleUnit.df <- SMU.df %>%
    filter(is.na(unit.headerName)) %>%
    select(var, unit) %>%
    rename(simpleUnit=unit)


  #### files to read in ####
  if(verbose) print('Get file names (looking for csv files in dir)')
  files.arr <- list.files(path=dir, pattern='\\.csv$', full.names=TRUE)

  for(fileNum in 1:length(files.arr)){
    if(verbose) print(paste('Reading', files.arr[fileNum]))

    #### read in files ####

    test <- read.csv(file=files.arr[fileNum], stringsAsFactors=FALSE)
    test[,ISCNKey$colIndex[ISCNKey$class == 'numeric']] <-
      lapply(test[,ISCNKey$colIndex[ISCNKey$class == 'numeric']], as.numeric)
    test[,ISCNKey$colIndex[ISCNKey$class == 'factor']] <- ##Note cast the factors for nice merging
      lapply(test[,ISCNKey$colIndex[ISCNKey$class == 'factor']], as.character)
    test[,ISCNKey$colIndex[ISCNKey$class == 'character']] <-
      lapply(test[,ISCNKey$colIndex[ISCNKey$class == 'character']], as.character)
    names(test) <- ISCNKey$headerName[ISCNKey$colIndex]

    if(verbose) print('Constructing IDs')
    ##fill with pervious values and add keys
    test <- test %>%
      fill_(filter(ISCNKey, dataframe == 'study')$headerName) %>%
      mutate(dataset_name=if_else(is.na(dataset_name), defaultDatasetName,
                                  as.character(dataset_name))) %>%
      mutate(studyName=sprintf('%s - %d - %s',
                             dataset_name, fileNum, dataset_name_sub),
             fieldName=sprintf(' %s - %s - %s',
                             site_name, profile_name, layer_name),
             sampleID=sprintf('%d - %.6d', fileNum, 1:nrow(test)))

    if(verbose) print('Create study dataframe')
    ####Create study dataframe ####
    study.df <- test[c('studyName',
                       filter(filter(ISCNKey, dataframe == 'study'))$headerName)] %>%
      unique %>%
      mutate(studyID = 1:length(studyName) +
               ifelse(fileNum == 1, 0, ceiling(max(ans$study$studyID))),
             sheetID = fileNum)

    if(verbose) print('Create field dataframe')
    ####Create field dataframe ####
    field.df <- test[,c('studyName', 'fieldName',
                        filter(ISCNKey, dataframe == 'field')$headerName)] %>%
      unique %>%
      left_join(study.df[,c('studyName', 'studyID')], by='studyName') %>%
      mutate(fieldID = 1:length(fieldName) +
               ifelse(fileNum == 1, 0, ceiling(max(ans$field$fieldID))))

    ##put the fieldNum and studyNum into test
    test <- test %>%
      left_join(field.df[,c('fieldName', 'studyName', 'fieldID', 'studyID')],
                by=c('studyName', 'fieldName'))

    if(verbose) print('Create methods dataframe')
    method1.df <- test[,c('studyID', 'fieldID', 'sampleID',
                          filter(ISCNKey, dataframe == 'sample' &
                                   type=='method')$headerName)] %>%
      gather(headerName, methodStr, -studyID, -fieldID, -sampleID, na.rm=TRUE) %>%
      filter(grepl('\\S', methodStr)) %>%
      rename(method.headerName.1=headerName) %>%
      inner_join(select(SMU.df, var, method.headerName.1), by='method.headerName.1') %>%
      rename(methodStr.1=methodStr)

    method2.df <- test[,c('studyID', 'fieldID', 'sampleID',
                          filter(ISCNKey, dataframe == 'sample' &
                                   type=='method')$headerName)] %>%
      gather(headerName, methodStr, -studyID, -fieldID, -sampleID, na.rm=TRUE) %>%
      filter(grepl('\\S', methodStr)) %>%
      rename(method.headerName.2=headerName) %>%
      inner_join(select(SMU.df, var, method.headerName.2), by='method.headerName.2') %>%
      rename(methodStr.2=methodStr)

    method.df <- method1.df %>%
      full_join(method2.df, by=c('studyID', 'fieldID', 'sampleID', 'var')) %>%
      mutate(method = if_else(is.na(method.headerName.2),
                              paste0(method.headerName.1, ': ', methodStr.1),
                              if_else(is.na(method.headerName.1),
                                      paste0(method.headerName.2, ': ', methodStr.2),
                                      paste0(method.headerName.1, ': ', methodStr.1, ' - ',
                                            method.headerName.2, ': ', methodStr.2)))) %>%
      select(contains('ID'), var, method)


    if(verbose) print('Create units dataframes')
    unit.df <- test[,c('studyID', 'fieldID', 'sampleID',
                       filter(ISCNKey, dataframe == 'sample' &
                                type=='unit')$headerName)] %>%
      gather(headerName, unitStr, -studyID, -fieldID, -sampleID, na.rm=TRUE) %>%
      filter(grepl('\\S', unitStr)) %>%
      rename(unit.headerName=headerName) %>%
      inner_join(select(SMU.df, var, unit.headerName), by='unit.headerName') %>%
      select(-unit.headerName) %>%
      rename(unit=unitStr) %>%
      select(contains('ID'), var, unit)

    #if(verbose) print('Create sigma dataframes')
    ##Nothing ehre
    #sigma.df <- test[,c('studyID', 'fieldID', 'sampleID',
    #                    filter(ISCNKey, dataframe == 'sample' &
    #                             type=='sigma')$headerName)] %>%
    #  unique %>%
    #  gather(headerName, sigma, -studyID, -fieldID, -sampleID, na.rm=TRUE) %>%
    #  left_join(ISCNKey[,c('headerName', 'var')], by='headerName') %>%
    #  select(-headerName)


    if(verbose) print('Create SOC var-value-method dataframe')
    ##deal with SOC first and append all BD/OC methods so we can reconstruct SOC calcs
    soc_methods_headers <- c('dataset_name_soc',
                             filter(ISCNKey, dataframe == 'sample',
                                  type=='method',
                                  grepl('(soc)|(bd_)|(^oc$)|(^c_tot$)', var))$headerName)
    sample_SOC.df <- test[, c('studyID', 'fieldID', 'sampleID', soc_methods_headers,
                     'soc__g_cm_2')] %>%
      filter(is.finite(soc__g_cm_2)) %>%
      unique %>%
      group_by(studyID, fieldID, sampleID, soc__g_cm_2) %>%
      gather(method, string,
             dataset_name_soc, bd_method, bdNRCS_prep_code,
             cNRCS_prep_code, c_method, soc_carbon_flag, soc_method, na.rm=TRUE) %>%
      filter(method == 'soc_method' | (method != 'soc_method' & grepl('\\S', string))) %>%
      mutate(method.str = paste0(method, ': ', string)) %>%
      summarise(method = paste(method.str, collapse=' - ')) %>%
      mutate(var='soc') %>%
      rename(value=soc__g_cm_2)

    if(verbose) print('Create full sample dataframe')
    sample.df <- test[,c('studyID', 'fieldID', 'sampleID',
                         filter(ISCNKey, dataframe == 'sample' &
                                  type=='value' &
                                  #deal with SOC seperately
                                  !grepl('^soc$', var))$headerName)] %>%
      unique %>%
      gather(headerName, value, -studyID, -fieldID, -sampleID, na.rm=TRUE) %>%
      left_join(ISCNKey[,c('headerName', 'var')], by='headerName') %>%
      select(-headerName) %>%
      left_join(method.df, by=c('studyID', 'fieldID', 'sampleID', 'var')) %>%
      bind_rows(sample_SOC.df) %>% ##add back in the SOC that had seperate methods above
      left_join(unit.df, by=c('studyID', 'fieldID', 'sampleID', 'var')) %>%
      #left_join(sigma.df, by=c('studyID', 'fieldID', 'sampleID', 'var')) %>%
      left_join(simpleUnit.df, by='var') %>%
      mutate(unit = if_else(is.na(unit), simpleUnit, unit)) %>%
      select(-simpleUnit, -sampleID) %>%
      unique

    if(fileNum == 1){
      if(verbose) print('Create inital answer list')
      ans <- list(sample=sample.df, field=field.df, study=study.df, ISCNKey)
    }else{
      if(verbose) print('Append to answer list')
      ans <- list(sample=bind_rows(ans$sample, sample.df),
                  field =bind_rows(ans$field, field.df),
                  study =bind_rows(ans$study, study.df),
                  key = ISCNKey)
    }
  }


  ##Cast things as factors
  ans$sample[,c('var', 'unit', 'method')] <-
    lapply(ans$sample[,c('var', 'unit', 'method')], as.factor)

  ans$field[,filter(ISCNKey, dataframe=='field' & class == 'factor')$headerName] <-
    lapply(ans$field[, filter(ISCNKey, dataframe=='field' &
                                class == 'factor')$headerName], as.factor)

  ans$study[,filter(ISCNKey, dataframe=='study' &
                      class == 'factor')$headerName] <-
    lapply(ans$study[,filter(ISCNKey, dataframe=='study' &
                               class == 'factor')$headerName], as.factor)


  ans$measure <- unique(ans$sample[,c('var', 'method', 'unit')])
  ans$measure$measureID <- 1:nrow(ans$measure)

  ans$field <- ans$field %>% select(-fieldName, -studyName) %>%
    mutate(studyID = as.integer(studyID),
           fieldID=as.integer(fieldID))
  ans$study <- ans$study %>% select(-studyName) %>%
    mutate(studyID = as.integer(studyID))

  ans$sample <- ans$sample %>%
    left_join(ans$measure, by=c('var', 'method', 'unit')) %>%
    select(-var, -method, -unit, -studyID) %>%
    mutate(fieldID = as.integer(fieldID))

  return(ans)
}
