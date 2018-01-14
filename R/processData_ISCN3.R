#' Load ISCN Layer and Meta data
#'
#' This function first downloads the layer and meta data from the ISCN website.
#' ISCN (http://iscn.fluxdata.org/data/access-data/database-reports/) data available: ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C1_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C2_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C3_1-1.xlsx ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/ISCN_ALL_DATA_LAYER_C4_1-1.xlsx
#'
#' @param layersDir path to the folder contianing ISCN_ALL_DATA_LAYER_C*_1-1.excel files. If this is left Null then files will be downloaded to a temporary directory and then deleted.
#' @param metaDir path to the folder contianing ISCN_ALL-DATA-CITATION_1-1.xlsx and ISCN_ALL_DATA_DATASET_1-1.xlsx files. If this is left Null then files will be downloaded to a temporary directory and then deleted.
#' @param verbose boolean flag denoting whether or not to print lots of status messages
#' @param onlyISCNKey boolean flag to only return the processed ISCN3 key which is slightly different then SoilDataR::ISCNKey.df
#' @param loadVars an array of characters to read in only certain variables
#'
#' @import dplyr
#' @importFrom tidyr gather spread
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @export
processData_ISCN3 <- function(layersDir=NULL, metaDir=NULL,
                              verbose=FALSE, onlyISCNKey=FALSE, loadVars=NULL){
  
  ## create the layer and meta directors if needed
  delete_layersDir <- is.null(layersDir)
  if(is.null(layersDir)){
    layersDir <- tempdir()
  }
  
  delete_metaDir <- is.null(metaDir)
  if(is.null(metaDir)){
    metaDir <- tempdir()
  }
  
  layerDataFiles.arr <- file.path(layersDir, c('ISCN_ALL_DATA_LAYER_C1_1-1.xlsx', 'ISCN_ALL_DATA_LAYER_C2_1-1.xlsx',
                                               'ISCN_ALL_DATA_LAYER_C3_1-1.xlsx', 'ISCN_ALL_DATA_LAYER_C4_1-1.xlsx'))
  metaDataFiles.arr <- file.path(metaDir,  c('ISCN_ALL-DATA-CITATION_1-1.xlsx', 'ISCN_ALL_DATA_DATASET_1-1.xlsx'))
  
  ## Download the layer data
  for(layerDataFile in layerDataFiles.arr){
    if(!file.exists(layerDataFile)){
      download.file(sprintf('ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/%s', basename(layerDataFile)), 
                    layerDataFile, quiet=FALSE)
    }
  }
  
  # Download meta data
  for(metaDataFile in metaDataFiles.arr){
    if(!file.exists( metaDataFile)){
      download.file(sprintf('ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/%s', basename(metaDataFile)), 
                    metaDataFile, quiet=FALSE)
    }
  }
  
  #### Fill in the regular expression variables ####
  unitVars <- dplyr::filter(SoilDataR::ISCNKey.df, type == 'value')$var

  ISCNKey <- SoilDataR::ISCNKey.df %>%
    group_by(header, dataframe, class, type, unit, method) %>%
    do((function(xx){
      if(grepl('(\\^)|(\\|)|(\\$)', xx$var)) #check for regular expression
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
    mutate(hardUnit = if_else(any(grepl(paste0('^',unit), SoilDataR::ISCNKey.df$header)), as.character(NA), unit),
           unitCol =  if_else(any(grepl(paste0('^',unit), SoilDataR::ISCNKey.df$header)),
                             SoilDataR::ISCNKey.df$header[grepl(paste0('^',unit),SoilDataR::ISCNKey.df$header)][1],
                              as.character(NA))) %>%
    ungroup()

  if(onlyISCNKey) return(ISCNKey)


  #### Only read in variables of interest ####
  if(!is.null(loadVars)){
    ISCNKey <- ISCNKey %>%
      filter((dataframe == 'sample' & var %in% loadVars) |
               dataframe != 'sample')
  }


  #### Read data files ####
  if(verbose) print('Maybe go get a cup of coffee... this takes a while.')

  ans <- list(study=data.frame(),
              field=data.frame(),
              sample=data.frame(),
              measure=data.frame())
  for(fileNum in 1:length(layerDataFiles.arr)){
    if(verbose) print(paste('Reading', layerDataFiles.arr[fileNum]))

    all.temp <- readxl::read_excel(path=layerDataFiles.arr[fileNum], sheet='layer', col_types='text') %>%
      filter(!is.na(dataset_name_sub)) %>% #remove empty lines
      mutate(rowNum = 1:nrow(.), ##Adding row numbers because dataset_name_soc breaks the soc variable
                                 ##...the layer name is no longer a unique row identifier
             observation_date = as.numeric(`observation_date (YYYY-MM-DD)`)) %>%
      mutate(`observation_date (YYYY-MM-DD)` = if_else(observation_date > 2020,
                                        lubridate::decimal_date(as.Date(observation_date, 
                                                                        origin = "1899-12-30")),
                                        observation_date))
    
    if(grepl('ISCN_ALL_DATA_LAYER_C1_1-1.xlsx', basename(layerDataFiles.arr[fileNum])) &
       ! floor(all.temp$`observation_date (YYYY-MM-DD)`[1]) == 2006){
      warning('Dates from ISCN3 layer files do not check out (SoilDataR::processData_ISCN3) ')
    }
    
    ##Pull the formal name for the ISCN data provider version
    datasetName <- as.character(names(all.temp)[1])
    #all.temp %>% select(-contains(datasetName)) %>% mutate(dataset_provider = datasetName)

    dropCols <- apply(all.temp, 2, function(xx){all(is.na(xx))}) ##select_if has issues with non-standard column names
    all.temp <- all.temp[,!dropCols]

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

    sampleVarNames <- intersect(names(all.temp), (ISCNKey %>% filter(dataframe == 'sample'))$header)
    if(length(sampleVarNames) > 0){
      sample <- all.temp %>%
        rename(dataset_name = dataset_name_sub) %>%
        select(dataset_name, layer_name, ##pull the study and field IDS... and all the sample variables
               rowNum, one_of(sampleVarNames)) %>%
        gather(header, entry, ##make the samples long table format
               one_of(sampleVarNames),
               na.rm=TRUE) %>%
        left_join(select(ISCNKey, header, var, type), by=c('header')) %>% ##trace all headers to a variable by their type
        group_by(dataset_name, layer_name, rowNum, var, add=FALSE) %>% ##for each variable
        spread(type, entry) %>% ##spread out the method, unit, value, or sigma associated with it
        filter(any(!is.na(value))) #remove NA data

      # mutate(unit=ifelse(exists('unit', where=.), unit, NA), ##Ensure that there are units and methods
      #        method=ifelse(exists('method', where=.), method, NA)) %>%
      ##coded in base for runtime issues
      if(!'unit' %in% names(sample)){
        sample$unit <- as.character(NA)
      }
      if(!'method' %in% names(sample)){
        sample$method <- as.character(NA)
      }

      sample <- sample %>%
        summarize(method=ifelse(all(is.na(method)), as.character(NA),
                                paste0(paste(header, method, sep=':')[!is.na(method)],
                                       collapse=';')), #glom multiple methods together
                  unit = ifelse(all(is.na(unit)), as.character(NA), unique(unit[!is.na(unit)])) ,
                  value = as.numeric(unique(value[!is.na(value)]))) %>%
        ungroup() %>%
        select(dataset_name, layer_name, var, method, unit, value) %>%
        unique()

      ans$sample <- sample %>%
        bind_rows(ans$sample)

      rm(sample)
    }
    rm(all.temp)
  }

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

  if(nrow(ans$sample) > 0){
    ans$measure <- ans$sample %>% ungroup() %>%
      select(var, method, unit) %>% unique %>%
      arrange(var) %>%
      mutate(measureID = 1:nrow(.))

    ans$sample <- ans$sample %>%
      left_join(select(ans$field, dataset_name, layer_name, fieldID),
                by=c('dataset_name', 'layer_name')) %>%
      left_join(ans$measure, by=c('var', 'method', 'unit')) %>%
      select(fieldID, measureID, value)
  }else{
    ans$measure <- data.frame()
  }
  #### read in meta files ####
  if(verbose) print('reading in meta files')
  ans$study <- read_excel(path=paste(metaDir, 'ISCN_ALL-DATA-CITATION_1-1.xlsx', sep='/'), sheet='citation') %>%
    full_join(read_excel(path=paste(metaDir, 'ISCN_ALL_DATA_DATASET_1-1.xlsx', sep='/'), sheet='dataset')) %>%
    mutate(`modification_date (YYYY-MM-DD)` = as.POSIXct(round(`modification_date (YYYY-MM-DD)`, unit='day'))) %>%
    select(-`ISCN 1-1 (2015-12-10)`) %>%
    group_by(dataset_name) %>%
    gather(header, value, -dataset_name,na.rm=TRUE) %>%
    unique() %>%
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

    #delete the files from the temepratory directorys
    if(delete_metaDir){
      if(verbose) print(paste('deleting: ', 
                              paste0(file.path(metaDir, c('ISCN_ALL-DATA-CITATION_1-1.xlsx', 'ISCN_ALL_DATA_DATASET_1-1.xlsx'),
                                               collapse=', '))))
      file.remove(file.path(metaDir, c('ISCN_ALL-DATA-CITATION_1-1.xlsx', 'ISCN_ALL_DATA_DATASET_1-1.xlsx')), recursive=TRUE)
    }
    if(delete_layersDir){
      if(verbose) print(paste('deleting: ', 
                              paste0(file.path(layersDir, c('ISCN_ALL_DATA_LAYER_C1_1-1.xlsx', 'ISCN_ALL_DATA_LAYER_C2_1-1.xlsx',
                                                           'ISCN_ALL_DATA_LAYER_C3_1-1.xlsx', 'ISCN_ALL_DATA_LAYER_C4_1-1.xlsx')),
                                     collapse=', ')))
      file.remove(file.path(layersDir, c('ISCN_ALL_DATA_LAYER_C1_1-1.xlsx', 'ISCN_ALL_DATA_LAYER_C2_1-1.xlsx',
                                   'ISCN_ALL_DATA_LAYER_C3_1-1.xlsx', 'ISCN_ALL_DATA_LAYER_C4_1-1.xlsx')), recursive=TRUE)
    }
  
  return(ans)
}
