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
#' @importFrom magrittr %>%
#' @imports dplyr
#' @importFrom tidyr gather spread
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @export
processData_ISCN3 <- function(layersDir=NULL, metaDir=NULL,
                              keyFile=NULL,
                              verbose=FALSE, onlyISCNKey=FALSE, loadVars=NULL){

  # debug.ls <- list(layersDir = '../soils-long-tail-recovery/repoData/ISCN_3/Layers',
  #                  metaDir = '../soils-long-tail-recovery/repoData/ISCN_3/Meta/',
  #                 keyFile = '../soils-long-tail-recovery/repoData/ISCN_3/ISCNKey.xlsx',
  #                  verbose = TRUE, onlyISCNKey=FALSE,
  #                 loadVars = c("14c_age"))
  # attach(debug.ls)

  ##TODO move to package::function notation
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)

  #### Make ISCN Key ####
  ISCNKey <- read_excel(path=keyFile, sheet='headerKey')

  #### Fill in the regular expression variables ####
  unitVars <- filter(ISCNKey, type == 'value')$var

  ISCNKey <- ISCNKey %>%
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
    mutate(hardUnit = if_else(any(grepl(paste0('^',unit), ISCNKey$header)), as.character(NA), unit),
           unitCol =  if_else(any(grepl(paste0('^',unit), ISCNKey$header)),
                              ISCNKey$header[grepl(paste0('^',unit), ISCNKey$header)][1],
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
  if(verbose) print(paste('Maybe go get a cup of coffee... this takes a while.\nGet file names, looking for excel files in layersDir:', layersDir))
  files.arr <- list.files(path=layersDir, pattern='\\.csv$', full.names=TRUE)

  #files.excel <- list.files(path=layersDir, pattern='^ISCN_ALL_DATA_LAYER_C.*\\.xlsx', full.names=TRUE)

  ans <- list(study=data.frame(),
              field=data.frame(),
              sample=data.frame(),
              measure=data.frame())
  for(fileNum in 1:length(files.arr)){
    if(verbose) print(paste('Reading', files.arr[fileNum]))

    #actualHeaders <- read_csv(file=files.arr[fileNum],
    #                          n_max=1, col_names=FALSE, col_types=paste0(rep('c', 95), collapse=''))

    all.temp <- read_csv(file=files.arr[fileNum], col_types = cols(.default = "c")) %>%
      #read_excel(path=files.excel[fileNum], sheet='layer') %>% ##Dates are not delt with well still
      filter(!is.na(dataset_name_sub)) %>% #remove empty lines
      mutate(rowNum = 1:nrow(.)) ##Adding row numbers because dataset_name_soc breaks the soc variable,
    ##...the layer name is no longer a unique row identifier

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

  return(ans)
}
