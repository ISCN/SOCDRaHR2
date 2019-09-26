#' Load ISCN Layer and Meta data
#'
#' This function first downloads the layer, profile, citaiton, and dataset tables from the ISCN website (http://iscn.fluxdata.org/data/access-data/database-reports/) data available: ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/* It then either returns the orginal structure or reformats the data.
#'
#' @param dataDir path to the folder contianing ISCN_ALL_DATA_LAYER_C*_1-1.xlsx, ISCN_ALL-DATA-CITATION_1-1.xlsx and ISCN_ALL_DATA_DATASET_1-1.xlsx files. If this is left NULL then files will be downloaded to a temporary directory from the ISCN website and then deleted.
#' @param verbose boolean flag denoting whether or not to print lots of status messages
#' 
#' @return list of data.table with layer, dataset and citation information
#'
#' @importFrom data.table rbindlist
#' @importFrom readxl read_excel
#' @importFrom tibble tibble
#' @importFrom utils download.file
#' @export
#' 
ISCN3 <- function(dataDir=NULL, orginalFormat = FALSE, verbose=FALSE){
  
  ## construct file paths ####
  delete_dataDir <- is.null(dataDir)
  if(is.null(dataDir)){
    dataDir <- tempdir()
  }
  
  layerDataFiles.arr <- file.path(dataDir, c('ISCN_ALL_DATA_LAYER_C1_1-1.xlsx', 
                                             'ISCN_ALL_DATA_LAYER_C2_1-1.xlsx',
                                             'ISCN_ALL_DATA_LAYER_C3_1-1.xlsx', 
                                             'ISCN_ALL_DATA_LAYER_C4_1-1.xlsx'))
  
  profileDataFiles.arr <- file.path(dataDir, c('ISCN_ALL-DATA_PROFILE_1-1.xlsx'))
  dataFiles.arr <- c(layerDataFiles.arr, 
                     profileDataFiles.arr,
                         file.path(dataDir, c(
                                            'ISCN_ALL-DATA-CITATION_1-1.xlsx', 
                                            'ISCN_ALL_DATA_DATASET_1-1.xlsx')))
  
  ## Download the data ####
  if(verbose) print('Download file.')
  
  for(dataFiles.arr in dataFiles.arr){
    if(!file.exists(dataFiles.arr)){
      utils::download.file(sprintf('ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/%s', basename(dataFiles.arr)), 
                    dataFiles.arr, quiet=FALSE)
    }
  }

  ## Read data files ####
  
  if(verbose) print('Meta data read in.')
  
  citation.dt <- data.table::data.table(readxl::read_excel(path=paste(dataDir, 
                                                          'ISCN_ALL-DATA-CITATION_1-1.xlsx', sep='/'),
                                    sheet='citation', col_types = 'text'))
  
  dataset.dt <- data.table::data.table(readxl::read_excel(path=paste(dataDir, 
                                                         'ISCN_ALL_DATA_DATASET_1-1.xlsx', sep='/'), 
                                   sheet='dataset', col_types = 'text'))
  
  if(verbose) print('Profile data read in.')
  #only one profile data sheet
  profile.dt <- data.table::data.table(readxl::read_excel(path=profileDataFiles.arr[1], 
                                                          sheet='profile', col_types='text'))
  
  if(verbose) print('Layer data read in.')

  layer.dt <- data.table::rbindlist(lapply(layerDataFiles.arr, 
                                                function(xx){
                                  readxl::read_excel(path=xx, sheet='layer', col_types='text')}))
  
  ##add collection level details like citation
  collection.dt <- data.table::data.table(collection_name_id = 'ISCN3', 
                                                   variable = 'collection_citation',
                                                   entry = 'Nave L, Johnson K, van Ingen C, Agarwal D, Humphrey M, Beekwilder N. 2017. International Soil Carbon Network (ISCN) Database, Version 3. DOI: 10.17040/ISCN/1305039. Database Report: ISCN_SOC-DATA_LAYER_1-1. Accessed 2 February 2017',
                                                   type = 'value')
  keys.ls <- makeKeys()
  
  if(orginalFormat){
    
    return(list(citation=citation.dt, dataset=dataset.dt, profile = profile.dt, layer = layer.dt, 
                key=keys.ls$ISCN3, 
                collection = collection.dt))
  }else{
    ans <- formatLongTable(list(citation=citation.dt, 
                                dataset=dataset.dt, profile = profile.dt, layer = layer.dt),
                           sourceKey = keys.ls$ISCN3, targetKey=keys.ls$ISCN)
    
    #### Clean up ####
    hardKeys <- keys.ls$ISCN3[!is.na(entry), c('variable', 'type', 'entry')]
    hardKeys[,collection_name_id := factor('ISCN3')]
    ans$collection <- data.table::rbindlist(list(collection.dt, hardKeys), fill = TRUE)
    
    ans$study[, collection_name_id := factor('ISCN3')]
    ans$study <- ans$study[!is.na(entry)]
    
    ans$profile[, collection_name_id := as.factor('ISCN3')]
    ans$profile <- ans$profile[!is.na(entry)]
    
    ans$layer[, collection_name_id := as.factor('ISCN3')]
    ans$layer <- ans$layer[!is.na(entry)]
    
    return(ans)
  }

}
