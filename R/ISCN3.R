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
  
  if(verbose) print('Meta data read in.')
  
  citation.dt <- data.table::data.table(readxl::read_excel(path=paste(dataDir, 
                                                          'ISCN_ALL-DATA-CITATION_1-1.xlsx', sep='/'),
                                    sheet='citation'))
  
  dataset.dt <- data.table::data.table(readxl::read_excel(path=paste(dataDir, 
                                                         'ISCN_ALL_DATA_DATASET_1-1.xlsx', sep='/'), 
                                   sheet='dataset'))
  
  if(verbose) print('Layer data read in.')

  layer.dt <- data.table::rbindlist(lapply(layerDataFiles.arr, 
                                                function(xx){
                                  readxl::read_excel(path=xx, sheet='layer', col_types='text')}))
  
  
  return(list(citation=citation.dt, dataset=dataset.dt, layer = layer.dt))

}
