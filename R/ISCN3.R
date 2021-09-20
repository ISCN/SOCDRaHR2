#' Load ISCN Layer and Meta data
#'
#' This function first downloads the layer, profile, citation, and dataset tables from the ISCN website (http://iscn.fluxdata.org/data/access-data/database-reports/) data available: ftp://ftp.fluxdata.org/.deba/ISCN/ALL-DATA/* It then either returns the original structure or reformats the data.
#'
#' @param dataDir path to the folder containing ISCN_ALL_DATA_LAYER_C*_1-1.xlsx, ISCN_ALL-DATA-CITATION_1-1.xlsx and ISCN_ALL_DATA_DATASET_1-1.xlsx files. If this is left NULL then files will be downloaded to a temporary directory from the ISCN website and then deleted.
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
  
  layerDataFiles.arr <- file.path(dataDir, c('ISCN3_layer.csv'))
  
  profileDataFiles.arr <- file.path(dataDir, c('ISCN3_profile.csv'))
  dataFiles.arr <- c(layerDataFiles.arr, 
                     profileDataFiles.arr,
                         file.path(dataDir, c(
                                            'ISCN3_citation.csv', 
                                            'ISCN3_dataset.csv',
                                            'ISCNtemplate.csv',
                                            'ISCNTranscribed_TemplateCVs.csv',
                                            'ISCN-Database.pdf',
                                            'Gen3-DB-documentation_REV.pdf',
                                            'TemplateCVs.pdf',
                                            'TemplateSubmit.pdf',
                                            'C&QA.Rmd')))
  
  ## Download the data ####
  
  download_table <- tibble::tribble(~download_url, ~file_name, 
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=4af719a84f8981fcc63f1f92760cb253", file.path(dataDir, 'ISCN3_layer.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=40527580cc045d33d9a5aaf728bf204e", file.path(dataDir, 'ISCN3_profile.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=320e31ca911f187550ca2143c31fd408", file.path(dataDir, 'ISCN3_citation.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=cdd0c7a4cac3f28d6d788c91f506775f", file.path(dataDir, 'ISCN3_dataset.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=a8eef6e94b669b365e443c15d9402a03", file.path(dataDir, 'ISCNTranscribed_TemplateCVs.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=114c95dd318e088108158adc3ae4eb23", file.path(dataDir, 'ISCNtemplate.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=eb320ae7b57296765f543cbb370b0f24", file.path(dataDir, 'TemplateCVs.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=b81a7f4214d176280a5ff4e0f0d52d8b", file.path(dataDir, 'TemplateSubmit.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=f914e6978c595c9a373dbc58365b6795", file.path(dataDir, 'Gen3-DB-documentation_REV.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=4dbc81eab612e09b84c688bb387d06c2", file.path(dataDir, 'ISCN-Database.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=3903927ae52655ff6359bc7c454aa42e", file.path(dataDir, 'C&QA.Rmd'))
  
  if(verbose) print('Download file.')
  
  for(row_index in 1:nrow(download_table)){
    if(!file.exists(download_table$file_name[row_index])){
      utils::download.file(download_table$download_url[row_index], download_table$file_name[row_index], quiet=!verbose)
    }
  }
  
  
  
  ## Read data files ####
  
  if(verbose) print('Meta data read in.')
  
  citation.dt <- readr::read_delim(file.path(dataDir, 'ISCN3_citation.csv'), delim = ';', col_types = strrep('c', times = 12)) %>% 
    #round all modification dates to their nearest day (ie whole number)
    dplyr::mutate(`modification_date (YYYY-MM-DD)` = as.character(round(as.numeric(`modification_date (YYYY-MM-DD)`))))
  
  dataset.dt <- readr::read_delim(file.path(dataDir, 'ISCN3_dataset.csv'), delim = ';', col_types = strrep('c', times = 19)) %>% 
    #round all modification dates to their nearest day (ie whole number)
    dplyr::mutate(`modification_date (YYYY-MM-DD)` = as.character(round(as.numeric(`modification_date (YYYY-MM-DD)`))))
  
  if(verbose) print('Profile data read in.')
  #only one profile data sheet
  profile.dt <- vroom::vroom(file.path(dataDir, 'ISCN3_profile.csv'), col_types = strrep('c', times = 44))
  
  if(verbose) print('Layer data read in.')

  layer.dt <- vroom::vroom(file.path(dataDir, 'ISCN3_layer.csv'), col_types = strrep('c', times = 95))
  
  ##add collection level details like citation
  collection.dt <- data.table::data.table(collection_name_id = 'ISCN3.2', 
                                                   variable = 'collection_citation',
                                                   entry = 'Nave L, Johnson K, van Ingen C, Agarwal D, Humphrey M, Beekwilder N. 2017. International Soil Carbon Network (ISCN) Database, Version 3.2. DOI: 10.17040/ISCN/1305039. Database Report: ISCN_SOC-DATA_LAYER_1-1. Accessed 2 February 2017',
                                                   type = 'value')
  keys.ls <- makeKeys()
  
  if(orginalFormat){
    
    return(list(citation=citation.dt, dataset=dataset.dt, profile = profile.dt, layer = layer.dt, 
                key=keys.ls$ISCN3, 
                collection = collection.dt))
  }else{
    ans <- formatLongTable(list(citation= setDT(citation.dt), 
                                dataset=setDT(dataset.dt), profile = setDT(profile.dt), layer = setDT(layer.dt)),
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


