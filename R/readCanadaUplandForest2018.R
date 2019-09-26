#' Raw read for Canadian upland soils (2018)
#' 
#' Download and read in the raw data tables from Shaw, C., Hilger, A., Filiatrault, M. and
#'  Kurz, W. (2018), A Canadian upland forest soil profile and carbon stocks 
#'  database. Ecology. doi:10.1002/ecy.2159
#'  This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository
#'
#' @return a list with meta data and raw table reads. This will be 7.4 Mb in size.
#' 
#' @export
#' 
#'
readCanandaUplandForest2018 <- function(dataDir, download=TRUE){
  
  #### Download ####

  ShawDownload_url <- 'https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002/ecy.2159'
  ShawDownload_target <- file.path(dataDir, 'CUFS2018.zip') 
  
  ShawDataFiles <- file.path(dataDir, 'CUFS2018',
                             c("LOOKUP.csv", "LOOKUP_DB.csv", "PROFILES.csv",
                               "REFERENCES.csv", "SITES.csv"))
  
  if(!all(file.exists(ShawDataFiles))){
    download.file(ShawDownload_url, ShawDownload_target, quiet=FALSE)
  }
  
  unzip(ShawDownload_target, exdir=file.path(dataDir, 'CUFS2018'))
  unzip(file.path(dataDir, 'CUFS2018', 'ecy2159-sup-0001-datas1.zip'), exdir=file.path(dataDir, 'CUFS2018'))
  
  
  ans <- list(downloadFiles = c(file.path(dataDir,'CUFS2018', 'ecy2159-sup-0002-MetadataS1.pdf'),
                                ShawDataFiles),
              licenseShort = "nonCC with BY-NC",
              licenseFull = "Copyright restrictions
© Her Majesty the Queen in Right of Canada, 2017
              
              Information contained in this publication or product may be reproduced, in part or in whole, and by any means, for personal or public non-commercial purposes, without charge or further permission, unless otherwise specified.
              You are asked to:
              
              -  exercise due diligence in ensuring the accuracy of the materials reproduced;
              -  indicate the complete title of the materials reproduced, and the name of the author organization; and
              -  indicate that the reproduction is a copy of an official work that is published by Natural Resources Canada (NRCan) and that the reproduction has not been produced in affiliation with, or with the endorsement of, NRCan.
              -  Commercial reproduction and distribution is prohibited except with written permission from NRCan. For more information, contact NRCan at copyright.droitdauteur@nrcan-rncan.gc.ca.",
              
              citation = "Shaw, C., Hilger, A., Filiatrault, M. and Kurz, W. (2018), A Canadian upland forest soil profile and carbon stocks database. Ecology. doi:10.1002/ecy.2159")
  
  
  for(readFile in ShawDataFiles){
    ans[[gsub('.csv', '', basename(readFile))]] <- data.table::as.data.table(
      read.csv(readFile, colClasses="character", na.strings = c('NA', ''))
      )
  }
  
  
  return(ans)
}