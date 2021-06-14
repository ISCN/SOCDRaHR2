#' Raw Read for data corresponding to the USGS_Harden dataset
#'
#' Download and read in the raw data tables from Harden, W., Turetsky, M., Manies, P., and
#' Waldrop, M. with Bonanza LTER. doi:10.6073/pasta/9b363cc74bcd784cfcee85e6920f2989
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository 
#'
#' @return a list with meta data and raw table reads. This will be ____ KB in size.
#' @export
#' @importFrom readr read_csv


#r data read in for citation [1], Harden2008a currently not possible due to stale URL link
#contacting of researchers is required, see ##TODO in data_reports/120_USGS_Harden.Rmd
#licensing information was not found, contacting of EDI is necessary

#Download and read in the raw data tables for Harden2008 data citations
readUSGSHarden2008 <- function(dataDir, download = TRUE, verbose = FALSE){
  
  urlTable <- data.frame(fileBase = c('Harden2008aData.txt', 'Harden2008aMetaData.xml',  'Harden2008bData.txt', 'Harden2008bMetaData.xml'),
                         downloadURL = c('', '', 'http://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/334_BNZ_moisturegradient_isotopestudy_2004_LTER.txt', 'http://www.lter.uaf.edu/eml/BNZeml334.xml'))
  
  #for loop to read in data from url_table if it does not exist in local repository
  for(ii in 1:nrow(urlTable)){
    dataFile <- file.path(dataDir, urlTable$fileBase[ii])
    if(!(file.exists(dataFile))){
      download.file(urlTable$downloadURL[ii], destfile= dataFile, quiet=FALSE)
    }
  }
  
  # #this code was not used as it requires additional package dependency
  # plyr::d_ply(
  #   urlTable,
  #   c('downloadURL','fileBase'),
  #   function(xx){
  #     dataFile <- file.path(dataDir, xx$fileBase)
  #     if(!(file.exists(dataFile))){
  #     download.file(xx$downloadURL, destfile= dataFile, quiet=FALSE)
  #     }
  #     }
  #   )
  
  #assigning filenames to a variable
  harden2008aData <- file.path(dataDir, 'Harden2008aData.txt')
  harden2008aMetadata <- file.path(dataDir, 'Harden2008aMetaData.xml')
  harden2008bData <- file.path(dataDir, 'Harden2008bData.txt')
  harden2008bMetadata <- file.path(dataDir, 'Harden2008bMetaData.xml')
  
  #reading in data
  readHarden2008aData <- readr::read_csv(harden2008aData)
  readHarden2008aMetadata <- readr::read_csv(harden2008aMetadata)
  readHarden2008bData <- readr::read_csv(harden2008bData)
  readHarden2008bMetadata <- readr::read_csv(harden2008bMetadata)
  
  
  #ans and its return
  ans <- list(downloadFiles = c(harden2008aData, harden2008aMetadata, harden2008bData, harden2008bMetadata),
              licenseShort = "",
              licenseFull = "",
              citation = c('', '', "Jennifer W. Harden, Merritt R Turetsky, Kristen L Manies, Mark P. Waldrop, and Bonanza Creek LTER. 2008. Bonanza Creek moisture gradient soil core data: 2004. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-bnz/334/18.", ''),
              abstract = c(""))
  
  return(ans)
  
}