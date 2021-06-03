#' Raw Read for data corresponding to the USGS_Harden dataset
#'
#' Download and read in the raw data tables from Harden, W., Turetsky, M., Manies, P., and Waldrop, M.
#' with Bonanza LTER. doi:10.6073/pasta/9b363cc74bcd784cfcee85e6920f2989
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository 
#'
#' @return a list with meta data and raw table reads. This will be ___ Mb in size.
#'
#' @export

#{r data read in for citation [1], Harden2008a}
#download link leads to error, see ##TODO


#{r data read in for citation [4], Harden2008b}

dataDir <- '~/Desktop/Research/Harden2008b' #edit to reflect personal directory

#Download and read in the raw data tables for Harden2008b data citation
Harden2008bData <- function(dataDir, download = TRUE, verbose = FALSE){
  Harden2008bDownload_url <- 'https://pasta.lternet.edu/package/data/eml/knb-lter-bnz/334/18/ae919a8ce160674a01f8394e6a15fc5d'
  Harden2008bDownload_target <- file.path(dataDir, 'Harden2008bData.txt')
  
  Harden2008bDataFiles <- file.path(dataDir, 'Harden2008bData.txt')
  
  if(!all(file.exists(Harden2008bDataFiles))){
    download.file(Harden2008bDownload_url, Harden2008bDownload_target, quiet=FALSE)
  }
  
  # ans <- list(downloadFiles = c(file.path(dataDir,),
  #                               Harden2008bDataFiles),
  #             licenseShort = "",
  #             licenseFull = "",
  #             
  #             citation = "Jennifer W. Harden, Merritt R Turetsky, Kristen L Manies, Mark P. Waldrop, and Bonanza Creek LTER. 2008. Bonanza Creek moisture gradient soil core data: 2004. LTER Network Member Node. https://pasta.lternet.edu/package/metadata/eml/knb-lter-bnz/334/18.")
  
  readHarden2008bData <- read_csv(file.path(dataDir, 'Harden2008bData.txt'))
  
  # return(ans)
}
