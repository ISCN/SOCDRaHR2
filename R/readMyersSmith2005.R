#' Raw read for Myers-Smith (2018)
#' 
#' Download and read in raw data from Myers-Smith, Isla. 2005. Soil data for cores from a transect from the center of the BBC collapse scar into the surrounding burn, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:192, http://www.lter.uaf.edu/data/data-detail/id/192. doi:10.6073/pasta/b0e9120983438c27bf1a30f37f0e5200
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository
#'
#' @return a data frame containing newly ingested data from the Myers-Smith dataset
#' 
#' @export
#' @importFrom readr read_delim
#'
readMyersSmith <- function(dataDir, download=TRUE, verbose=FALSE){
  
  #### Download ####
  
  MyersSmithDownload_url <- 'https://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/bog_soil_data.txt'
  MyersSmithDownload_target <- file.path(dataDir, 'MyersSmith2005.txt') 
  
  download.file(MyersSmithDownload_url,  MyersSmithDownload_target, quiet=FALSE)
  
  
  #### Read into table ####
  
  ans <- readr::read_tsv(file = MyersSmithDownload_target)
  
  return(ans)
}