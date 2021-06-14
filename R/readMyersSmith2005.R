#' Raw read for Myers-Smith (2005)
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
readMyersSmith2005 <- function(dataDir, download=TRUE, verbose=FALSE){
  #### Download ####
  
  MyersSmithDownload_url <- 'https://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/bog_soil_data.txt'
  MyersSmithDownload_target <- file.path(dataDir, 'MyersSmith2005.txt') 
  
  download.file(MyersSmithDownload_url,  MyersSmithDownload_target, quiet=FALSE)
  

  MyersSmithXML <- file.path(dataDir, 'MyersSmith.xml')
  
  if(!all(file.exists(dataDir,'MyersSmith.xml'))) {
    download.file('http://www.lter.uaf.edu/eml/BNZeml192.xml', MyersSmithXML)
  }
  
  #### Read into table ####
  
  ans <- list(MyersSmithData = readr::read_tsv(file = MyersSmithDownload_target),
              publications = c("Myers-Smith, I., A.D. McGuire, J.W. Harden, and F.S. Chapin III. 2007. The influence of disturbance on carbon exchange in a permafrost collapse and adjacent burned forest. Journal of Geophysical Research - Biosciences 112, G04017, doi:10.1029 2007JG000423.", "Myers-Smith, I.H., J.W. Harden, M. Wilmking, C.C. Fuller, A.D. McGuire, and F.S. Chapin, III. 2007. Wetland succession in a permafrost collapse: Interactions between fire and thermokarst. Biogeosciences Discussion 4:4507-4538."),
              dataAcknowledgement = "Data are provided by the Bonanza Creek LTER, a partnership between the University of Alaska Fairbanks, and the U.S. Forest Service. Significant funding for collection of these data was provided by the National Science Foundation Long-Term Ecological Research program (NSF Grant numbers DEB-1636476, DEB-1026415, DEB-0620579, DEB-0423442, DEB-0080609, DEB-9810217, DEB-9211769, DEB-8702629) and by the USDA Forest Service, Pacific Northwest Research Station (Agreement # RJVA-PNW-01-JV-11261952-231).",
              dataCitation= "Myers-Smith, Isla. 2005. Soil data for cores from a transect from the center of the BBC collapse scar into the surrounding burn, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:192, http://www.lter.uaf.edu/data/data-detail/id/192. doi:10.6073/pasta/b0e9120983438c27bf1a30f37f0e5200")
  
  
  return(ans)
}