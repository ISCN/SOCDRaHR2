#' Raw read for USGS_S3C (2011)
#' 
#' Download and read in raw data from Myers-Smith, Isla. 2005. Soil data for cores from a transect from the center of the BBC collapse scar into the surrounding burn, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:192, http://www.lter.uaf.edu/data/data-detail/id/192. doi:10.6073/pasta/b0e9120983438c27bf1a30f37f0e5200
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param data_dir filename for download directory
#' @param download boolean that will download the files from repository
#'
#' @return a list with meta data and raw table reads. This will be 67 KB in size.
#' @export
#' @importFrom readr read_csv

readBuell2011 <- function(dataDir, download=TRUE, verbose=FALSE){
  
  
  urlTable <- data.frame(fileName = c(file.path(dataDir, 'bog_soil_data.txt'),
                                      file.path(dataDir, 'BNZeml192.xml')),
                         downloadURL = c('http://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/bog_soil_data.txt',
                                         'http://www.lter.uaf.edu/eml/BNZeml192.xml'))
  
  
  #for loop to read in data from urlTable if it does not exist in local repository
  for(ii in 1:nrow(urlTable)){
    dataFile <- urlTable$fileName[ii]
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
  
  
  #reading in data
  readBuelllayerData <- readr::read_tsv(urlTable$fileName[1],
                                        col_types = strrep("c", 19))
  
  #ans and its return
  ans <- list(downloadFiles = c(urlTable$fileName[1],
                                urlTable$fileName[2]),
              bog_soil_data.txt = readBuelllayerData,
              licenseShort = "",
              licenseFull = "",
              citation = c("	Myers-Smith, Isla. 2005. Soil data for cores from a transect from the center of the BBC collapse scar into the surrounding burn, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:192, http://www.lter.uaf.edu/data/data-detail/id/192. doi:10.6073/pasta/b0e9120983438c27bf1a30f37f0e5200"),
              abstract = c("This data set contains soil data for cores from a transect from the center of the BBC collapse scar (0 m) into the surrounding burn (30 m). Thirty-five cores were collected soil cores along the transect in March 2003."),
              publications = c("Myers-Smith, I., A.D. McGuire, J.W. Harden, and F.S. Chapin III. 2007. The influence of disturbance on carbon exchange in a permafrost collapse and adjacent burned forest. Journal of Geophysical Research - Biosciences 112, G04017, doi:10.1029 2007JG000423.",
                               "Myers-Smith, I.H., J.W. Harden, M. Wilmking, C.C. Fuller, A.D. McGuire, and F.S. Chapin, III. 2007. Wetland succession in a permafrost collapse: Interactions between fire and thermokarst. Biogeosciences Discussion 4:4507-4538."))
  
  
  return(ans)
  
}
