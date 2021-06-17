#' Raw read for Myers-Smith (2005)
#' 
#' Download and read in raw data from Myers-Smith, Isla. 2005. Soil data for cores from a transect from the center of the BBC collapse scar into the surrounding burn, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:192, http://www.lter.uaf.edu/data/data-detail/id/192. doi:10.6073/pasta/b0e9120983438c27bf1a30f37f0e5200
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository 
#'
#' @return a list with meta data and raw table reads. This will be 67 KB in size.
#' @export
#' @importFrom readr read_tsv



readMyersSmith2005 <- function(dataDir, download=TRUE, verbose=FALSE){
  
  urlTable <- data.frame(fileName = c(file.path(dataDir, 'bog_soil_data.txt'),
                                      file.path(dataDir, 'BNZeml192.xml')),
                         downloadURL = c('https://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/bog_soil_data.txt',
                                         'http://www.lter.uaf.edu/eml/BNZeml192.xml'))
  
  #for loop to read in data from urlTable if it does not exist in local repository
  for(ii in 1:nrow(urlTable)){
    dataFile <- file.path(dataDir, urlTable$fileName[ii])
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
  readSchuurlayerData <- readr::read_csv(urlTable$fileName[1])
  
  #ans and its return
  ans <- list(downloadFiles = c(urlTable$fileName[1],
                                urlTable$fileName[2]),
              bog_soil_data.txt = readSchuurlayerData,
              licenseShort = "",
              licenseFull = "",
              citation = "Myers-Smith, Isla. 2005. Soil data for cores from a transect from the center of the BBC collapse scar into the surrounding burn, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:192, http://www.lter.uaf.edu/data/data-detail/id/192. doi:10.6073/pasta/b0e9120983438c27bf1a30f37f0e5200",
              abstract = "This data set contains soil data for cores from a transect from the center of the BBC collapse scar (0 m) into the surrounding burn (30 m). Thirty-five cores were collected soil cores along the transect in March 2003. We drilled cores using a gasoline powered, permafrost corer while soils were frozen. Two to four cores were drilled every 3 m along the transect, yielding a total of 35 cores. We stored cores frozen and cut sample sections using a radial saw. Cores were sampled at the interfaces between different soil layers. We classified soils using the Canadian Soil Classification system (Soil Classification Working Group 1998) identifying fibric, mesic, and humic organic horizions and the A and C mineral horizons. Nine cores were sampled only to the mineral boundary. We measured bulk density, %C and %N for all soil samples. The pH of sample was determined using litmus paper. We oven-dried at 50 - 65°C and ground all samples before analysis. We analyzed samples for %C and %N using a Carlo Erba EA1108 CHNS analyzer (CE Instruments, Milan, Italy) and a COSTECH ECS 4010 CHNS-O analyzer (Costech Analytical Technologies Inc., Valencia, CA,USA). Sample standard errors were ± 0.01% for nitrogen, ± 0.45% for carbon. To indicate fire events in the surrounding ecosystem, charcoal layers in the cores were quantified. We estimated charcoal by emptying dried samples of a known volume and depth (on mean 4.5 cm3) over a 10 cm x 10 cm grid and counting macroscopic charcoal fragments (greater than 0.05 mm in diameter) in each cm grid cell.",
              publications = c("Myers-Smith, I., A.D. McGuire, J.W. Harden, and F.S. Chapin III. 2007. The influence of disturbance on carbon exchange in a permafrost collapse and adjacent burned forest. Journal of Geophysical Research - Biosciences 112, G04017, doi:10.1029 2007JG000423.",
                               "Myers-Smith, I. H., Harden, J. W., Wilmking, M., Fuller, C. C., McGuire, A. D., and Chapin III, F. S.: Wetland succession in a permafrost collapse: interactions between fire and thermokarst, Biogeosciences, 5, 1273–1286, https://doi.org/10.5194/bg-5-1273-2008, 2008."))
  
  return(ans)
}