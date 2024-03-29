#' Raw Read for data corresponding to the Kane dataset
#'
#' Download and read in the raw data tables from Kane, E., Ping, C., Chapin, S. and 
#' Valentine, D. with Bonanza LTER. doi:10.6073/pasta/d09433eee2cb6587eca672864cf7e90f
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository 
#'
#' @return a list with meta data and raw table reads. This will be 49 KB in size.
#' @export
#' @importFrom readr read_csv
readKane2004 <- function(dataDir, download = TRUE, verbose = FALSE){
 
  
  #data read in for citation [2] not included
  #url confirmation is needed, see ##TODO from data_reports/114_Kane.Rmd
  #KaneBiophysicalData is missing header information that could not be found online, contacting of researchers is necessary
  #licensing information was not found, contacting of EDI is necessary
  
  urlTable <- data.frame(fileName = c(file.path(dataDir, 'x190_2031_all_soil_profile_depths_carbon_BD.txt'),
                                      file.path(dataDir,'x190_1608_sitesummarydata.txt'),
                                      file.path(dataDir, 'BNZeml132.xml')),
                          downloadURL = c('http://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/190_2031_all_soil_profile_depths_carbon_BD.txt',
                                          'http://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/190_1608_sitesummarydata.txt',
                                          'http://www.lter.uaf.edu/eml/BNZeml132.xml'))
  
  
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
  readKanelayerData <- readr::read_csv(urlTable$fileName[1])
  readKanesiteData <- readr::read_csv(urlTable$fileName[2], col_names = c("Site Description", "Site ID", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14")) #note that most headers are missing

  #ans and its return
  ans <- list(downloadFiles = c(urlTable$fileName[1],
                                urlTable$fileName[2],
                                urlTable$fileName[3]),
              x190_2031_all_soil_profile_depths_carbon_BD.txt = readKanelayerData,
              x190_1608_sitesummarydata.txt = readKanesiteData,
              licenseShort = "",
              licenseFull = "",
              citation = c("Kane, Evan S.; Ping, Chien-Lu L. 2004. Soil carbon stabilization along productivity gradients in interior Alaska: Summer 2003, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:132, http://www.lter.uaf.edu/data/data-detail/id/132."),
              abstract = c("Boreal forests in a warmer future climate are likely to exhibit altered productivity levels, tightened fire return intervals, and increased decomposition rates to varying degrees across the landscape. This research focuses on mechanisms of soil C stabilization in P. mariana systems along gradients in stand productivity. Charred material in the soil will be quantified to understand the lasting effect of fire on the stabilization of soil C. The interaction between temperature and productivity in relation to the stabilization of soil C will be investigated by monitoring climate and soil temperatures along the productivity gradients and through laboratory incubations of soil. Research questions are addressed in three main areas of inquiry: 1) how the interaction between stand production and landscape position effect the stabilization of C throughout the soil profile, 2) how the contribution of burn residues to total C accumulation varies across the landscape, and 3) the relationship between aboveground productivity and burn residues across the landscape. The overall goal is to apply an understanding of the biophysical controls on C storage in the boreal forest to the landscape level."),
              publications = "Currently no publications.")

  
  return(ans)
  
}
