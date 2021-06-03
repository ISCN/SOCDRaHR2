#' Raw Read for data corresponding to the Kane dataset
#'
#' Download and read in the raw data tables from Kane, E., Ping, C., Chapin, S. and Valentine, D.
#' with Bonanza LTER. doi:10.6073/pasta/d09433eee2cb6587eca672864cf7e90f
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository 
#'
#' @return a list with meta data and raw table reads. This will be 30 KB in size.
#'
#' @export

dataDir <- '~/Desktop/Research/Kane' #edit to reflect personal directory

#data read in for citation [2]
#url confirmation is needed, see ##TODO


KaneData2004 <- function(dataDir, download = TRUE, verbose = FALSE){
  
  ### Download of Pit Data ##
  KanePitDownload_url <- 'http://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/190_2031_all_soil_profile_depths_carbon_BD.txt'
  KaneDownload_target <- file.path(dataDir, 'KanePitData2004.txt')
  
  KaneDataFiles <- file.path(dataDir, 'KanePitData2004.txt')
  
  if(!all(file.exists(KaneDataFiles))){
    download.file(KanePitDownload_url, KaneDownload_target, quiet=FALSE)
  }
  
   # ans <- list(downloadFiles = c(file.path(dataDir,),
   #                               KaneDataFiles),
   #             licenseShort = "",
   #             licenseFull = "",
   #             
   #             citation = "Kane, Evan S.; Ping, Chien-Lu L. 2004. Soil carbon stabilization along productivity gradients in interior Alaska: Summer 2003, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:132, http://www.lter.uaf.edu/data/data-detail/id/132.",
   # 
   #             abstract = "Boreal forests in a warmer future climate are likely to exhibit altered productivity levels, tightened fire return intervals, and increased decomposition rates to varying degrees across the landscape. This research focuses on mechanisms of soil C stabilization in P. mariana systems along gradients in stand productivity. Charred material in the soil will be quantified to understand the lasting effect of fire on the stabilization of soil C. The interaction between temperature and productivity in relation to the stabilization of soil C will be investigated by monitoring climate and soil temperatures along the productivity gradients and through laboratory incubations of soil. Research questions are addressed in three main areas of inquiry: 1) how the interaction between stand production and landscape position effect the stabilization of C throughout the soil profile, 2) how the contribution of burn residues to total C accumulation varies across the landscape, and 3) the relationship between aboveground productivity and burn residues across the landscape. The overall goal is to apply an understanding of the biophysical controls on C storage in the boreal forest to the landscape level.")
  
  readKanePitData2004 <- read_csv(file.path(dataDir, 'KanePitData2004.txt'))
  
###
  
  KaneBiophysicalDownload_url <- 'http://www.lter.uaf.edu/php/download_data.php?f=/data_files/ascii/files/190_1608_sitesummarydata.txt'
  KaneDownload_target <- file.path(dataDir, 'KaneBiophysicalData2004.txt')
  
  KaneDataFiles <- file.path(dataDir, 'KaneBiophysicalData2004.txt')
  
  if(!all(file.exists(KaneDataFiles))){
    download.file(KaneBiophysicalDownload_url, KaneDownload_target, quiet=FALSE)
  }
  
   # ans <- list(downloadFiles = c(file.path(dataDir,),
   #                               KaneDataFiles),
   # 
   #             licenseShort = "",
   #             licenseFull = "",
   #             citation = "Kane, Evan S.; Ping, Chien-Lu L. 2004. Soil carbon stabilization along productivity gradients in interior Alaska: Summer 2003, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:132, http://www.lter.uaf.edu/data/data-detail/id/132.",
   # 
   #             abstract = "Boreal forests in a warmer future climate are likely to exhibit altered productivity levels, tightened fire return intervals, and increased decomposition rates to varying degrees across the landscape. This research focuses on mechanisms of soil C stabilization in P. mariana systems along gradients in stand productivity. Charred material in the soil will be quantified to understand the lasting effect of fire on the stabilization of soil C. The interaction between temperature and productivity in relation to the stabilization of soil C will be investigated by monitoring climate and soil temperatures along the productivity gradients and through laboratory incubations of soil. Research questions are addressed in three main areas of inquiry: 1) how the interaction between stand production and landscape position effect the stabilization of C throughout the soil profile, 2) how the contribution of burn residues to total C accumulation varies across the landscape, and 3) the relationship between aboveground productivity and burn residues across the landscape. The overall goal is to apply an understanding of the biophysical controls on C storage in the boreal forest to the landscape level.")
  
  readKaneBiophysicalData2004 <- read_csv(file.path(dataDir, 'KaneBiophysicalData2004.txt'), col_names = c("Site Description", "Site ID", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14"))
  
  
  ### Download of Metadata ###
  KaneMetaDownload_url <- 'http://www.lter.uaf.edu/eml/BNZeml132.xml'
  KaneDownload_target <- file.path(dataDir, 'KaneMetaData2004.txt')
  
  KaneDataFiles <- file.path(dataDir, 'KaneMetaData2004.txt')
  
  if(!all(file.exists(KaneDataFiles))){
    download.file(KanePitDownload_url, KaneDownload_target, quiet=FALSE)
    
    
  #return(ans)
}
}
