#' Raw Read for data corresponding to the Schuur dataset
#'
#' Download and read in the raw data tables from Hicks-Pries C. and Bonanza Creek LTER (2009).
#' Identifier: knb-lter-bnz.366.16
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository 
#'
#' @return a list with meta data and raw table reads. This will be ___ Mb in size.
#'
#' @export

dataDir <- '~/Desktop/Research/Schuur' #edit to reflect personal directory

readSchuur <- function(dataDir, download=TRUE, verbose=FALSE){
  
  ### Download ###
  
  SchuurDownload_url <- ''
  SchuurDownload_target <- file.path(dataDir, '') 
  
  SchuurDataFiles <- file.path(dataDir, '',
                             c(""))
  
  if(!all(file.exists(SchuurDataFiles))){
    download.file(SchuurDownload_url, SchuurDownload_target, quiet=FALSE)
  }
  
  
  ans <- list(downloadFiles = c(file.path(dataDir,'', ''),
                                SchuurDataFiles),
              licenseShort = "",
              licenseFull = "",
              
              citation = "Caitlin Elizabeth Hicks-Pries and Bonanza Creek LTER. 2009. The impact of permafrost thaw on ecosystem carbon balance: Eight Mile Lake soil carbon and nitrogen. LTER Network Member Node. knb-lter-bnz.366.16.", 
              
              abstract = "In this larger study, we are asking the question: Is old carbon that comprises the bulk of the soil organic matter pool released in response to thawing of permafrost? We are answering this question by using a combination of field and laboratory experiments to measure radiocarbon isotope ratios in soil organic matter, soil respiration, and dissolved organic carbon, in tundra ecosystems. The objective of these proposed measurements is to develop a mechanistic understanding of the SOM sources contributing to C losses following permafrost thawing. We are making these measurements at an established tundra field site near Healy, Alaska in the foothills of the Alaska Range. Field measurements center on a natural experiment where permafrost has been observed to warm and thaw over the past several decades. This area represents a gradient of sites each with a different degree of change due to permafrost thawing. As such, this area is unique for addressing questions at the time and spatial scales relevant for change in arctic ecosystems. In this data set, thaw gradient site soil carbon and nitrogen inventories are reported by depth layer for both organic and mineral horizons. The inventories include % Carbon, % Nitrogen, bulk density, gC/m2, gN/m2, 13C, and 14N.")
  
  
  return(ans)
}