#' Read ISRaD contribution down to the layer level.
#'
#' @param dataDir string identifying the file where the ISRaD data will be downloaded
#' @param gitRef a string identifying the git reference for the ISRaD install
#' @param verbose boolean flag to produce excessive debugging statements
#'
#' @return a list of character-based data tables
#' @export
#' @importFrom devtools install_github
#'
readISRaD <- function(dataDir, verbose=FALSE, gitRef = "master"){

  if(verbose) print('installing the ISRaD R package from github')
  #devtools::install_github("International-Soil-Radiocarbon-Database/ISRaD/Rpkg", 
  #               ref=gitRef)
  devtools::install_github("ktoddbrown/ISRaD/Rpkg", 
                 ref=gitRef)
  
  #dataDir <- '~/Documents/Datasets/ISRaD
  if(verbose) print('fetching ISRaD data and casting it as a character-based data table')
  ISRaD.ls <- lapply(ISRaD::ISRaD.getdata(directory = dataDir, extra = TRUE),#, branch = gitRef), 
         function(xx){ data.table::as.data.table( lapply(xx, as.character))})
  
  # library(tidyverse)
  # plyr::ldply(ISRaD.ls[c("metadata", "site", "profile", "layer")], function(xx){
  #   data.frame(header = names(xx))
  # }, .id = 'table') %>%
  #   write.csv(file='data-raw/key_ISRaD.csv')
  # tempMeta <- readxl::read_xlsx(path = file.path(dataDir, 'ISRaD_Template_Info.xlsx'), sheet = 'metadata', col_types = "text") %>%
  #   mutate(table = 'metadata') %>% rename('Vocab' = 'Controlled_Vocab/Values')
  # tempSite <- readxl::read_xlsx(path = file.path(dataDir, 'ISRaD_Template_Info.xlsx'), sheet = 'site', col_types = "text") %>%
  #   mutate(table = 'site')
  #  tempProf <- readxl::read_xlsx(path = file.path(dataDir, 'ISRaD_Template_Info.xlsx'), sheet = 'profile', col_types = "text") %>%
  #    mutate(table = 'profile') %>% rename('Units/Info'='Units/info')
  # # tempLayer <- readxl::read_xlsx(path = file.path(dataDir, 'ISRaD_Template_Info.xlsx'), sheet = 'layer', col_types = "text") %>%
  # #   mutate(table = 'layer')
  #  temp <- bind_rows(tempMeta, tempSite, tempProf, tempLayer)
  #  write.csv(temp, file='data-raw/key_ISRaD.csv')
  # 
  
  ###only return the agreed upon data tables. 
  ###Check ISRaD MOU before changing this!!!
  return(ISRaD.ls[c("metadata", "site", "profile", "flux", "layer")])
}