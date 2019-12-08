#' Translational keys for datasets
#'
#' @return a list of data tables
#' @export
#'
#' @examples
#' allKeys <- makeKeys()
makeKeys <- function(dataDir=NULL, saveDir=NULL){
  ##data keys should have
  ##... table, header, variable, type, entry
  
  if(is.null(dataDir) | is.null(saveDir)){
    return(ISCN_key_list)
  }
  
  #### ISCN5 key ####
  ISCN5.target <- read.csv(file.path(dataDir, "ISCN_variableTables.csv"), 
                           colClasses = 'character', na.strings = c('', 'NA'))
  
  ##### ISCN3 key ####
  ISCN3Key <-  read.csv(file.path(dataDir, "key_ISCN3.csv"), 
                           colClasses = 'character', na.strings = c('', 'NA'))

  #### ISCN4 key ####
  ISCN2016 <- read.csv(file.path(dataDir, "key_ISCN2016.csv"),
                       colClasses = 'character', na.strings = c('', 'NA'))
  
  #### CPEAT key ####
  CPEAT2018Key <- read.csv(file.path(dataDir, "key_CPEAT2018.csv"),
                          colClasses = 'character', na.strings = c('', 'NA'))
  
  ####CanadaUplandForest2018####
  CUFS2018Key <- read.csv(file.path(dataDir, "key_CUFS2018.csv"),
                          colClasses = 'character', na.strings = c('', 'NA'))
  
  ####ISRaD####
  ISRaDKey <- read.csv(file.path(dataDir,'key_ISRaD.csv'), 
                       colClasses = 'character', na.strings = c('', 'NA'))
  
  ####return answer####
  ISCN_key_list <- list(ISCN = data.table::as.data.table(ISCN5.target), 
              ISCN3 = data.table::as.data.table(ISCN3Key),
              ISCN2016 = data.table::as.data.table(ISCN2016),
              CUFS2018 = data.table::as.data.table(CUFS2018Key),
              CPEAT = data.table::as.data.table(CPEAT2018Key),
              ISRaD = data.table::as.data.table(ISRaDKey))
  
  save(ISCN_key_list, file=file.path(saveDir, 'ISCN_key_list.RData'))
  return(ISCN_key_list)
  }