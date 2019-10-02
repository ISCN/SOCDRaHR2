#' Read in new ISCN5 contributions
#'
#' Read in the two ISCN5 contributions
#'
#' @param dataDir string identifying the data directory
#' @param orginalFormat boolean flag whether or not to translate to keep in orginal data table.
#'
#' @return a list of data tables
#' @export
#'
ISCN5 <- function(dataDir, orginalFormat = TRUE, newDataOnly=TRUE, verbose = FALSE){
  
  CUFS2018 <- readCanandaUplandForest2018(dataDir=dataDir)
  
  CPEAT2018 <- readCPEAT(dataDir=dataDir)
  #plyr::ldply(CPEAT2018, function(xx){data.frame(header=names(xx))})
  #write.csv(plyr::ldply(CPEAT2018, function(xx){data.frame(header=names(xx))}), file='data-raw/key_CPEAT2018.csv', row.names = FALSE)
  
  key.ls <- makeKeys()
  
  if(orginalFormat){
    return(list(CUFS2018 = CUFS2018,
                CPEAT2018 = CPEAT2018))
  }else{
    ans1 <- formatLongTable(CUFS2018[c('PROFILES', 'REFERENCES', 'SITES')],
                           sourceKey = key.ls$CUFS2018, targetKey = key.ls$ISCN, verbose=verbose)
    ans1$collection <- data.table::data.table(collection_name_id = 'Canadian Upland Forest Soils 2018',
                                  variable = c('license', 'collection_citation'),
                                  type = 'value',
                                  entry = c(CUFS2018$licenseFull, CUFS2018$citation))
    ans1$study$collection_name_id <- ans1$collection$collection_name_id[1]
    ans1$profile$collection_name_id <- ans1$collection$collection_name_id[1]
    ans1$layer$collection_name_id <- ans1$collection$collection_name_id[1]
    
    ans2 <- formatLongTable(CPEAT2018[c('site', 'sample', 'files')],
                            sourceKey = key.ls$CPEAT, targetKey = key.ls$ISCN)
    ans2$collection <- data.table::data.table(collection_name_id = 'CPEAT 2018',
                                  variable = c('license'),
                                  type = 'value',
                                  entry = 'Creative Commons Attribution 3.0 Unported (CC-BY-3.0)')
    ans2$study$collection_name_id <- ans2$collection$collection_name_id
    ans2$profile$collection_name_id <- ans2$collection$collection_name_id
    ans2$layer$collection_name_id <- ans2$collection$collection_name_id
    
    ans <- list(collection = data.table::rbindlist(list(ans1$collection, ans2$collection), fill=TRUE),
      study = data.table::rbindlist(list(ans1$study, ans2$study), fill=TRUE),
      profile = data.table::rbindlist(list(ans1$profile, ans2$profile), fill=TRUE),
      layer = data.table::rbindlist(list(ans1$layer, ans2$layer), fill=TRUE))
    if(newDataOnly){
    return(ans)
    }else{
      ISCN <- ISCN4(dataDir = dataDir, onlyNewData = FALSE, verbose=verbose)
      
      return(list(collection=data.table::rbindlist(list(ans$collection, ISCN$collection), fill=TRUE),
                  study = data.table::rbindlist(list(ans$study, ISCN$study), fill=TRUE),
                  profile = data.table::rbindlist(list(ans$profile, ISCN$profile), fill=TRUE),
                  layer = data.table::rbindlist(list(ans$layer, ISCN$layer), fill=TRUE)))
    }
  }
}