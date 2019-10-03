#' Read in new ISCN5 contributions
#'
#' Read in the two ISCN5 contributions
#'
#' @param dataDir string identifying the data directory
#' @param orginalFormat boolean flag whether or not to translate to keep in orginal data table.
#'
#' @import data.table
#'
#' @return a list of data tables
#' @export
#'
ISCN5 <- function(dataDir, orginalFormat = TRUE, newDataOnly=TRUE, verbose = FALSE){
  
  CUFS2018 <- readCanandaUplandForest2018(dataDir=dataDir)
  
  CPEAT2018 <- readCPEAT(dataDir=dataDir)
  CPEAT2018$sample <- unique(CPEAT2018$sample)
  #plyr::ldply(CPEAT2018, function(xx){data.frame(header=names(xx))})
  #write.csv(plyr::ldply(CPEAT2018, function(xx){data.frame(header=names(xx))}), file='data-raw/key_CPEAT2018.csv', row.names = FALSE)
  
  key.ls <- makeKeys()
  
  if(orginalFormat){
    return(list(CUFS2018 = CUFS2018,
                CPEAT2018 = CPEAT2018))
  }else{
    ###Harmonize Canadian Upland Forest Soils#######
    
    #Convert to depth from top units
    CUFS2018$PROFILES[, layer_top := as.character(as.numeric(UPPER_HZN_LIMIT)-min(as.numeric(UPPER_HZN_LIMIT))), by=LOCATION_ID][,
                        layer_bottom := as.character(as.numeric(layer_top) + as.numeric(HZN_THICKNESS)), by=LOCATION_ID] 
    key.ls$CUFS2018 <- data.table::rbindlist(list(key.ls$CUFS2018,
                                             data.table::data.table(table = 'PROFILES', header = c('layer_bottom', NA), variable = 'layer_bottom', type= c('value', 'unit'), entry = c(NA, 'cm')),
                                               data.table::data.table(table = 'PROFILES', header = c('layer_top', NA), variable = 'layer_top', type= c('value', 'unit'), entry = c(NA, 'cm'))), fill = TRUE)
    
    #Reformat
    ans1 <- formatLongTable(CUFS2018[c('PROFILES', 'REFERENCES', 'SITES')],
                           sourceKey = key.ls$CUFS2018, targetKey = key.ls$ISCN, verbose=verbose)
    ans1$collection <- data.table::data.table(collection_name_id = 'Canadian Upland Forest Soils 2018',
                                  variable = c('license', 'collection_citation'),
                                  type = 'value',
                                  entry = c(CUFS2018$licenseFull, CUFS2018$citation))
    hardKeys <- key.ls$CUFS2018[!is.na(entry) & !is.na(variable), c('variable', 'type', 'entry')]
    hardKeys[,collection_name_id := ans1$collection$collection_name_id[1]]
    ans1$collection <- data.table::rbindlist(list(ans1$collection, hardKeys), fill = TRUE)
    
    ans1$study$collection_name_id <- ans1$collection$collection_name_id[1]
    ans1$profile$collection_name_id <- ans1$collection$collection_name_id[1]
    ans1$layer$collection_name_id <- ans1$collection$collection_name_id[1]
    
    ######Harmonize CPEAT########
    
    #Convert the units for the depth from m to cm and add a top to the layers
    CPEAT2018$sample[,layer_bottom := as.character(as.numeric(`Depth [m]`)*100)][,layer_top := as.character(c(0, layer_bottom[-length(layer_bottom)])), by=Site_core]
    #...update the key
    key.ls$CPEAT <- data.table::rbindlist(list(key.ls$CPEAT[variable != 'layer_bottom', ],
                               data.table::data.table(table = 'sample', header = c('layer_bottom', NA), variable = 'layer_bottom', type= c('value', 'unit'), entry = c(NA, 'cm')),
                               data.table::data.table(table = 'sample', header = c('layer_top', NA), variable = 'layer_top', type= c('value', 'unit'), entry = c(NA, 'cm'))), fill = TRUE)
    
    #Reformat to long
    ans2 <- formatLongTable(CPEAT2018[c('site', 'sample', 'files')],
                            sourceKey = key.ls$CPEAT, targetKey = key.ls$ISCN)
    ans2$collection <- data.table::data.table(collection_name_id = 'CPEAT 2018',
                                  variable = c('license'),
                                  type = 'value',
                                  entry = 'Creative Commons Attribution 3.0 Unported (CC-BY-3.0)')
    hardKeys <- key.ls$CPEAT[!is.na(entry) & !is.na(variable), c('variable', 'type', 'entry')]
    hardKeys[,collection_name_id := ans2$collection$collection_name_id[1]]
    ans2$collection <- data.table::rbindlist(list(ans2$collection, hardKeys), fill = TRUE)
    
    ans2$study$collection_name_id <- ans2$collection$collection_name_id[1]
    ans2$profile$collection_name_id <- ans2$collection$collection_name_id[1]
    ans2$layer$collection_name_id <- ans2$collection$collection_name_id[1]
    
    
    #######Put everything together#####
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
                  layer = data.table::rbindlist(list(ans$layer, ISCN$layer), fill=TRUE),
                  key = key.ls))
    }
  }
}