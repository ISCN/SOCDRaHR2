#' Read in two data sets queued in for data processing for ISCN4.
#'
#' @param dataDir string identifing the data directory to download data to, or NULL to download to a temporary directory
#' @param onlyNewData boolean flagging new ISCN4 data only or appended to ISCN3
#' @param verbose boolean flagging verbose error messages
#'
#' @return a list of dataframes containing the study, field, and sample data
#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom readr write_csv read_csv
#' @importFrom readxl read_excel
#' @importFrom lubridate decimal_date mdy
#' @export
#'
#' @examples 
#' \dontrun{
#' temp <- ISCN4()
#' }
ISCN4 <- function(dataDir=NULL, onlyNewData=TRUE, verbose=FALSE){
 
  
  ###### dowload and datafiles identified #########
  delete_dataDir <- is.null(dataDir)
  if(is.null(dataDir)){
    dataDir <- tempdir()
  }
  
  download.url <- c(Treat='http://iscn.fluxdata.org/wp-content/uploads/sites/23/2018/08/ISCNtemplate_Treat_peatProps_v2.xlsx',
                    Alamos='http://iscn.fluxdata.org/wp-content/uploads/sites/23/2018/08/ISCNtemplate_Alamos.xlsx')
  datafile.arr <- file.path(dataDir, basename(download.url))
  names(datafile.arr) <- names(download.url)
  
  ###### Download the data #########
  for(dataset in names(datafile.arr)){
    if(!file.exists(datafile.arr[dataset])){
      if(verbose)print('downloading Treat and Alamos...')
      download.file(download.url[dataset], datafile.arr[dataset] , quiet=FALSE)
      if(verbose)print('done')
    }
  }
  
  ######Make the keys #####
  keys.ls <- makeKeys()
  
  
  ##Treat: Metadata is flipped, dealwith this####
  tempMeta <- readxl::read_excel(path=datafile.arr['Treat'], sheet='metadata',
                                 col_names = c('V1', 'V2', 'V3', 'V4'), col_types = 'text',
                                 na = c('', 'NA'))
  Treat.ls <- list(metadata = data.table::as.data.table(t(tempMeta[,4])), #only data in column 4
                   site = data.table::as.data.table(readxl::read_excel(path=datafile.arr['Treat'], sheet='site',
                                                           col_types = 'text', na = c('', 'NA')))[-(1:2),],
                   profile = data.table::as.data.table(readxl::read_excel(path=datafile.arr['Treat'], sheet='profile',
                                                           col_types = 'text', na = c('', 'NA')))[-(1:2),],
                   layer = data.table::as.data.table(readxl::read_excel(path=datafile.arr['Treat'], sheet='layer',
                                                           col_types = 'text', na = c('', 'NA')))[-(1:2),])
  ######Treat: pull the names from the first column of metadata####
  names(Treat.ls$metadata) <- unlist(tempMeta[,1])
  
  ###Treat: Strip the empty columns #####
  emptySites <- lapply(Treat.ls, function(yy){lapply(yy, function(xx){all(is.na(xx))})})
  Treat.ls$metadata <- Treat.ls$metadata[,names(emptySites$metadata[emptySites$metadata == FALSE]), with=FALSE]
  Treat.ls$site <- Treat.ls$site[,names(emptySites$site[emptySites$site == FALSE]), with=FALSE]
  Treat.ls$profile <- Treat.ls$profile[,names(emptySites$profile[emptySites$profile == FALSE]), with=FALSE]
  Treat.ls$layer <- Treat.ls$layer[,names(emptySites$layer[emptySites$layer == FALSE]), with=FALSE]
  
  
  ####set up missing ids in Treat ####
  #TreatLong.ls$layer$dataset_name_id <- 'P2C2 Synthesis: Peat properties'
  #TreatLong.ls$profile$dataset_name_id <- 'P2C2 Synthesis: Peat properties'
  
  dataset_name <- paste0(regmatches(Treat.ls$site$add_note, regexpr("^(\\w|')+", Treat.ls$site$add_note)),
        regmatches(Treat.ls$site$add_note, regexpr('((20)|19)\\d\\d', Treat.ls$site$add_note)),
        '_',regmatches(Treat.ls$site$add_note, regexpr('\\d+$', Treat.ls$site$add_note)))
  
  # Treat.ls$site$citation <- Treat.ls$site$add_note
  #Treat.ls$site$add_note <- NULL
  
  
  #check headers
  #unique(unlist(lapply(Treat.ls, names)))[!unique(unlist(lapply(Treat.ls, names))) %in% keys.ls$ISCN2016$header]
  
  #####Cast as long format####
  if(verbose)print('casting Treat to long...')
  TreatLong.ls <- formatLongTable(data.ls = Treat.ls, 
                                  sourceKey = keys.ls$ISCN2016[header %in% unlist(lapply(Treat.ls, names)),], 
                                  targetKey = keys.ls$ISCN, verbose = verbose)
  
  if(verbose) print(lapply(TreatLong.ls, function(x) format(object.size(x), unit='Mb')))
  if(verbose)print('done')

  ####### Alamose has some formatting issues so let's deal with that first ####
  tempMeta <- readxl::read_excel(path=datafile.arr['Alamos'], sheet='metadata',
                                 col_names = c('V1', 'V2', 'V3', 'V4'), col_types = 'text',
                                 na = c('', 'NA'))
  Alamos.ls <- list(metadata = data.table::as.data.table(t(tempMeta[,4])), #only data in column 4
                   site = data.table::as.data.table(readxl::read_excel(path=datafile.arr['Alamos'], sheet='site',
                                                           col_types = 'text', na = c('', 'NA')))[-(1:3),],
                   profile = data.table::as.data.table(readxl::read_excel(path=datafile.arr['Alamos'], sheet='profile',
                                                           col_types = 'text', na = c('', 'NA')))[-(1:3),],
                   layer = data.table::as.data.table(readxl::read_excel(path=datafile.arr['Alamos'], sheet='layer',
                                                           col_types = 'text', na = c('', 'NA')))[-(1:3),])
  ######Alamos: pull the names from the first column of metadata####
  names(Alamos.ls$metadata) <- unlist(tempMeta[,1])
  
  ###Alamos: Strip the empty columns #####
  emptySites <- lapply(Alamos.ls, function(yy){lapply(yy, function(xx){all(is.na(xx))})})
  Alamos.ls$metadata <- Alamos.ls$metadata[,names(emptySites$metadata[emptySites$metadata == FALSE]), with=FALSE]
  Alamos.ls$site <- Alamos.ls$site[,names(emptySites$site[emptySites$site == FALSE]), with=FALSE]
  Alamos.ls$profile <- Alamos.ls$profile[,names(emptySites$profile[emptySites$profile == FALSE]), with=FALSE]
  Alamos.ls$layer <- Alamos.ls$layer[,names(emptySites$layer[emptySites$layer == FALSE]), with=FALSE]
  
  
  
   #####Cast Alomos as long format####
  if(verbose)print('casting Alamos to long...')
  AlamosLong.ls <- formatLongTable(data.ls = Alamos.ls, 
                                  sourceKey = keys.ls$ISCN2016, 
                                  targetKey = keys.ls$ISCN, verbose = verbose)
  
  if(verbose) print(lapply(AlamosLong.ls, function(x) format(object.size(x), unit='Mb')))
  if(verbose)print('done')
  
  ##Set up the missing ids that didn't get entired in the orginal Alamos file####
  AlamosLong.ls$layer$dataset_name_id <- factor('Alamos')
  AlamosLong.ls$layer$site_name_id <- factor('Alamos')
  AlamosLong.ls$profile$dataset_name_id <- factor('Alamos')
  AlamosLong.ls$profile$site_name_id <- factor('Alamos')
  
  ###Put Treat and Alamos together####
  data.ls <- list(
    study = data.table::rbindlist(list(TreatLong.ls$study, AlamosLong.ls$study), fill = TRUE),
    profile = data.table::rbindlist(list(TreatLong.ls$profile, AlamosLong.ls$profile), fill = TRUE),
    layer = data.table::rbindlist(list(TreatLong.ls$layer, AlamosLong.ls$layer), fill = TRUE))
  
  lapply(data.ls, function(xx){xx[,collection_name_id := factor('ISCN4')]})
  
  ##Add the collection level information for ISCN4
  data.ls$collection <- data.table::data.table(collection_name_id = 'ISCN4', 
                                                                  variable = 'collection_citation',
                                                                  entry = 'In Prep',
                                                                  type = 'value')
  
  hardKeys <- keys.ls$ISCN2016[!is.na(entry)  & !is.na(variable), c('variable', 'type', 'entry')]
  hardKeys[,collection_name_id := factor('ISCN4')]
  data.ls$collection <- data.table::rbindlist(list(data.ls$collection, hardKeys), fill = TRUE)
  
  
  if(!onlyNewData){
    if(verbose)print('loading ISCN3...')
    ISCN <- ISCN3(dataDir = dataDir, orginalFormat = FALSE, verbose=verbose)
    if(verbose)print('done')
    
    data.ls <- list(
      collection = data.table::rbindlist(list(data.ls$collection, ISCN$collection), fill = TRUE),
      study = data.table::rbindlist(list(data.ls$study, ISCN$study), fill = TRUE),
      profile = data.table::rbindlist(list(data.ls$profile, ISCN$profile), fill = TRUE),
      layer = data.table::rbindlist(list(data.ls$layer, ISCN$layer), fill = TRUE))
  }
  
   
  return(data.ls)
}
