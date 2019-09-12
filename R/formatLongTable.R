#' Convert to internal data format
#' 
#' This function converts a given list of relational data tables to an internal quasi-long format
#'
#' @param data.ls list of source relational data tables
#' @param sourceKey table describing the source relational data tables in \code{data.ls}. Describes what variable is associated with which source header and table and what kind of information is in that column as well as any hard values associated with a given variable
#' @param targetKey table describing the target data tables (what table to put the variables in)
#' @param tableOrder default order in wihc to process the source tables
#' @param verbose boolean flagging lots of output
#'
#' @importFrom  data.table melt.data.table setcolorder
#' 
#' @return a list of data tables in the interal quasi-long format
#' @export
formatLongTable <- function(data.ls, sourceKey, targetKey, 
                            tableOrder = c('study', 'site'),
                            verbose=FALSE){
  key <- merge(data.table::as.data.table(targetKey)[,c('table', 'variable')], 
               data.table::as.data.table(sourceKey)[,c('table', 'header', 'entry', 'type', 'variable')],
               by=c('variable'), suffixes = c('_target', '_source'),
               all=TRUE)
  
  ##Check that the data.ls tables are data.tables
  if(!all(unlist(lapply(data.ls, is.data.table)))){
    stop('data.ls must be data.table')
  }
  
  ##check for headers that are not matched with table targets
  danglingHeader <- key[(!is.na(header) & is.na(table_target)) | is.na(variable)]
  if(nrow(danglingHeader) > 0){
    warning(paste('Dangling headers in source table that will be dropped:', 
                  paste( unique(danglingHeader$header), collapse = ' ')))
  }
  
  missingHeader <- key[(is.na(header) & !is.na(table_target)) | is.na(variable)]
  if(nrow(missingHeader) > 0){
    warning(paste('Dangling headers in the target table that will be dropped:',
                  paste(unique(missingHeader$variable), collapse = ' ')))
  }
  
  missingType <- key[is.na(type)]
  if(nrow(missingType) > 0){
    warning(paste('Missing type definition in source key, will drop:',
                  paste(missingHeader$header, collapse=' ')))
  }
  
  ##keep only keys that are well defined
  key <- key[!is.na(header) & !is.na(table_target) & !is.na(variable) & !is.na(type)]
  
  
  ##construct the resulting long table format with specified ID keys
  ans <- list()
  #for each target table, force an ordering
  for(targetTbl in c(tableOrder, base::setdiff(unique(key$table_target), tableOrder))) {
    if(!(targetTbl %in% key$table_target)){
      
      next()
    }
    
    ans[[targetTbl]] <- data.table::data.table()
    orderSource <- sort(table(key[table_target == targetTbl]$table_source))
    for(sourceTbl in names(orderSource)){
      #pull the relvant table
      xx <- key[table_source == sourceTbl & table_target == targetTbl]
      
      #Pull all the columns indicated by the key
      columnNames <- base::intersect((xx[xx$header != '',])$header, names(data.ls[[sourceTbl]]))
      
      ##pull out unique data
      sourcedata <- unique((data.ls[[sourceTbl]])[,..columnNames])
      
      
      idVars <- (xx[xx$type == 'id',])$header
      names(idVars) <- paste0((xx[xx$type == 'id',])$variable, '_id')
      
      if(any(xx$type != 'id')){
        sourcedata <- melt(sourcedata, id.vars = idVars, 
                           variable.name = 'header', value.name='entry', na.rm=TRUE)
        
        temp <- merge(key[type != 'id',c('variable', 'header', 'type')], sourcedata, 
                      by='header', allow.cartesian = TRUE)
        #temp$entry <- paste0(temp$entry.x, temp$entry.y)
        selectCols <- c(idVars, 'header', 'entry', 'variable', 'type')
        sourcedata <- temp[,..selectCols]
        
        names(sourcedata) <- c(names(idVars), 'header', 'entry', 'variable', 'type')
      }else{
        ##keep the tables with only id headers incase we are linking headers across multiple source tables
        names(sourcedata) <- names(idVars)
      }
      ##merge it with the data from other tables
      if(nrow(ans[[targetTbl]]) == 0){
        ans[[targetTbl]] <- sourcedata
      }else{
        ans[[targetTbl]] <- rbind(ans[[targetTbl]], sourcedata, fill=TRUE)
      }
      
    }
    
    ##Mutate all the non-entry colums to factors
    cols <- setdiff(names(ans[[targetTbl]]), c('entry'))
    ans[[targetTbl]][,(cols) := lapply(.SD, as.factor), .SDcols=cols]
    
    ##only keep unique values
    ans[[targetTbl]] <- unique(ans[[targetTbl]])
  }
  
  return(ans)
  
}