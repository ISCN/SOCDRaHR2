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
#' @importFrom  data.table melt.data.table setcolorder rbind merge
#' 
#' @return a list of data tables in the interal quasi-long format
#' @export
formatLongTable <- function(data.ls, sourceKey, targetKey, 
                            tableOrder = c('study', 'site'),
                            warnMismatch = FALSE, verbose=FALSE){
  key <- merge(data.table::as.data.table(targetKey)[,c('table', 'variable')], 
               data.table::as.data.table(sourceKey)[,c('table', 'header', 'entry', 'type', 'variable')],
               by=c('variable'), suffixes = c('_target', '_source'),
               all=TRUE)
  
  ##Check that the data.ls tables are data.tables
  if(!all(unlist(lapply(data.ls, data.table::is.data.table)))){
    stop('data.ls must be data.table')
  }
  
  ##check for headers that are not matched with table targets
  danglingHeader <- key[(!is.na(header) & is.na(table_target)) | is.na(variable)]
  if(nrow(danglingHeader) > 0 & warnMismatch){
    warning(paste('Dangling headers in source table that will be dropped:', 
                  paste( unique(danglingHeader$header), collapse = ' ')))
  }
  
  missingHeader <- key[(is.na(header) & !is.na(table_target)) | is.na(variable)]
  if(nrow(missingHeader) > 0 & warnMismatch){
    warning(paste('Dangling headers in the target table that will be dropped:',
                  paste(unique(missingHeader$variable), collapse = ' ')))
  }
  
  missingType <- key[is.na(type)]
  if(nrow(missingType) > 0 & warnMismatch){
    warning(paste('Missing type definition in source key, will drop:',
                  paste(unique(missingHeader$header), collapse=' ')))
  }
  
  ##keep only keys that are well defined
  key <- key[!is.na(header) & !is.na(table_target) & !is.na(variable) & !is.na(type)]
  
  
  ##construct the resulting long table format with specified ID keys
  ans <- list()
  #for each target table, force an ordering
  for(targetTbl in c(tableOrder, base::setdiff(unique(key$table_target), tableOrder))) {
    if(verbose) cat(paste('Processing for target:',targetTbl))
    if(!(targetTbl %in% key$table_target)){
      if(verbose) cat('target not found in key... moving on.\n')
      next()
    }
    
    ans[[targetTbl]] <- data.table::data.table()
    orderSource <- sort(table(key[table_target == targetTbl]$table_source))
    for(sourceTbl in names(orderSource)){
      if(verbose) cat(paste('\tData from source table:', sourceTbl, '\n'))
      #pull the relvant table
      xx <- key[table_source == sourceTbl & table_target == targetTbl]
      if(verbose) cat(paste('\tusing key:\n'))
      if(verbose) print(xx)
      
      #Pull all the columns indicated by the key
      if(verbose) cat(paste('data columns [', paste(names(data.ls[[sourceTbl]]), collapse=', '), ']\n' ))
      columnNames <- base::intersect((xx[xx$header != '',])$header, names(data.ls[[sourceTbl]]))
      if(verbose) cat(paste('select columns [', paste(columnNames, collapse = ', '), ']\n'))
      
      ##pull out unique data
      sourcedata <- unique((data.ls[[sourceTbl]])[,..columnNames])
      
      
      idVars <- (xx[xx$type == 'id',])$header
      names(idVars) <- paste0((xx[xx$type == 'id',])$variable, '_id')
      
      if(any(xx$type != 'id')){
        sourcedata <- data.table::melt.data.table(sourcedata, id.vars = idVars, 
                           variable.name = 'header', value.name='entry', na.rm=TRUE)
        
        temp <- merge(key[type != 'id',c('variable', 'header', 'type')], sourcedata, 
                      by='header', allow.cartesian = TRUE)
        #temp$entry <- paste0(temp$entry.x, temp$entry.y)
        selectCols <- c(idVars, 'header', 'entry', 'variable', 'type')
        sourcedata <- temp[,..selectCols]
        
        names(sourcedata) <- c(names(idVars), 'header', 'entry', 'variable', 'type')
      }else{
        ##There is no data here... moving on
        next
      }
      
      ##merge it with the data from other tables
      if(nrow(ans[[targetTbl]]) == 0){
        if(verbose)print(paste('starting new target table:', targetTbl))
        ans[[targetTbl]] <- sourcedata
      }else{
        if(verbose)print('adding to existing target table...')
        sourceID <- names(idVars)
        targetID <- setdiff(names(ans[[targetTbl]]), c('header', 'entry', 'variable', 'type'))
        if(!setequal(sourceID, targetID)){
          if(verbose){print(paste(targetTbl, 'names do not match sourcedata:[', paste0(sourceID, collapse=','), '] != [',
                                  paste0(targetID, collapse = ', '),']'))}
          if(length(sourceID) > length(targetID)){
            sourceID <- names(idVars)
            if(verbose) print(paste('merging sourcedata columns [', paste0(sourceID, collapse=', '), '] into ans$', targetTbl, 
                                    'with columns [', paste0(targetID, collapse=','), '] by columns [',
                                    paste0(base::intersect(targetID, sourceID), collapse=', '), ']'))
     
            #there are extra id columns in the sourcedata
            ans[[targetTbl]] <- merge(ans[[targetTbl]], unique(sourcedata[,..sourceID]), by=base::intersect(targetID, sourceID), all=TRUE)
          }else{
            #there are extra id columns in the ans table
            sourcedata <- merge(sourcedata, ans[[targetTbl]][,..targetID], all=TRUE)
          }

          #bind the data tables
        }
        ans[[targetTbl]] <- rbindlist(list(ans[[targetTbl]], sourcedata), use.names=TRUE)
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