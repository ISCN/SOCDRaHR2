#' Convert Measurement, methods and units block
#'
#' This function will convert a long format with columns declared as methods, measurements, or units.
#'
#' @param methodColStrs an array of strings idenfiying the methods
#' @param unitName
#' @param org.df
#' @param unit.df
#' @param verbose
#'
#' @return
convert_MMU_Block <- function(methodColStrs=NULL, unitName=NULL,
           org.df, unit.df=NULL, verbose=FALSE){

    ##Merge methods
    org.df$method <- ''
    for(methodStr in methodColStrs){
      patternStr <- sprintf('%%s %s: %%s', methodStr)
      emptyStr <- sprintf('\\s+%s:\\s+$', methodStr)
      org.df$method <- sprintf(patternStr, org.df$method, org.df[,methodStr])
      org.df$method <- gsub(emptyStr, '', org.df$method)
      org.df$method <- gsub('^\\s+', '', org.df$method)
      org.df[,methodStr] <- NULL
    }
    if(verbose) cat('flag1')
    #reshape2::melt values
    org.df <- unique(reshape2::melt(org.df, id.vars=c(c('fieldID', 'method'), unitName),
                                        variable.name='measurement',
                                        na.rm=TRUE, forceNumericValue=TRUE))
    if(verbose) cat('flag2')
    if(!is.null(unitName)){
      names(org.df)[3] <- 'unit'
    }else if(is.null(unitName) & !is.null(unit.df)){
      #units
      org.df <- merge(org.df, unit.df[,c('measurement', 'unit')])
    }else{
      warning('units not specified')
    }
    if(verbose) cat('flag3')
    #issolate unique measurements
    measurementTemp <- unique(org.df[c('measurement','method')])
    measurementTemp$measurementID <- sprintf('%s_%02d', measurementTemp$measurement,
                                             1:nrow(measurementTemp))
    if(verbose) cat('flag4')
    org.df <- merge(org.df, measurementTemp)[, c('fieldID', 'measurementID',
                                                         'value', 'unit')]

    #rename stuff
    names(measurementTemp)[1] <- 'type'

    org.df$value <- as.numeric(org.df$value)
    if(verbose) cat('flag5')
    org.df <- org.df[org.df$value != -999, ]
    if(verbose) cat('flag6')
    return(list(sample=org.df, measurement=measurementTemp))

  }
