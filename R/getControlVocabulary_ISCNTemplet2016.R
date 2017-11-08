getControlVocabulary_ISCNTemplet2016 <- function(pdfDoc){
  library(pdftools)
  library(dplyr)
  library(tidyr)
  #textStream <- pdf_text('../soils-long-tail-recovery/templates/ISCN2016_TemplateControlVocab-2.pdf')
  textStream <- pdf_text(pdfDoc)
  lineStream <- strsplit(textStream, '\\n')

  ##Pull the disclaimer
  header <- lineStream[[1]][1:8]
  lineStream[[1]] <- lineStream[[1]][-1:-9]

  ans <- plyr::ldply(lineStream, function(xx){
    plyr::ldply(xx, function(yy){
      temp <- unlist(strsplit(yy, '\\s{2,}'))
      #return(yy)
      if(!grepl(':', temp[1]) & !grepl('^\\s*$', temp[1])){
        temp <- c('', temp)
      }
      return(data.frame(X1 = temp[1],
                        X2 = temp[2],
                        X3 = temp[3], stringsAsFactors=FALSE))
    })
    })

  ##Deal with bad reads as progamatically as possible
  badrow <- which(grepl('(equivalent to ppm)',ans[,2]))
  ans[badrow-1, 3] <- paste(as.character(ans[badrow-1, 3]), '(equivalent to ppm)')
  ans <- ans[-badrow, ]

  barronIndex <- which(grepl('Barren Land \\(Rock/Sand/Clay\\)', ans[,2]))
  ans[barronIndex, 3] <- gsub('Barren Land \\(Rock/Sand/Clay\\)', '', ans[barronIndex, 2])
  ans[barronIndex, 2] <- 'Barren Land (Rock/Sand/Clay)'
  landsatIndex <- which(grepl('landsat', ans[,1]))
  landscapeIndex <- which(grepl('landscape', ans[,1]))
  lastVocab <- landsatIndex+1
  removeRows <- rep(FALSE, (landscapeIndex-1)-(landsatIndex+1))
  for(index in (landsatIndex+1):(landscapeIndex-1)){
    if(is.na(ans[index,3])){
      ans[lastVocab, 3] <- paste(ans[lastVocab, 3], ans[index, 2])
      removeRows[index-landsatIndex] <- TRUE
    }else{
      lastVocab <- index
    }
  }
  ans <- ans[-1*((landsatIndex+1):(landscapeIndex-1))[removeRows],]


  variableKey <- data.frame(X1 = unique(ans$X1), stringsAsFactors=FALSE) %>%
    mutate('ControlVariableName' = gsub(' used by :.*$', '', X1),
           'usedBy' = gsub('^.* used by :', '', X1)) %>%
    separate(usedBy, into=paste0('Y', 1:5), sep='[^[:alnum:]_]+', fill='right') %>%
    gather(col, variable, Y1, Y2, Y3, Y4, Y5, na.rm=TRUE) %>%
    select(-col)

  ans <- ans %>%
    mutate(X1 = if_else(grepl('^\\s*$', X1), as.character(NA), X1)) %>%
    fill(X1, .direction='down') %>%
    filter(!is.na(X2)) %>%
    left_join(unique(variableKey %>% select(X1, ControlVariableName)), by='X1') %>%
    rename(vocabulary=X2, note=X3) %>%
    select(ControlVariableName, vocabulary, note)

  return(list(header=header, controlVocab = ans, variableKey=variableKey))
}
