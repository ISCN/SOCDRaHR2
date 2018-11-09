readPangaeaTabToDf <- function(downloadURL, destfile){
  downloadURL <- 'https://doi.pangaea.de/10.1594/PANGAEA.863689?format=textfile'
  destfile <- '~/Documents/GitHubRepo/soilDataR/temp/Treat2016_S1.tab'
  download.file(url=downloadURL, destfile = destfile)
  
  Treat <-  pangaear::pg_data(doi = c('10.1594/PANGAEA.863692',
                                      '10.1594/PANGAEA.863695', 
                                      '10.1594/PANGAEA.863689'))
  
  ####Try loading in metadata and the formating is a nightmare
  test <- readLines('temp/Treat2016_S1.tab')
  
  
  headerStart <- which(grepl('^/\\*', test))
  headerEnd <- which(grepl('^\\*/', test))
 
  ##Example: 
  treeString <- "Campbell_Creek * LATITUDE: 69.290000 * LONGITUDE: -133.250000"
  treeString <- "LATITUDE: 69.290000 * LONGITUDE: -133.250000"
  treeString <- "LATITUDE: 69.290000"
  
  which(!grepl('^\t', test))[1:10]
  ans <- list(Citation = unlist(strsplit(test[2], split='\t'))[2],
              `In supplement to` = unlist(strsplit(test[3], split='\t'))[2],
              `Related to` = c(unlist(strsplit(test[4], split='\t'))[2],
                               gsub('\t','',test[5:77])))
  currentListIndex <- length(ans)
  for(lineIndex in 80:82){
    if(grepl('^\t', test[lineIndex])){
      ans[[currentListIndex]] <- c(ans[[currentListIndex]], 
                                   makeTreeFromLine(gsub('^\t', '', test[lineIndex])))
    }else{
      currentListIndex <- currentListIndex + 1 
      splitLine <- unlist(strsplit(test[lineIndex], split=':\t'))
      ans[[currentListIndex]] <- makeTreeFromLine(splitLine[2])
      names(ans)[currentListIndex] <- splitLine[1]
    }
  }
  
  makeTreeFromLine <- function(treeString){
    #cat(treeString)
    if(!grepl(' \\* ', treeString) & grepl(': ', treeString)){ #<name>: <value>
      temp <- unlist(strsplit(treeString, ': '))
      #print('if 2')
      #if(length(temp) != 2) stop()
      return(setNames(as.list(makeTreeFromLine(temp[2])), temp[1]))
    }else if(grepl('^[^:]+ \\*', treeString)){ #<superName> * <name>: [...]
      listName <- sub(' \\* ', '', regmatches(x=treeString, m = regexpr('^[^:]+ \\* ', treeString)))
      daughterString <- sub('^[^:]+ \\* ', '', treeString)
      #print('if 3')
      return(setNames(makeTree(daughterString), listName))
    }else if(grepl('\\*', treeString)){ #[...] * [...]
      #print('if 4')
      return(list(unlist(lapply(as.list(unlist(strsplit(treeString, split=' \\* '))), makeTree))))
    }else{
      #print('if 5')
      return(treeString)
    }
  }
  
  #in the header a new line is a new list
  #break if you do not have a tab in the next line and return the current list
  # if there is a (\w:) block, create a new list named (\w) and parse the rest of the line
  # else if there is a * return values 
  header.ls <- list()
  listNumber <- 0
  makeListPairs <- 
  for(lineNumber in (headerStart+1):(headerEnd-1)){
    if(!grepl('^\t', test[lineNumber])){
      listNumber <- 1 + listNumber
      primaryHeader <- strsplit(test[lineNumber], ':\t')[[1]][1]
      
      header.ls[[listNumber]] <- read.delim(text=test[lineNumber], 
                                            header=FALSE, stringsAsFactors = FALSE)
      names(header.ls)[listNumber] <- header.ls[[listNumber]][1,1]
      header.ls[[listNumber]] <- header.ls[[listNumber]][,-1]
    }else{
      header.ls[[listNumber]] <- rbind(header.ls[[listNumber]],
                                       read.delim(text=test[lineNumber], 
                                                  header = FALSE, stringsAsFactors = FALSE)[,-1])
    }
  }
  #grepl('^\t', test[(headerStart+1):(headerEnd-1)])
  
  #header.arr <- strsplit(test[(headerStart+1):(headerEnd-1)], split='(:\\s)|\\*')
  
  temp <- as.tibble(read.delim(text=test[-1*(1:headerEnd)]))
  names(temp) <- unlist(strsplit(test[headerEnd+1], split='\t'))
  
  temp2 <- temp %>%
    group_by_at(vars(starts_with('ID'))) %>%
    gather(key='ColumnName', value='value_numeric', one_of(names(temp)[unlist(lapply(temp, is.numeric))])) %>%
    select(starts_with('ID'), ColumnName, value_numeric) %>%
    bind_rows(temp %>%
                group_by_at(vars(starts_with('ID'))) %>%
                gather(key='ColumnName', value='value_string', -one_of(names(temp)[unlist(lapply(temp, is.numeric))]), -starts_with('ID')) %>%
                select(starts_with('ID'), ColumnName, value_string))
  
  
}
