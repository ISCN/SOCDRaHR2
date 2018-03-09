#' Simple read-in for small datasets
#' 
#'  This function will read in a generic data file and convert it a long data table and a wide data table with the assistance of a key. This is intended for relatively small data sets, if your data is more then 10K layers (with an expected data load >10Mb) consider writing your own function.
#'
#' @param filename characters identifying an Excel file with worksheets or set of csv files to be read in
#' @param key.df data frame converting headers to variables. 
#' @param excludeSheets list of sheets in Excel workbook not to read in
#' @param verbose a flag to print out helpful statements
#' @param verticalTable a list of sheets or file names with data by column instead of by row. Typically this is metadata where a single entry is expected.
#' @param dropRows rows to be dropped after the data is read in. Typically these are descritions below the headers.
#' @param skipRows the rows (or columns) to skip because they are human readable descriptions or units. The header will be read as being after these rows.
#'
#' @return a list of a wide and long data tables with a copy of the final key
#' 
#' @importFrom readxl excel_sheets read_excel
#' @importFrom assertthat assert_that
#' @export
#'
readKeyedData <- function(filename=NA, key.df=NA,
                                verticalTable=c('metadata'),
                                excludeSheets=c('controlled vocabulary'),
                                skipRows=0, dropRows=NA, verbose=FALSE){
  
  ### Check input file(s) ###
  assertthat::assert_that(all(!is.na(filename)) & all(!is.null(filename)), 
              msg='Must specify filename for input, if file does not exist then function will attempt to download from download_url specificed in key.df.')
  
  ### check file type ###
  readingCSVfiles <- all(grepl('\\.csv', filename))
  readingEXCELworkbook <- all(grepl('\\.xls', filename)) & length(filename) == 1
  
  if(readingCSVfiles){
    numIterate <- length(filename)
    if(any(filename %in% verticalTable)){
      verticalTable <- which(filename %in% verticalTable)
    }else{
      verticalTable <- -1
    }
  }else if(readingEXCELworkbook){
    numIterate <- length(readxl::excel_sheets(filename))
    if(any(readxl::excel_sheets(filename) %in% verticalTable)){
      verticalTable <- which(readxl::excel_sheets(filename) %in% verticalTable)
    }else{
      verticalTable <- -1
    }
  }else{
    stop('Invalide file format, must be csv files or an excel workbook')
  }
  
  ### Check key.df structure ###
  assertthat::assert_that(all(c('header', 'var', 'flagID', 'softType') %in% 
                    names(key.df)),
              msg='Missing some key names')
  
  assertthat::assert_that('value' %in% key.df$softType, 
              msg='"value" must be in key.df$softType')
  
  #### Trim key to only read in matched vars ####
  if(verbose) {print(paste('dropping', sum(is.na(key.df$var)), 'unpaired headers.'))}
  key.df <- key.df %>%
    filter(!is.na(var))
  
  ####Expand the regular expressions in the key ####
  varsToExpand <- key.df %>%
    filter(softType != 'value' & !is.na(softType), #pull softTypes that are not values
           #If var doesn't match value vars assume they are regular expressions
           !var %in% (key.df %>% filter(softType == 'value'))$var) %>%
    group_by(header) 
  
  if(nrow(varsToExpand) > 0){
    key.df <- varsToExpand %>%
      do((function(xx=.,
                   varlist = (key.df %>% filter(!is.na(var) & softType == 'value'))$var){
        return(data.frame(fullVar=varlist[grepl(xx$var, varlist)],
                          stringsAsFactors=FALSE))
      })(.)) %>% #fill in the vars that match the regular expression
      right_join(key.df, by='header') %>% #merge back into main key
      group_by(header, dataframe, softType) %>%
      mutate(var = if_else(is.na(fullVar), var, fullVar)) %>%
      arrange(var) %>%
      ungroup() %>%
      select(-fullVar)
  }
  
  ### Check that all vars are either long or wide and there are wide ids flaged ###
  assertthat::assert_that(any(is.na(key.df$softType) & !is.na(key.df$flagID)), 
             msg='A long data type can not be a flaged id')
  assertthat::assert_that(any(!is.na(key.df$softType) & is.na(key.df$flagID)),
             msg='There must be a flagged id for the wide data types')
  
  
  wide.df <- NULL
  long.df <- NULL
  
  for(tableIndex in 1:numIterate){
    if(verbose) print(paste('reading table', tableIndex))
    ### read in orginal data table ###
    if(readingCSVfiles){
      data.df <- read_csv(filename[tableIndex], skip=skipRows, 
                          col_names= tableIndex != verticalTable)
    }else{
      if(readxl::excel_sheets(filename)[tableIndex] %in% excludeSheets){
        next()
      }
      data.df <- readxl::read_excel(filename, sheet=tableIndex, skip=skipRows,
                                    col_names= tableIndex != verticalTable)
    }
    
    ### flip things around if it's a vertical format ###
    if(tableIndex == verticalTable){
      data.df <- bind_rows(data.df, setNames(names(data.df), names(data.df)))
      data.df <- as.data.frame(t(data.df), stringsAsFactors=FALSE)
      names(data.df) <- data.df[1,]
      data.df <- data.df[-1,]
    }
    
    ### drop any preidentified rows and empty columns ###
    if(all(!is.na(dropRows))){
      data.df <- data.df[-1*dropRows,] 
    }
    
    ### pull the wide data ###
    wideKey.df <- key.df %>%
      filter(is.na(softType), ##define wide tables by absence of soft Type
             header %in% names(data.df)) ##only pull the headers we care about
    
    wideTemp <- data.df %>% 
      select_(quote(wideKey.df$header)) %>%
      select_if(~any(!is.na(.))) #drop empty columns
    
    if(verbose) print(paste('Wide headers to be merged:', 
                            paste(names(wideTemp), collapse=', ')))
    
    if(is.null(wide.df)){
      if(verbose) print('initalizing wide table')
      wide.df <- wideTemp
    }else if(length(intersect(names(wideTemp), names(wide.df))) == 0 &
             (nrow(wideTemp) == 1 | nrow(wide.df) == 1)){
      if(verbose) print('binding to wide table')
      wide.df <- data.frame(wide.df, wideTemp) #column bind what is a single row
    }else{
      if(verbose) print('full_join with wide table')
      wide.df <- full_join(wide.df, wideTemp) #merge data based on common names
    }
    
    ### pull the long table ###
    longKey.df <-  key.df %>%
      filter(!is.na(softType) | !is.na(flagID), #define long table by their soft type
                                                #...identification and cross id by flagID
             header %in% names(data.df)) # only look appropreate headers
    if(nrow(longKey.df) == 0 | 
       all(is.na(longKey.df$flagID)) |
       all(!is.na(longKey.df$flagID))){
      next() ##go on to the next table, there isn't anything here or 
             ##...if there are no id keys or it's only id
    }
 
    longTemp <- data.df %>%
      ##pull the long headers
      select_(quote(longKey.df$header)) %>% 
      ##group by the headers flagged as ids
      group_by_at(vars(one_of(longKey.df$header[!is.na(longKey.df$flagID)]))) %>%
      ##gather up all the non-id columns into two columns
      gather(key=header, value=value, 
             one_of(longKey.df$header[is.na(longKey.df$flagID)]), na.rm=TRUE) 
    
    if(nrow(longTemp) == 0){
      next()
    }
    longTemp <- longTemp %>% 
      ##key out what type of data is in each column
      left_join(longKey.df %>% select(header, var, softType), by='header') %>%
      ##merge columns of the same var-type (ie multi-columns defining methods)
      group_by(var, softType, add=TRUE) %>% unique %>%
      summarize(value=ifelse(length(value) == 1 | all(softType == 'value'), 
                             value,
                             paste(header, value, sep=':', collapse='; '))) %>%
      ##regroup by only the id columns and var
      ungroup() %>%
      group_by_at(vars(one_of(longKey.df$header[!is.na(longKey.df$flagID)])),
                  add=FALSE) %>% 
      group_by(var) %>%
      ##spead the type-value into 'value, sigma, unit, method' columns
      spread(key=softType, value=value) %>%
      ##filter out things that are never given a value
      filter(!is.na(value)) 
      
      ##append it to the data from previous tables
      long.df <- longTemp %>%
        bind_rows(long.df)
    
  }
  
  ### rename header w/ variables ###
  wide.df <- wide.df %>%
    setNames( setNames(key.df$var, key.df$header)[names(.)])
  
  long.df <- long.df %>%
    setNames( setNames( c(key.df$var, 'var', unique(key.df$softType)),
                        c(key.df$header, 'var', unique(key.df$softType)) )[names(.)])
  
  return(list(wide=wide.df, long=long.df, key=key.df))
}
