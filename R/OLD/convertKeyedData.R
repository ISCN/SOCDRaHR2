#' Convert data tables
#' 
#' This function converts a set of data tables from a specified format to a keyed format 
#' including renaming variables.
#'
#' @param data.ls list containing the data tables specified in the key
#' @param key.df the key or look-up table that converts one formate to another 
#' @param returnFormat string identifying the return type, can be either "long", "wide", or "3Table"
#'
#' @return a data frame of one of three formats: a long table, wide table, or 
#' 3 data tables specifying the sample, field, and study data
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom tibble as_tibble
#' @importFrom tidyr unite spread
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' rawData <- readCanandaUplandForest2018() #~7Mb
#' longData <- convertKeyedData(rawData, rawData$ISCN5_key, returnFormat = 'long') #~50Mb
#' wideData <- convertKeyedData(rawData, rawData$ISCN5_key, returnFormat = 'wide') #~7Mb
#' 3TableData <- convertKeyedData(rawData, rawData$ISCN5_key, returnFormat = '3Table') #~9Mb
#' }
convertKeyedData <- function(data.ls, key.df, returnFormat = c('3Table', 'long', 'wide')[1]){
  ### Check key.df structure ###
  assertthat::assert_that(all(c('orgTable', 'orgHeader', 'var',
                                'flagID', 'softType', 'dataframe') %in% 
                                names(key.df)),
                          msg='Missing some key names')
  
  assertthat::assert_that(all(returnFormat %in% c('3Table', 'long', 'wide')),
                          msg='Invalid return format specified')
  
  if(returnFormat == '3Table'){
    assertthat::assert_that(all(unique(key.df$dataframe) %in% c('sample', 'field', 'study', NA)),
                            msg='Unexpected dataframe specified in key.df')
  }
  
  #### Trim key to only read in matched vars ####
  key.df <- tibble::as_tibble(key.df) %>%
    dplyr::filter(!is.na(var)) %>% 
    tidyr::replace_na(list(softType='value')) #set defalut for softTypes
  
  ####Expand the regular expressions in the key ####
  varsToExpand <- key.df %>%
    dplyr::filter(softType != 'value' & !is.na(softType), #pull softTypes that are not values
           #If var doesn't match value vars assume they are regular expressions
           !var %in% (key.df %>% dplyr::filter(softType == 'value'))$var) %>%
    dplyr::group_by(orgHeader) 
  
  if(nrow(varsToExpand) > 0){
    key.df <- varsToExpand %>%
      dplyr::do((function(xx=.,
                   varlist = (key.df %>% filter(!is.na(var) & softType == 'value'))$var){
        return(data.frame(fullVar=varlist[grepl(xx$var, varlist)],
                          stringsAsFactors=FALSE))
      })(.)) %>% #fill in the vars that match the regular expression
      dplyr::right_join(key.df, by='header') %>% #merge back into main key
      dplyr::group_by(header, dataframe, softType) %>%
      dplyr::mutate(var = if_else(is.na(fullVar), var, fullVar)) %>%
      dplyr::arrange(var) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fullVar)
  }
  
  ##### Make everything long #####
  
  ##pull tables to read
  data.df <- NULL
  for(tableName in as.character(unique(key.df$orgTable))){
    #print(tableName)
    if(is.null(data.df)){
      data.df <- tibble::as_tibble(data.ls[[tableName]])
    }else{
      data.df <- dplyr::full_join(data.df, data.ls[[tableName]])
    }
  }
  
  ##pull ID headers
  header_to_select <- 
    list(sample=(key.df %>% 
                   dplyr::filter(grepl('(study)|(field)|(sample)', 
                                         as.character(key.df$flagID))))$orgHeader %>%
           as.character(),
         field=(key.df %>% 
                  dplyr::filter(grepl('(study)|(field)', 
                                        as.character(key.df$flagID))))$orgHeader%>%
           as.character(), 
         study=(key.df %>% 
                  dplyr::filter(grepl('study', 
                                        as.character(key.df$flagID))))$orgHeader %>%
           as.character())
  
  
  longTable <- data.df %>%
    dplyr::group_by_at(unique(unlist(header_to_select))) %>%
    dplyr::na_if("") %>%
    ##gather non-ID columns into 'header' and 'string'
    tidyr::gather(key='orgHeader', value='string_value', 
                  -one_of(unique(unlist(header_to_select))), na.rm=TRUE) %>%
    ##merge with key.df[header, var, type, class]
    dplyr::left_join(key.df %>% select('orgHeader', 'var', 'softType', 'dataframe')) %>%
    ##cast the numeric vs strings appropreately
    dplyr::mutate(number_value = if_else(grepl('\\d+', string_value),
                                         as.numeric(string_value), as.numeric(NA))) %>%
    ##cast the identifies as factors
    dplyr::ungroup() %>%
    dplyr::mutate_at(unique(unlist(header_to_select)), as.factor)%>%
    dplyr::group_by_at(unique(unlist(header_to_select)))

  if(returnFormat == 'long'){
    return(longTable)
  }
  
  if(returnFormat == 'wide'){
    #print('wide')
    wideTable <- longTable %>%
      ## make a header variable_type dropping the 'value' marker
      dplyr::filter(!is.na(var)) %>%
      tidyr::unite(newHeader, var, softType, sep=':') %>%
      dplyr::group_by(newHeader, add=TRUE) %>%
      dplyr::summarise(string_value = if_else(length(string_value) == 1,
       #below paste shouldn't actually be triggered but keeps the formal if_else from triggering
                                         paste(string_value, collapse='-'),
                                         paste(paste(orgHeader, string_value, sep=':'),
                                               collapse='-')))  %>%
      dplyr::group_by_at(unique(unlist(header_to_select)), add=FALSE) %>%
      tidyr::spread(key='newHeader', value='string_value')
    return(wideTable)
  }
  
  ##COnstruct the sample, field, and study tables
  ans <- list(sample = longTable %>% 
                dplyr::filter(dataframe == 'sample') %>% 
                dplyr::group_by(var, softType, add=TRUE) %>%
                dplyr::summarise(string_value = if_else(length(string_value) == 1,
                                                        #below paste shouldn't actually be triggered but keeps the formal if_else from triggering
                                                        paste(string_value, collapse='-'), 
                                                        paste(paste(orgHeader, string_value, sep=':'),
                                                              collapse='-'))) %>%
                dplyr::group_by_at(header_to_select$sample) %>%
                tidyr::spread(key=softType, value=string_value) %>%
                dplyr::ungroup() %>%
                dplyr::mutate_at('value', as.numeric) %>%
                dplyr::mutate_if(is.character, as.factor) %>%
                dplyr::group_by_at(header_to_select$sample))
  
  ### Make long stuff wide
  ans$field <- longTable %>%
    ## Only look at the field
    dplyr::filter(dataframe == 'field') %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(header_to_select$field) %>%
    dplyr::select(var, string_value) %>%
    unique() %>%
    tidyr::spread(key='var', value='string_value')# %>% 
    #ungroup() %>% 
    #mutate_if(~all(grepl('\\d+', .)), as.numeric) %>% 
    #mutate_if(is.character, as.factor)
  
  ans$study <- longTable %>%
    ## Only look at the study
    dplyr::filter(dataframe == 'study') %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(header_to_select$study) %>%
    dplyr::select(var, string_value) %>%
    unique() %>%
    tidyr::spread(key='var', value='string_value') #%>%
    #ungroup() %>% 
    #mutate_if(~all(grepl('\\d+', .)), as.numeric) %>% 
    #mutate_if(is.character, as.factor)
  
  if(returnFormat == '3Table'){
    return(ans)
  }
  
  return(NULL)
}