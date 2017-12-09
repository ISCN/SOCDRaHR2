#' This function will read in a generic data file and convert it to the internal format with the assistance of a key.
#'
#' @param filename characters identifying an excel file with worksheets to be read in
#' @param skip the columns or rows to skip because they are human readable descriptions or units. Note that 1 is the first row/column after the header.
#' @param key.df data frame converting headers to variables. key.df must have the following fields: header, var, type, class, dataframe. 'headers' must match the headers in the data file. 'var' should be the final name of the long format sample data or regular expressions that will match to the orginal header to apply the same methods notes to. 'type' flags the type of information in the column; note that while there can be multiple 'method' associated with a variable, there should only be one 'value' and 'sigma'; NA signals a wide table format that will be a header in the final output. 'dataframe' identifies what final data frame to put the header in (currently can only be 'study', 'field', 'treatment', 'sample'); NA will result in the column being dropped.
#' @param verticalSheets an array of names for the data sheets that are vertical (ie names are in column 1 instead of row 1)
#' @param excludeSheets TODO
#' @param idRegEx TODO
#' @param templetCasting TODO
#' @param verbose a flag to print out helpful statements
#'
#' @return a list of the dataframes identified by 'dataframe' and a copy of the final key
#' @export
#'
#' @examples
#' key.df <- data.frame(header=c('site_name', 'profile_name', 'layer_top', 'layer_bottom', 'SOC', 'SOC notes'),
#'                         var=c('site_name', 'profile_name',  'layer_top',  'layer_bottom','soc',  'soc'),
#'                         class=c('factor',    'factor',       'numeric',   'numeric',  'numeric','character'),
#'                         type=c(NA,         NA,              NA,          NA,            'value', 'method'),
#'                    dataframe=c('field',     'field',        'field',    'field',       'sample', 'sample'))
#'
processData_Templet <- function(filename='repoData/test/ISCNtemplate.xlsx', key.df=NA,
                                        verticalSheets=c('disturbance', 'metadata'),
                                excludeSheets=c('controlled vocabulary'),
                                idRegEx = '_name$',
                                skip=c(1,2), templetCasting=FALSE, verbose=FALSE){

  ##TODO check to see if there are more then one value associated with a var
  ##TODO Accept an arbitrary dataframe ID
  ##TODO be more clever about indexing ans$treatment appropreately so we aren't going through the whole '_name'
  ##...to identify a treatment

  primaryNameStr <- 'site_name' ##TODO finish abstracting this out

  # ##dev file
  # detach(parms)
  # parms <- list(filename='repoData/test/ISCNtemplate.xlsx', key.df=NA,
  #               verticalSheets=c('disturbance', 'metadata'),
  #               excludeSheets=c('controlled vocabulary'),
  #               idRegEx = '_name$',
  #               skip=c(1,2), templetCasting=FALSE, verbose=FALSE)
  # attach(parms)
  # filename <- '../soils-long-tail-recovery/repoData/Treat_2015/ISCNtemplate_Treat_peatProps_v2.xlsx'
  # #filename <- '../soils-long-tail-recovery/repoData/Berhe2012/Berhe_2012.xlsx'
  # keyfile <- '../soils-long-tail-recovery/templates/ISCNtemplate_2016Key.xlsx'
  # key.df <- read_excel(path=keyfile, sheet='headerKey')
  # #key.df <- readxl::read_excel(path='../soils-long-tail-recovery/templates/PowellCenterKey.xlsx', sheet='headerKey')
  # #verticalSheets <- c()
  # #skip <- NA
  #

  assert_that(all(key.df$type %in% c('value', 'method', 'sigma', 'unit', NA)), msg='Unexpected type names')
  assert_that(all(key.df$dataframe %in% c('study', 'field', 'treatment', 'sample', NA)), msg='Unexpected dataframe names')
  assert_that(all(c('header', 'dataframe', 'class', 'type', 'hardUnit') %in% names(key.df)), msg='Missing key names')

  ####Expand the regular expressions in the key ####
  unitVars <- unique(filter(key.df, type == 'value')$var)
  key.df <- key.df %>%
    group_by(header, dataframe, class, type, hardUnit) %>%
    ##Expand regular expressions in 'var' (as defined by appearence of ^|)
    do((function(xx){
      if(grepl('\\^|\\|\\$', xx$var)) #check for regular expression
        #return all variables that match
        return(data.frame(var=as.character(unitVars[grepl(xx$var, unitVars)]), stringsAsFactors=FALSE))
      else
        #do nothing
        return(data.frame(var=xx$var, stringsAsFactors=FALSE))
    })(.)) %>%
    arrange(var) %>%
    ungroup()
  
  if(verbose){print('key.df: '); print(key.df)}

  ### pull the final header names
  wideHeaders <- setNames(filter(key.df, is.na(type) | type == 'header')$var, #values
                            filter(key.df, is.na(type) | type == 'header')$header) #names
  wideHeaders <- wideHeaders[!is.na(wideHeaders) & !is.na(names(wideHeaders))]
  if(verbose){print('to be renamed - wideHeaders: '); print(wideHeaders)}
  #### Read in all the data either to key wide format or for future reformating as long
  bigWide.df <- NA
  futureLong.df <- data.frame()
  
  for(sheetName in setdiff(excel_sheets(filename), excludeSheets)){
    if(verbose) print(paste('reading sheet: [', sheetName, ']'))
    if(sheetName %in% verticalSheets){
      ##Read in by column data
      temp <- readxl::read_excel(path=filename, sheet=sheetName, col_types = 'text', col_names=FALSE)
      temp <- as.data.frame(t(temp), stringsAsFactors=FALSE)
      names(temp) <- temp[1,]
      temp <- temp[-1,]
    }else{
      ##Read in by row data
      temp <- readxl::read_excel(path=filename, sheet=sheetName, col_types = 'text')
    }

    ##Drop identified skips
    if(!all(is.null(skip)) & !all(is.na(skip))) temp <- temp[-1*skip,]

    ##Drop empty columns
    dropCols <- apply(temp, 2, function(xx){all(is.na(xx))}) ##select_if has issues with column names
    temp <- temp[,!dropCols]

    ##rename headers
    names(temp)[names(temp) %in% names(wideHeaders)] <- wideHeaders[names(temp[names(temp) %in% names(wideHeaders)])]

    ##forces that we always have an id for a merge
    if(is.null(temp[[primaryNameStr]])){
      temp[[primaryNameStr]] <- as.character(NA)
    }

    wideVars <- intersect(c(primaryNameStr, wideHeaders), names(temp))
    if(is.data.frame(bigWide.df)){
      bigWide.df <- full_join(
        temp %>% select(one_of(wideVars), matches(idRegEx)), bigWide.df)
    }else{
      bigWide.df <-  temp %>% select(one_of(wideVars), matches(idRegEx))
    }

    longVars <- setdiff(names(temp), wideVars) ##anything that is not wide is long
    if(length(longVars) > 0){
      futureLong.df <- bind_rows(
        temp %>% select(one_of(longVars), matches(idRegEx)), futureLong.df)
    }
  }
  if(verbose) {
    print('Done with reading in all sheets')
    print('bigWide.df')
    print(head(bigWide.df))
    print('futureLong.df')
    print(head(futureLong.df))
  }
  ###Pull out dataframes ###
  ans <- list()

  ans$study <- bigWide.df %>%
    select(one_of(intersect(names(bigWide.df), filter(key.df, dataframe=='study')$header))) %>%
    unique()

  if(verbose) print('Done with constructing study')
  if(verbose) print(head(ans$study))
  ans$field <- bigWide.df %>%
    select(one_of(c(intersect(names(bigWide.df), filter(key.df, dataframe=='field')$header)))) %>%
    unique()

  if(verbose) print('Done with constructing field')
  if(verbose) print(head(ans$field))
  
  if(length(intersect(names(bigWide.df)[!grepl(idRegEx,
                                               names(bigWide.df))], 
                      filter(key.df, dataframe=='treatment')$header)) > 0){
    print('constructing treatment from bigWide.df')
    ans$treatment <-  bigWide.df %>%
      select(one_of(intersect(names(bigWide.df), filter(key.df, dataframe=='treatment')$header)),
             matches(idRegEx)) %>%
      unique()
  }else if(length(intersect(names(futureLong.df), filter(key.df, dataframe=='treatment')$header)) > 0){
    print('constructing treatment from futureLong.df')
    ans$treatment <- futureLong.df %>%
      select(one_of(intersect(names(futureLong.df), filter(key.df, dataframe=='treatment')$header))) %>%
      unique() %>%
      group_by_(.dots=names(.)[grepl(idRegEx, names(.))]) %>%
      gather(header, entry, -matches(idRegEx), na.rm=TRUE) %>%
      left_join(key.df, by=c('header')) %>%
      select(matches(idRegEx), class, type, var, header, entry) %>%
      mutate(entry = if_else(type == 'method', paste(header, entry, sep=':'), entry)) %>%
      group_by(var, add=TRUE) %>%
      summarize(method = paste0(entry[type=='method'], collapse='|'),
                unit = ifelse(any(type == 'unit'),
                              as.character(unique(entry[type=='unit'])), as.character(NA))) %>%
      ungroup()
  } else {
    ans$treatment <- data.frame()
  }

  if(verbose) print('Done with constructing treatment')
  if(verbose) print(head(ans$treatment))
  
  ans$sample <- futureLong.df %>%
    select(one_of(intersect(names(futureLong.df), filter(key.df, dataframe=='sample')$header)),
           matches(idRegEx)) %>%
    filter(!is.na(site_name)) %>%
    group_by_(.dots=names(futureLong.df)[grepl(idRegEx, names(futureLong.df))]) %>%
    gather(header, entry, -matches(idRegEx), na.rm=TRUE) %>%
    left_join(key.df, by='header') %>%
    select(matches(idRegEx), class, type, var, header, entry) %>%
    mutate(entry = if_else(type == 'method', paste(header, entry, sep=':'), entry)) %>%
    group_by(var, add=TRUE) %>%
    filter(any(type == 'value')) %>% unique() %>%
    summarize(method = paste0(entry[type=='method'], collapse='|'),
              value = unique(entry[type == 'value']),
              sigma = ifelse(any(type == 'sigma'),
                             unique(entry[type=='sigma']), as.character(NA)),
              unit = ifelse(any(type == 'unit'),
                            as.character(unique(entry[type=='unit'])), as.character(NA))) %>%
    ungroup()

  if(verbose) print('Done with constructing sample')
  if(verbose) print(head(ans$sample))
  
  ####Cast to numerics and factors####
  if(templetCasting){
    ans$field <- ans$field %>%
      mutate_at(vars(one_of(intersect(unique(key.df$header[key.df$class == 'factor']), names(ans$field)))),
                funs(factor)) %>%
      mutate_at(vars(one_of(intersect(unique(key.df$header[key.df$class == 'numeric']), names(ans$field)))),
                funs(as.numeric)) %>% filter(is.finite(site_name))
  }

    ans$sample <- ans$sample %>%
      mutate_at(vars(matches(idRegEx)),
                funs(factor)) %>%
      mutate_at(vars(method, var),
                funs(factor)) %>%
      mutate_at(vars(value, sigma), funs(as.numeric))

  ans$key <- key.df
  return(ans)
}
