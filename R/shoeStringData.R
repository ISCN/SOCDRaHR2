#' Title
#' 
#' Body 
#' 

#'
shoeStringData <- function(dataTables.ls, 
                          threeKeys = list(data_meta, data_structure, data_thesaurus),
                          verbose=FALSE){
  
  # ##Test/dev loads
  # threeKeys <- list(data_meta = read_csv('data-raw/data_meta.csv', col_types = strrep('c', 5)) %>% filter(data_product == 'ISCN3') %>% select(-data_product), 
  #                  data_structure = read_csv('data-raw/data_structure.csv', col_types = strrep('c', 4)) %>% filter(data_product == 'ISCN3') %>% select(-data_product), 
  #                  data_thesaurus = read_csv('data-raw/data_thesaurus.csv', col_types = strrep('c', 6)) %>% filter(data_product == 'ISCN3') %>% select(-data_product))
  # 
  # dataTables.ls <- list( dataset = read_delim('~/Documents/Datasets/ISCN/ISCN3_dataset.csv', delim = ';', col_types = strrep('c', 19)),
  #                        citation = read_delim('~/Documents/Datasets/ISCN/ISCN3_citation.csv', delim = ';', col_types = strrep('c', 12)),
  #                        profile = read_delim('~/Documents/Datasets/ISCN/ISCN3_profile.csv', delim = ';', col_types = strrep('c', 44)),
  #                        layer = read_delim('~/Documents/Datasets/ISCN/ISCN3_layer.csv', delim = ';', col_types = strrep('c', 95)))
  # 
  
  #TODO add checks to make sure the keys and the tables match
  
  if(verbose){
    message(paste('Identified data keys'))
    print(threeKeys$data_structure %>% filter(data_type == 'id') %>% arrange(data_table, data_column))
  }
  
  ans <- lapply(setNames(names(dataTables.ls), names(dataTables.ls)), function(xx){
    
    #take all the meta keys and put them together
    fullTableMeta <- threeKeys$data_meta %>% 
      #pull meta for a single table
      filter(data_table == xx) %>%
      select(-data_table) %>%
      #rename the 'entry' column so that it's associated with the meta
      rename('entry.meta' = 'entry') %>%
      #join with what kind of data is in each colum.
      full_join(threeKeys$data_structure %>% 
                  #pull meta for a single table
                  filter(data_table == xx) %>% 
                  select(-data_table) , 
                by='data_column', suffix = c('.meta', '.data_column')) %>%
      #join with how that data is grouped
      full_join(threeKeys$data_thesaurus %>% 
                  filter(data_table == xx) %>% 
                  #TODO Here is where we need to deal with long data differently, right now ignoring things
                  select(-data_table, -variable_location),
                by = c('data_column' = 'provided_variable' ) ) %>%
      mutate(across(matches('[^(meta)]'), as.factor))
    
    #pull the data table
    longData <- dataTables.ls[[xx]] %>% #sample_n(10) %>%
      #make everything long
      pivot_longer(cols = (threeKeys$data_structure %>% filter(data_table == xx, data_type != 'id'))$data_column, 
                   names_to = 'data_column', values_to = 'value', values_drop_na = TRUE) %>%
      mutate(across(matches('[^(value)]'), as.factor)) %>%
      #join it with the meta data
      left_join(fullTableMeta, by = 'data_column') %>%
      rename('entry.data_column' = 'value') %>%
      rename('data_column.entry' = 'entry.data_column', 'data_column.data_type' = 'data_type.data_column', 
             'meta.entry' = 'entry.meta', 'meta.data_type'='data_type.meta') %>%
      #kick that meta data also into a long format
      pivot_longer(cols = c("data_column.entry", "meta.data_type", "meta.entry", "data_column.data_type"), 
                   names_to = c('source', '.value'),
                   names_pattern = '(.*)\\.(.*)', values_drop_na = TRUE) %>%
      mutate(across(matches('[^(entry)]'), as.factor)) # %>%
      # group_by(across(c((threeKeys$data_structure %>% filter(data_table == xx, data_type == 'id'))$data_column, 'data_column', 'data_type', 'variable'))) %>%
      # unique() %>%
      # summarize(value = paste0(entry, collapse = '; '),
      #           count = length(entry)) %>%
      # pivot_wider(names_from = 'data_type', values_from = 'value')
    
    return(longData)
  })
  
  return(ans)
}
