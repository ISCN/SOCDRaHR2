#' Convert dataset into long format
#' 
#' @param data.df is original dataset
#' @param annotations is data annotations from google sheet in dataframe
#'
#' @return dataframe with shoestringed data
#' @export
#' 

transformLong <- function(data.df, annotations) {
  
  #check if dataset is too large
  
  ans <- data.df %>%
    mutate(study_id = as.character(annotations[1, "study_id"]), 
           table_id = as.character(annotations[1, "table_id"]))  %>%
    #Group by the columns that are identifiers
    group_by(across(all_of((annotations %>%
                              filter(is_type == 'identifier'))$column_id))) %>%
    #Pull the current group index or id 
    mutate(observation_id = cur_group_id()) %>%
    #and then remove the grouping so we can move on
    ungroup() %>%
    pivot_longer(cols = -c(study_id, table_id, observation_id), names_to = 'column_id', 
                 values_to = 'with_entry', values_drop_na = TRUE) %>%
    full_join(annotations, 
              by = join_by(study_id, table_id, column_id),
              suffix = c('.data', ''), multiple = 'all') %>%
    mutate(
      with_entry = if_else((with_entry == "--") | is.na(with_entry), with_entry.data, with_entry)) %>%
    select(-with_entry.data)
  
  return(ans)
}