#' Convert database into long format
#' 
#' @param data.ls is a list of tables in database
#' @param annotations is data annotations from google sheet in dataframe
#' 
#' @import tidyverse
#'
#' @return dataframe with shoestringed data
#' @export
#' 

transformLong <- function(data.ls, annotations) {
  # data.ls <- list(table1 = tibble(col1 = 1:3, col2 = c("A","B","C")), 
  #                 table2 = tibble(col1 = 11:13, col2b = letters[1:3])
  #                   )
  # 
  # annotations <- tribble(~table_id, ~column_id, ~of_type, ~with_entry,
  #                        "table1", "col1", "unit", "count",
  #                        "table1", "col1", "value", "--",
  #                        "table1", "col2", "value", "--",
  #                        "table2", "col1", "value", "--",
  #                        "table2", "col2b", "value", "--",
  #                        "table1", "col2", "description", "clevernamehere",)
  
  ans <- plyr::ldply(.data = data.ls, .fun = function(x) {
    if("row_number" %in% colnames(x)) {
      warning("Replacing row_number with row order and using as a unique identifier.")
    }
    
    #transform long
    temp <- x %>%
      mutate(across(.cols = everything(), .fns = as.character)) %>%
      
      ungroup() %>%
      mutate(row_number = 1:n()) %>%
      

      ungroup() %>%
      pivot_longer(cols = -c(row_number), names_to = 'column_id',
                   values_to = 'with_entry', values_drop_na = TRUE)
      
      return(temp)
    }, .id = "table_id") %>%
    full_join(annotations, 
            by = join_by(table_id, column_id),
            suffix = c('.data', ''),
            relationship = "many-to-many") %>%
    mutate(
      with_entry = if_else((with_entry == "--") | is.na(with_entry), with_entry.data, with_entry)) %>%
    select(-with_entry.data)
  
  
  return(ans)
}