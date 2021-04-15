ISCN3_1 <- function(){
  #ans <- data.frame()
  
  #load in library
  library(data.table)
  #library(SOCDRaH2)
  library(tidyverse)
  library(lubridate)
  library(tibble)
  library(ggmap)
  library(maps)
  library(mapdata)
  library(knitr)
  library(tidyr)

  #read in the data
  
  data_dir <- '~/Documents/Todd-Brown Lab/ISCN3/' #change to location of ISCN3
  #ISCN3 <- SOCDRaH2::ISCN3(orginalFormat=TRUE)
  #citation_raw <- data.frame(ISCN3$citation)
  #dataset_raw <- data.frame(ISCN3$dataset)
  #profile_raw <- data.frame(ISCN3$profile)
  #layer_raw <- data.frame(ISCN3$layer)
  #rm(ISCN3)
  
  citation_raw <- read_delim(file.path(data_dir, 'ISCN3_citation.csv'), delim = ';', col_types = strrep('c', times = 12)) %>% 
    #round all modification dates to their nearest day (ie whole number)
    mutate(`modification_date (YYYY-MM-DD)` = as.character(round(as.numeric(`modification_date (YYYY-MM-DD)`))))
  
  dataset_raw <- read_delim(file.path(data_dir, 'ISCN3_dataset.csv'), delim = ';', col_types = strrep('c', times = 19)) %>% 
    #round all modification dates to their nearest day (ie whole number)
    mutate(`modification_date (YYYY-MM-DD)` = as.character(round(as.numeric(`modification_date (YYYY-MM-DD)`))))
  
  profile_raw <-  vroom::vroom(file.path(data_dir, 'ISCN3_profile.csv'), col_types = strrep('c', times = 44))
  
  layer_raw <- vroom::vroom(file.path(data_dir, 'ISCN3_layer.csv'), col_types = strrep('c', times = 95))
  
  #initialize list
  
  ISCN3_1_List <-list(citation_raw, dataset_raw, profile_raw, layer_raw) 
  

  
  #set up standardCast()
  
  
  #define dataset name from dataframe
  #extract study info
  #filter appropriate columns for that dataset
  #only include columns that aren't entirely NA
  #compiling citation and dataset columns
  #running standardCast()
  #taking profile and layer info
  #if rows contain "ISCN" in dataset_name_soc, filling set columns (`soc_depth (cm)`, `soc (g cm-2)`, soc_carbon_flag, soc_spatial_flag, soc_method) with NA, otherwise leaving value as is
  
  
  #put if statements to catch if it's a particular dataset/frame which will perform special functions to do what we need to
  
  
  
  
  
  return(ans)
}
