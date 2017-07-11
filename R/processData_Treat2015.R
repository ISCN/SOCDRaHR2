processData_Treat2015 <- function(dir='repoData/Treat_2015', verbose=FALSE){

  library(readxl)
  #'Imports excel files into R
  dir <- '../soils-long-tail-recovery/repoData/Treat_2015'
  #'Proper directory linking
  
  ##Read in meta info
  meta.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx', dir),
                                sheet='metadata', col_names=FALSE)
  meta.df <- as.data.frame(t(meta.df), stringsAsFactors=FALSE)
  #'Meta dataframe read, all factors cannot be changed to strings
  meta.df <- meta.df[,!is.na(meta.df[4,])]
  #'As long as data is available, 4-on read into data frame
  names(meta.df) <- as.character(meta.df[1,])
  #'Dataframe from [1-on changed into characters
  meta.df <- meta.df[-1,]
  #'All data replaced into meta

  ##Read in site level info
  site.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx', dir),
                                sheet='site')
  siteCols <- unlist(plyr::llply(site.df[-1:-2,], function(xx){return(!all(is.na(xx)))}))
  site.header <- site.df[1:2, siteCols]
  site.df <- site.df[-1:-2, siteCols]

  ##Read in profile level info
  profile.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx', dir),
                                   sheet='profile')
  profileCols <- unlist(plyr::llply(profile.df[-1:-2,], function(xx){return(!all(is.na(xx)))}))
  profile.header <- profile.df[1:2,profileCols]
  profile.df <- profile.df[-1:-2,profileCols]
  #' Profile excel read
  #' List plyr function with unlist to separate all the data into columns
  #' 1:2 header of new list
  #' Data placed back into profile dataframe
  
  ##Read in layer level info
  layer.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx', dir),
                                   sheet='layer')
  layerCols <- unlist(plyr::llply(layer.df[-1:-2,], function(xx){return(!all(is.na(xx)))}))
  layer.header <- layer.df[1:2, layerCols]
  layer.df <- layer.df[-1:-2, layerCols]
  #' Same as for the profile information

  ##Convert numerics
  layerStr.num <- c('layer_top', 'layer_bot', 'bd_samp', 'soc', 'c_tot', 'n_tot', 'c_to_n', 'loi', 'rc_lab_number', '14c_age', '14c_age_sigma', '210Pb_age', '210Pb_error')
  #oldLayer.df <- layer.df
  layer.df[, layerStr.num] <- llply(layer.df[, layerStr.num], as.numeric)
  #' layer.df<-list plyr of layerSTR.num after being converted to numeric

  profileStr.num <- c('observation_date', 'soc', 'soc_lcount', 'soc_depth')
  #' Numbered columns for the profile, labeled ('')
  profile.df[, profileStr.num] <- llply(profile.df[, profileStr.num], as.numeric)
  #' Converted to numeric
  
  siteStr.num <- c('lat', 'long', 'elevation', 'slope')
  #' Numbered columns for the site, labeled ('')
  site.df[, siteStr.num] <- llply(site.df[, siteStr.num], as.numeric)
  #' Converted into numeric
  
  return(list(meta=meta.df, site=site.df, profile=profile.df, layer=layer.df))
}
#' @return list of data from meta, site, profile, and layer for reintroduction to a pdf
