processData_Treat2015 <- function(dir='repoData/Treat_2015', verbose=FALSE){

  library(readxl)
  dir <- '../soils-long-tail-recovery/repoData/Treat_2015'

  ##Read in meta info
  meta.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx', dir),
                                sheet='metadata', col_names=FALSE)
  meta.df <- as.data.frame(t(meta.df), stringsAsFactors=FALSE)
  meta.df <- meta.df[,!is.na(meta.df[4,])]
  names(meta.df) <- as.character(meta.df[1,])
  meta.df <- meta.df[-1,]
  row.names(meta.df) <- NULL

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

  ##Read in layer level info
  layer.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx', dir),
                                   sheet='layer')
  layerCols <- unlist(plyr::llply(layer.df[-1:-2,], function(xx){return(!all(is.na(xx)))}))
  layer.header <- layer.df[1:2, layerCols]
  layer.df <- layer.df[-1:-2, layerCols]

  ##Convert numerics
  layerStr.num <- c('layer_top', 'layer_bot', 'bd_samp', 'soc', 'c_tot', 'n_tot', 'c_to_n', 'loi', 'rc_lab_number', '14c_age', '14c_age_sigma', '210Pb_age', '210Pb_error')
  #oldLayer.df <- layer.df
  layer.df[, layerStr.num] <- llply(layer.df[, layerStr.num], as.numeric)

  profileStr.num <- c('observation_date', 'soc', 'soc_lcount', 'soc_depth')
  profile.df[, profileStr.num] <- llply(profile.df[, profileStr.num], as.numeric)

  siteStr.num <- c('lat', 'long', 'elevation', 'slope')
  site.df[, siteStr.num] <- llply(site.df[, siteStr.num], as.numeric)

  return(list(meta=meta.df, site=site.df, profile=profile.df, layer=layer.df))
}

