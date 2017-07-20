processData_Treat2015 <- function(dir='repoData/Treat_2015', verbose=FALSE){

  library(readxl)
  library(plyr)
  #dir <- '../soils-long-tail-recovery/repoData/Treat_2015'

  ########################
  ##Read in meta info
  ########################
  meta.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx',
                                             dir),
                                sheet='metadata', col_names=FALSE)
  meta.df <- as.data.frame(t(meta.df), stringsAsFactors=FALSE)
  meta.df <- meta.df[,!is.na(meta.df[4,])]
  names(meta.df) <- as.character(meta.df[1,])
  meta.df <- meta.df[-1,]
  row.names(meta.df) <- NULL
  meta.df[1, 'modification_date'] <- paste(meta.df[1, 'modification_date'],
                                           '[', meta.df[2, 'modification_date'], ']')
  meta.df <- meta.df[c(1,3),]
  meta.df$studyID <- c('Study identifier', 'Treat_peat_2015')
  meta.df$doi <- c('Digital object identifier', '10.1002/2015JG003061')
  meta.df$permissions <- c('Reuse permissions', 'unknown')
  study.df <- meta.df[2,]

  #########################
  ##Read in site level info
  #########################
  site.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx',
                                             dir),
                                sheet='site')
  siteCols <- unlist(plyr::llply(site.df[-1:-2,], function(xx){return(!all(is.na(xx)))}))
  site.header <- site.df[1:2, siteCols]
  site.df <- site.df[-1:-2, siteCols]

  ############################
  ##Read in profile level info
  ############################
  profile.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx',
                                                dir),
                                   sheet='profile')
  profileCols <- unlist(plyr::llply(profile.df[-1:-2,], function(xx){return(!all(is.na(xx)))}))
  profile.header <- profile.df[1:2,profileCols]
  profile.df <- profile.df[-1:-2,profileCols]

  ##Correct site names in profile.df c('profileName'='SiteName')
  ##setdiff(unique(profile.df$site_name), unique(site.df$site_name))
  siteProfileKey <- c('Lama Lake'= 'Lama_Lake_peat',
               'Lek Vorkuta profile LVPS4+5B'='LVPS_4+5B_Lek_Vorkuta',
               "Sasapimakwanistik 1" = "Sasapimakwananistik 1")
  profile.df$site_name <- plyr::revalue(profile.df$site_name, siteProfileKey)

  ##Not in site definitions nor in the layers
  profileOnlySites <- c('07-SA-LY-B', '1973-31', "1973-32", "1973-33",
                        "1973-41","1973-42", "1973-43", "1973-44")
  sitesWithoutProfiles <- c('1973-4')

  ##########################
  ##Read in layer level info
  ##########################
  layer.df <- readxl::read_excel(path=sprintf('%s/ISCNtemplate_Treat_peatProps_v2.xlsx',
                                              dir), sheet='layer')
  layerCols <- unlist(plyr::llply(layer.df[-1:-2,], function(xx){return(!all(is.na(xx)))}))
  layer.header <- layer.df[1:2, layerCols]
  layer.df <- layer.df[-1:-2, layerCols]
  #setdiff( layer.df$site_name, site.df$site_name)
  siteLayerKey <- c('Lama Lake'= 'Lama_Lake_peat',
                    'Lek Vorkuta profile LVPS4+5B'='LVPS_4+5B_Lek_Vorkuta',
                    "Sasapimakwanistik 1" = "Sasapimakwananistik 1",
                    "Lac_le_Caron" = "Lac_Le_Caron")
  layer.df$site_name <- plyr::revalue(layer.df$site_name, siteLayerKey)

  ###################
  ##Convert numerics
  ###################
  layerStr.num <- c('layer_top', 'layer_bot', 'bd_samp', 'soc', 'c_tot', 'n_tot', 'c_to_n',
                    'loi', '14c_age', '14c_age_sigma', '210Pb_age', '210Pb_error')
  #oldLayer.df <- layer.df
  layer.df[, layerStr.num] <- plyr::llply(layer.df[, layerStr.num], as.numeric)

  profileStr.num <- c('observation_date', 'soc', 'soc_lcount', 'soc_depth')
  profile.df[, profileStr.num] <- llply(profile.df[, profileStr.num], as.numeric)

  siteStr.num <- c('lat', 'long', 'elevation', 'slope')
  site.df[, siteStr.num] <- llply(site.df[, siteStr.num], as.numeric)

  #####################################
  ##merge profile and layer information
  #####################################
  profileVals.df <- profile.df[, c('profile_name', 'soc', 'soc_lcount', 'soc_depth')]
  profile.df <- profile.df[, setdiff(names(profile.df),
                                     c('soc', 'soc_lcount', 'soc_depth'))]


  #############################
  ##identify process columns
  #############################
  ##field columns
  fieldColStr <- c('site_name', 'profile_name', 'layer_name', 'observation_date', 'layer_top', 'layer_bot',
                   'surface_veg', 'sampler_names', 'hzn_desgn', 'layer_note',  'rc_lab', 'rc_lab_number')

  ##value columns
  valColStr <- c('bd_samp', 'soc', 'c_tot', 'n_tot', 'c_to_n', 'loi', '14c_age', '210Pb_age')
  ##error columns
  errColStr <- c('14c_age_sigma', '210Pb_error')
  ##bulk density columns
  bdColStr <- c('bd_method', 'bd_samp')
  ##radiocarbon columns
  rcColStr <- c('14c_age', '210Pb_age')

  ############################
  ##make ID for field sampling
  ############################
  #names(sample.df)[!(names(sample.df) %in% c(valColStr, errColStr, bdColStr, rcColStr, fieldColStr))]
  sample.df <- merge(profile.df, layer.df, all=TRUE, by=c('site_name', 'profile_name'))
  field.df <- site.df
  field.df <- merge(field.df, sample.df[,fieldColStr])
  field.df$fieldID <- field.df$layer_name
  field.df$fieldID[is.na(field.df$layer_name)] <- field.df$profile_name[is.na(field.df$layer_name)]
  sample.df <- merge(sample.df, field.df[, c('fieldID', 'layer_name', 'profile_name')],
                     by=c('layer_name', 'profile_name'), all.x=TRUE)

  ########################
  ##Pull values and seperate errors
  ########################
  sample.df <- sample.df[, setdiff(names(sample.df), c(fieldColStr, 'bd_method'))]
  err.df <- sample.df[, c('fieldID', errColStr)]
  names(err.df) <-c('fieldID', '14c_age', '210Pb_age')
  err.df <- reshape2::melt(err.df, id.vars='fieldID', na.rm=TRUE,
                           value.name='uncertainty', factorsAsStrings=TRUE)
  err.df$variable <- as.character(err.df$variable)
  sample.df <- reshape2::melt(sample.df[,c('fieldID', valColStr)], id.vars='fieldID',
                              na.rm=TRUE, factorsAsStrings=TRUE)
  sample.df$variable <- as.character(sample.df$variable)

  sample.df <- merge(sample.df, err.df, all=TRUE, by=c('fieldID', 'variable'))

  ####################
  ##create measurement
  ####################
  measurement.df <- data.frame(type=c('bd_samp', 'soc', 'c_tot', 'n_tot', 'c_to_n',
                                      'loi', '14c_age', '210Pb_age'),
                               method=c('Volumetric', rep('', 7)),
                               units=c('g cm-3', 'g cm-2', '%', '%', 'mass %',
                                       '%', 'permill', ''),
                               uncertainty_type=c(rep('', 6), c('sigma', 'error')))

  ans <- list(study=study.df, sample=sample.df, site=field.df, measurement=measurement.df)

  return(ans)
}

