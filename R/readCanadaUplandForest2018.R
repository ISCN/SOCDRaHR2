#' Raw read for Canadian upland soils (2018)
#' 
#' Download and read in the raw data tables from Shaw, C., Hilger, A., Filiatrault, M. and
#'  Kurz, W. (2018), A Canadian upland forest soil profile and carbon stocks 
#'  database. Ecology. doi:10.1002/ecy.2159
#'  This is not post-processed or QA/QCed by this package.
#'
#' @param dataDir filename for download directory
#' @param download boolean that will download the files from repository
#'
#' @return a list with meta data and raw table reads. This will be 7.4 Mb in size.
#' 
#' @export
#' 
#'
readCanandaUplandForest2018 <- function(dataDir, download=TRUE){
  
  #### Download ####
  ShawDownload_url <- c('http://binarystore.wiley.com/store/10.1002/ecy.2159/asset/supinfo/ecy2159-sup-0002-MetadataS1.pdf?v=1&s=1b267bc2e7bbafc52b12a9e522033fa590445713', 
                        'http://binarystore.wiley.com/store/10.1002/ecy.2159/asset/supinfo/ecy2159-sup-0001-DataS1.zip?v=1&s=f95f40084248b880723282404305327c0aa95aeb')
  ShawDataFiles <- file.path(dataDir, 
                             c("LOOKUP.csv", "LOOKUP_DB.csv", "PROFILES.csv",
                               "REFERENCES.csv", "SITES.csv"))
  if(download){
    ShawDownloadFilename <- file.path(dataDir, 
                                      c('ecy2159-sup-0002-MetadataS1.pdf',
                                        'ecy2159-sup-0001-DataS1.zip'))
    if(!all(file.exists(ShawDownloadFilename))){
      download.file(ShawDownload_url[1], ShawDownloadFilename[1], quiet=FALSE)
      download.file(ShawDownload_url[2], ShawDownloadFilename[2], quiet=FALSE)
    }
    #ShawDataFiles <- file.path(file.path(dataDir, 'Shaw2018'),
    #                           unzip(ShawDownloadFilename[2], list=TRUE)$Name)
    
    unzip(ShawDownloadFilename[2], exdir=dataDir)
  }
  
  ans <- list(downloadFiles = c(file.path(dataDir, 'ecy2159-sup-0002-MetadataS1.pdf'),
                                ShawDataFiles),
              licenseShort = "nonCC with BY-NC",
              licenseFull = "Copyright restrictions
© Her Majesty the Queen in Right of Canada, 2017
              
              Information contained in this publication or product may be reproduced, in part or in whole, and by any means, for personal or public non-commercial purposes, without charge or further permission, unless otherwise specified.
              You are asked to:
              
              -  exercise due diligence in ensuring the accuracy of the materials reproduced;
              -  indicate the complete title of the materials reproduced, and the name of the author organization; and
              -  indicate that the reproduction is a copy of an official work that is published by Natural Resources Canada (NRCan) and that the reproduction has not been produced in affiliation with, or with the endorsement of, NRCan.
              -  Commercial reproduction and distribution is prohibited except with written permission from NRCan. For more information, contact NRCan at copyright.droitdauteur@nrcan-rncan.gc.ca.",
              
              citation = "Shaw, C., Hilger, A., Filiatrault, M. and Kurz, W. (2018), A Canadian upland forest soil profile and carbon stocks database. Ecology. doi:10.1002/ecy.2159")
  
  
  for(readFile in ShawDataFiles){
    ans[[gsub('.csv', '', basename(readFile))]] <- read.csv(readFile, colClasses="character")
  }
  
  ##### Enter data key #####
ans$ISCN5_key <- read.csv(text='table,header,MetaDataDescriptors,var,longVar,flagID,dataframe,class,softType,hardValue,hardSigma,hardUnit,hardMethod
PROFILES,LOCATION_ID,Unique identifier for each site. ,site_name,,field_id,field,,,,,,
PROFILES,HZN_SEQ_NO,Unique identifier for each horizon. Used in conjunction with LOCATION_ID to produce unique identifiers for the PROFILES table. ,profile_name,,field_id,field,,,,,,
PROFILES,HORIZON,Soil horizon designation according to the Canadian System of Soil Classification. ,hzn_desgn,,,field,,,,,,
PROFILES,HZN_ORG_MIN,Indicates whether the horizon is organic or mineral according to definitions of the Canadian System of Soil Classification. ,hzn_desgn_note,,,field,,,,,,
PROFILES,UPPER_HZN_LIMIT,Depth of upper horizon limit referenced to the top of the first mineral soil horizon (depth of 0 cm).,layer_top,,,field,,,,,,
PROFILES,HZN_THICKNESS,"Thickness of the horizon; the lowest horizon of the mineral soil is adjusted, so that the total depth for MIN_CARB_THA calculations is based on 100 cm of mineral soil depth.",layer_thickness,,,field,,,,,,
PROFILES,BULK_DENSITY,"If the measured bulk density of soil was reported, then it was used, but bulk density was not commonly measured in the archival data sources, see meta for details.",bd_other,,,sample,,value,,,Mg m-3,
PROFILES,DB_MEAS_EST,Indicates whether bulk density was measured or estimated ,bd_other,,,sample,,method,,,,
PROFILES,DB_EST_TYPE,Indicates method of estimation if DB_MEAS_EST is “Estimated” ,bd_other,,,sample,,method,,,,
PROFILES,DB_CF_CORR,Indicates whether a measured BULK_DENSITY was corrected for coarse fragments by the source ,bd_other,,,sample,,method,,,,
PROFILES,CF_CLASS,Coarse fragments class based on percent volume of coarse fragments from visual estimates reported by the source in the soil profile description ,wpg2,,,sample,,method,,,,
PROFILES,CF_VOL_PCT,Percent (volume basis) of coarse fragments used in HZN_CARB_THA calculations. ,wpg2,,,sample,,value,,,percent (volume),
PROFILES,CF_CORR_FACTOR,CF_CORR_FACTOR is calculated as (1 - CF_VOL_PCT/100) and is a multiplier used to calculate HZN_CARB_THA.,soc,,,sample,,method,,,,
PROFILES,ORG_CARB_PCT,"Percent organic carbon reported by the source, or estimated. See meta documentation for details)",oc,,,sample,,value,,,,
PROFILES,CARB_MEAS_EST,Indicates whether ORG_CARBON_PCT was measured and reported in the source or estimated using the procedure described for ORG_CARBON_PCT ,oc,,,sample,,method,,,,
PROFILES,CARB_EST_TYPE,Indicates method of estimation if CARB_MEAS_EST is “Estimated” ,oc,,,sample,,method,,,,
PROFILES,HZN_CARB_THA,Organic carbon stock for a horizon calculated as HZN_CARB_THA = ORG_CARB_PCT/100 * BULK_DENSITY * HZN_THICKNESS * 100 * HZN_CF_FACTOR,soc,,,sample,,value,,,,
PROFILES,SAND_PCT,Percent sand as reported by the source rounded to the nearest integer ,sand_tot_psa,,,sample,,value,,,percent,
PROFILES,SILT_PCT,"Percent silt as reported by the source, rounded to the nearest integer ",silt_tot_psa,,,sample,,value,,,percent,
PROFILES,CLAY_PCT,"Percent clay as reported by the source, rounded to the nearest integer ",clay_tot_psa,,,sample,,value,,,percent,
PROFILES,TEXT_CLASS,"Texture class as computed using a function (based on SAND_PCT, SILT_PCT, and CLAY_PCT) defined in the Canadian System of Soil Classification ",textureClass,,,field,,,,,,
PROFILES,TEXT_EST_TYPE,Indicates method of assigning TEXT_CLASS as “Manual” or “Calculated” ,textureClass_method,,,field,,,,,,
PROFILES,EXCH_Ca,Exchangeable calcium as reported by the source. ,ca_ext,,,sample,,value,,,cmol kg-1,
PROFILES,EXCH_Mg,Exchangeable magnesium as reported by the source. ,mg ext,,,sample,,value,,,cmol kg-1,
PROFILES,EXCH_Na,Exchangeable sodium as reported by the source. ,na_ext,,,sample,,value,,,cmol kg-1,
PROFILES,EXCH_K,Exchangeable potassium as reported by the source. ,k_ext,,,sample,,value,,,cmol kg-1,
PROFILES,CEC_CALCULATED,"Cation exchange capacity calculated as the sum of EXCH_Ca, EXCH_Mg, EXCH_Na, and EXCH_K ",,,,,,,,,cmol kg-1,
PROFILES,TEC_REPORTED,Total exchange capacity or cation exchange capacity as reported by the source. ,cec_sum,,,sample,,value,,,cmol kg-1,
PROFILES,ELEC_COND,Electrical conductivity as reported by the source ,elec_cond,,,sample,,value,,,mmhos cm-1,
PROFILES,pH,"The pH value reported by the source, whether the method was pH in water or pH in CaCl2",ph_other,,,sample,,value,,,,
PROFILES,pH_H2O_CACL2,Indicates whether pH was measured in water or a CaCl2 solution.,ph_other,,,sample,,method,,,,
PROFILES,CARBONATE_CARB_PCT,"Percent carbonate carbon, if data were provided in the original source. ",caco3,,,sample,,value,,,percent,
PROFILES,TOT_NITRO_PCT,Percent total nitrogen as reported by the source. ,n_tot,,,sample,,value,,,percent,
PROFILES,Fe_PYROPHOSPHATE,Pyrophosphate-extractable iron as reported by the source. ,fe_other,,,sample,,value,,,percent,Pyrophosphate-extractable
PROFILES,Al_PYROPHOSPHATE,Pyrophosphate-extractable aluminum as reported by the source. ,al_other,,,sample,,value,,,percent,Pyrophosphate-extractable
PROFILES,Fe_OXALATE,Oxalate-extractable iron as reported by the source. ,fe_ox,,,sample,,value,,,percent,
PROFILES,Al_OXALATE,Oxalate-extractable aluminum as reported by the source. ,al_ox,,,sample,,value,,,percent,
SITES,LOCATION_ID,Unique identifier for each site. ,site_name,,field_id,field,,,,,,
SITES,RELEASE_SOURCE,Short name for data source (publication or database) ,source_name,,,field,,,,,,
SITES,RELEASE_SOURCE_SITEID,Site identifier used in source publication or database ,source_site_name,,,field,,,,,,
SITES,REF_ID,Identification number for linking SITES to REFERENCES table ,source_id,,study_id,field,,,,,,
SITES,YEAR_SAMPLED,"The best estimate of the year the soil profile was sampled. Where year of sampling was not provided by the original source, we used the year of the publication. ",observation_date,,,field,,,,,,
SITES,CSSC_CODE,Soil great group and subgroup abbreviation according to the Canadian System of Soil Classification ,cssc_code,,,field,,,,,,
SITES,ORDER,Soil order according to the Canadian System of Soil Classification. ,cssc_order,,,field,,,,,,
SITES,GREAT_GROUP,Soil great group according to the Canadian System of Soil Classification ,cssc_gg,,,field,,,,,,
SITES,GGSG,Soil great group and subgroup according to the Canadian System of Soil Classification ,cssc_ggsg,,,field,,,,,,
SITES,ORG_C_THA,Total soil organic carbon stock in organic horizons of the soil profile ,soc_organic,,,sample,,value,,,t ha-1,
SITES,MIN_C_THA,"Total soil organic carbon stock in the mineral horizons of the soil profile to a depth of 100 cm or less, where rock is present above a 100-cm depth",soc_mineral,,,sample,,value,,,t ha-1,
SITES,TOT_C_THA,"Total soil organic carbon in the soil profile to a depth of 100 cm, or less where rock is present above a 100-cm depth.",soc,,,sample,,value,,,t ha-1,
SITES,COARSE_FRAGMENTS,Indicates if any steps were taken to correct for coarse fragments in the calculation of MIN_CARB_THA ,soc,,,sample,,method,,,,
SITES,DEPTH_TO_RX,"Depth from the surface of the mineral soil to rock, if the presence of rock was reported by the source ",total_soil_depth,,,field,,,,,cm,
SITES,FOREST_TYPE,"Defined as hardwood (HW), softwood (SW), or mixedwood (MW) based on descriptions provided by original source, or in some cases by a quantitative assessment of individual tree data if they were available ",vegclass,,,field,,,,,,
SITES,LEAD_SPECIES_CODE,"Leading or dominant tree species code: first 4 letters of the genus, then first 3 letters of the species",dominate_tree,,,field,,,,,,
SITES,PROV_TERR,"Code for province, territory, or region in Canada ",province,,,field,,,,,,
SITES,ECOBOUND,Modified terrestrial ecozones of Canada ,eco_note,,,field,,,,,,
SITES,LATITUDE,Site location latitude ,lat,,,field,,,,,,
SITES,LONGITUDE,Site location longitude,lon,,,field,,,,,,
SITES,LOC_ACCURACY,Code for the relative accuracy of the location information within this database.,lat_accuracy,,,field,,,,,,
SITES,LOC_ACCURACY_TYPE,Code for method to determine location and location accuracy,lat_accuracy_method,,,field,,,,,,
SITES,LOC_ACCURACY_NOTES,Additional notes on location method and accuracy ,lat_accuracy_method2,,,field,,,,,,
SITES,MAT,"Mean annual temperature for the geographic coordinates of each site, as predicted by model",mat,,,field,,,,,deg C,
SITES,PRECIP,"Annual precipitation for the geographic coordinates of each site, as predicted by the model",map,,,field,,,,,mm yr-1,
REFERENCES,REF_ID,Identification number for linking SITES to REFERENCES table ,source_id,,study_id,study,,,,,,
REFERENCES,CITE_SHORT,,source_cite,,,study,,,,,,
REFERENCES,CITE_LONG,,source_citation,,,study,,,,,,
REFERENCES,AUTHORS,,source_authors,,,study,,,,,,
REFERENCES,YEAR,,source_year,,,study,,,,,,
REFERENCES,TITLE,,source_title,,,study,,,,,,
REFERENCES,LINK_TO_PDF,,source_url,,,study,,,,,,', na.strings='')
  
  return(ans)
}