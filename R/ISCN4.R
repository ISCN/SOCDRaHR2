#' Read in two data sets queued in for data processing for ISCN4.
#'
#' @param dataDir string identifing the data directory to download data to, or NULL to download to a temporary directory
#' @param onlyNewData boolean flagging new ISCN4 data only or appended to ISCN3
#' @param verbose boolean flagging verbose error messages
#'
#' @return a list of dataframes containing the study, field, and sample data
#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom readr write_csv read_csv
#' @importFrom readxl read_excel
#' @importFrom lubridate decimal_date mdy
#' @export
#'
#' @examples 
#' \dontrun{
#' temp <- ISCN4()
#' }
ISCN4 <- function(dataDir=NULL, onlyNewData=TRUE, verbose=TRUE){
 
  dataDir <- '~/Documents/Professional/Datasets/ISCN_3'
  ISCN <- ISCN3(dataDir = dataDir, verbose=TRUE)
  
  keys.ls <- makeKeys()
  
  tempKey <- dplyr::left_join(keys.ls$ISCN, keys.ls$ISCN3, by='variable', 
                              suffix=c('_target', '_source'))
  
  #xx <- tempKey %>% filter(table_target == 'study', table_source == 'citation', !is.na(header_source))
  
  temp <- plyr::dlply(tempKey %>% filter(!is.na(header_source)), 
                      c('table_target'), function(xx){
                        plyr::ddply(xx, c('table_source'), function(yy){
                          selectCols <- unique(yy$header_source)
                          ans <- unique(data.table::data.table(ISCN[[yy$table_source[1]]])[, ..selectCols])
                          return(ans)
                        })
  })
  
if(!onlyNewData){
    stop('Merging with ISCN3 not currently implimented.')
  }
  
  ####### construct the ISCN2016 template key ##########
  key.df <- readr::read_csv( 
 'sheet,header,headerLong,var,varLong,flagID,dataframe,class,softType,hardUnit,hardValue,hardSigma,hardMethod
,,,download_url,,,,,,,,,
  metadata,curator_name,Curator Name - ,curator_name,name of person curating dataset,,study,character,,,,,
  metadata,curator_organization,Curator Organization - ,curator_organization,organization responsible for curation,,study,character,,,,,
  metadata,curator_email,Curator Email - ,curator_email,email for curator,,study,character,,,,,
  metadata,modification_date,Template Modification Date - YYYY-MM-DD or MM/DD/YYYY,modification_date,last data of modification for the data,,study,character,,,,,
  metadata,contact_name,Additional Contact Name - ,contact_name,contact person for data set,,study,character,,,,,
  metadata,contact_email,Additional Contact Email - ,contact_email,email for contact,,study,character,,,,,
  metadata,dataset_name,Dataset Name - ,dataset_name,reference name for the dataset,study_id,study,character,,,,,
  metadata,dataset_description,Dataset Description - ,dataset_description,long discription of the data set,,study,character,,,,,
  metadata,reference,Reference - ,reference,list of references associated with dataset,,study,character,,,,,
  metadata,citation,Citation text - ,citation,citation for dataset,,study,character,,,,,
  metadata,citation_usage,Citation usage - ,citation_usage,nows on how to cite the dataset,,study,character,,,,,
  metadata,acknowledgement,Acknowlegement text - ,acknowledgement,text for inclusion in an acknowledgement section,,study,character,,,,,
  metadata,acknowledgement_usage,Acknowlegement usage - ,acknowledgement_usage,notes on how to use the acknowledgement text,,study,character,,,,,
  metadata,c_est_method,Carbon Estimation Method - ,c_est_method,,,study,character,,,,,
  metadata,c_est_note,Carbon Estimation Note - ,c_est_note,,,study,character,,,,,
  metadata,lab_method,Lab Analysis Method - ,lab_method,,,study,character,,,,,
  metadata,metadata_general,General Metadata - ,metadata_general,,,study,character,,,,,
  metadata,missdata_param,Parameters for Missing Data Equations - ,missdata_param,,,study,character,,,,,
  metadata,resample,Resample Metadata - ,resample,,,study,character,,,,,
  metadata,funding_organization,Funding Organization(s) - ,funding_organization,,,study,character,,,,,
  site,site_name,Site Name - ,site_name,name for the site,field_id,field,factor,,,,,
  site,site_note,Site Notes - ,site_note,,,field,character,,,,,
  site,country,Country - ,country,country of location,,field,factor,,,,,
  site,province,Province - ,state,state or other poltical unit at sub-country level,,field,factor,,,,,
  site,state,State - (us_states),state,state or other poltical unit at sub-country level,,field,factor,,,,,
  site,county,County - ,county,,,field,factor,,,,,
  site,lat,Latitude - dec. deg,lat,latitute,,field,numeric,,,,,
  site,long,Longitude - dec. deg,lon,longitude,,field,numeric,,,,,
  site,datum,Latitude/Longitude Datum - (datum),datum,lat-lon datum id,,field,factor,,,,,
  site,location_acc,Location Accuracy - m,location_acc,,,field,numeric,,,,,
  site,elevation,Elevation - m,elevation,,,field,numeric,,,,,
  site,vegclass_nat,US National Vegetation Classification Standard - ,vegclass_nat,,,field,factor,,,,,
  site,vegclass,Vegetation Classification Code - ,vegclass,,,field,factor,,,,,
  site,vegclass_local,Local Vegetation Classification Code - ,vegclass_local,local vegitation decriptor (non standard),,field,character,,,,,
  site,vegclass_local_type,Local Vegetation Classification Type - ,vegclass_local_type,,,field,character,,,,,
  site,veg_note,Additional Vegetation Data Notes - ,veg_note,,,field,character,,,,,
  site,landscape,Landscape - (landscape),landscape,,,field,factor,,,,,
  site,landform,Landform - (landform),landform,,,field,factor,,,,,
  site,2d_position,2D Position - (2d_position),2d_position,,,field,factor,,,,,
  site,landform_note,Landform Notes - ,landform_note,,,field,character,,,,,
  site,parent,Parent Material - ,parent,,,field,character,,,,,
  site,aspect_deg,Site Aspect - degree,aspect_deg,,,field,numeric,,,,,
  site,aspect_cl,Site Aspect Class - (direction),aspect_cl,,,field,factor,,,,,
  site,slope,Slope - percent,slope,,,field,numeric,,,,,
  site,stand_age,Maximum Stand Age - years,stand_age,,,field,numeric,,,,,
  site,stand_maturity,Stand Maturity - (stand_maturity),stand_maturity,,,field,factor,,,,,
  site,successional_status,Successional Status - ,successional_status,,,field,character,,,,,
  site,drainagecl,Drainage Class - (drainage),drainagecl,,,field,factor,,,,,
  site,depth_water,Depth to Water Table - cm,depth_water,,,field,numeric,,,,,
  site,thaw_depth_site,Thaw Depth of the Site - cm,thaw_depth_site,,,field,numeric,,,,,
  site,bedrock_depth,Depth to Bedrock - cm,bedrock_depth,,,field,numeric,,,,,
  site,climate_station,Meterological Station - ,climate_station,,,field,character,,,,,
  site,ffdays,Frost Free Days - days,ffdays,,,field,numeric,,,,,
  site,flood_freq,Flooding Frequency - (flood_freq),flood_freq,,,field,factor,,,,,
  site,geo_form,Geologic Formation - ,geo_form,,,field,character,,,,,
  site,map,Mean Annual Precipitation - mm,map,,,field,numeric,,,,,
  site,mat,Mean Annual Temperature - degree C,mat,,,field,numeric,,,,,
  site,mast,Mean Annual Soil Temperature - degree C,mast,,,field,numeric,,,,,
  site,pond_freq,Ponding Frequency - (pond_freq),pond_freq,,,field,factor,,,,,
  site,runoff,Local Runoff Class - (runoff),runoff,,,field,factor,,,,,
  site,site_perm,Site Permeability - (site_perm),site_perm,,,field,factor,,,,,
  site,water_table_duration,Wet Soil Moisture Duration - days,water_table_duration,,,field,numeric,,,,,
  site,cflux_note,C Flux Notes - ,cflux_note,,,field,character,,,,,
  site,climate_note,Climate Notes - ,climate_note,,,field,character,,,,,
  site,eco_note,Ecoregion Note - ,eco_note,,,field,character,,,,,
  site,photo_note,Photo Notes - ,photo_note,,,field,character,,,,,
  site,soiltemp_note,Soil Temperature Notes - ,soiltemp_note,,,field,character,,,,,
  site,add_note,Additional Data Note - ,add_note,,,field,character,,,,,
  site,soc,Site Calculated Soil Organic Carbon Stock - g cm-2,soc,,,sample,numeric,value,g cm-2,,,
  site,soc_type,Site Calculated Soil Organic Carbon Stock Type - (ccon_type),soc,,,sample,character,method,,,,
  site,soc_pcount,Site Calculated Soil Organic Carbon Stock Profile Count - ,soc,,,sample,character,method,,,,
  site,soc_depth,Site Calculated Soil Organic Carbon Stock Depth - cm,soc_depth,,,sample,numeric,value,cm,,,
  site,soc_sigma,Site Calculated Soil Organic Carbon Stock Standard Deviation - g cm-2,soc,,,sample,numeric,sigma,,,,
  site,soc_method,Site Calculated Soil Organic Carbon Stock Method - ,soc,,,sample,character,method,,,,
  cluster,cluster_name,Cluster Name - ,cluster_name,,,field,factor,,,,,
  cluster,cluster_note,Cluster Notes - ,cluster_note,,,field,character,,,,,
  profile,profile_name,Profile/Plot Name - ,profile_name,name for profile,field_id,field,factor,,,,,
  profile,profile_note,Profile Notes - ,profile_note,,,field,character,,,,,
  profile,profile_method,Profile Sampling Method - ,profile_method,,,field,character,,,,,
  profile,observation_date,Observation Date - YYYY-MM-DD or MM/DD/YYYY,observation_date,day the sample was taken,,field,character,,,,,
  profile,observation_date_acc,Observation Date Accuracy - (date_qual),observation_date_acc,,,field,character,,,,,
  profile,transect_flag,Part of a Transect Flag - (yes_blank),transect_flag,,,field,factor,,,,,
  profile,surface_veg,Surface Vegetation - ,surface_veg,,,field,character,,,,,
  profile,soil_taxon,Soil Taxonomy - ,soil_taxon,soil taxon,,field,character,,,,,
  profile,soil_series,Soil Series - ,soil_series,soil series name,,field,character,,,,,
  profile,add_taxon_flag,Complete Taxonomy Flag - (yes_blank),add_taxon_flag,,,field,factor,,,,,
  profile,thaw_depth_profile,Thaw Depth of the Profile - cm,thaw_depth_profile,,,field,numeric,,,,,
  profile,sampler_names,Sampler Names - ,sampler_PersonName,,,field,factor,,,,,
  profile,soc_lcount,Profile Calculated Soil Organic Carbon Stock Layer Count - ,soc,,,sample,character,method,,,,
  layer,layer_name,Layer Name - ,layer_name,name for layer,field_id,field,factor,,,,,
  layer,layer_number,Laboratory Layer Number - ,layer_number,,,field,character,,,,,
  layer,labeled_addition,Labeled Addition - (label_add),labeled_addition,,,treatment,factor,,,,,
  layer,layer_top,Layer Top - cm,layer_top,depth to top of the layer,,field,numeric,,,,,
  layer,layer_bot,Layer Bottom - cm,layer_bottom,depth to bottom of the layer,,field,numeric,,,,,
  layer,hzn_desgn,Horizon Designation - ,hzn_desgn,horizon desigination (detailes TBD),,field,character,,,,,
  layer,hzn_desgn_other,Horizon Designation Other - ,hzn_desgn_other,other horizon desigination,,field,character,,,,,
  layer,hzn_desgn_other_note,Horizon Designation Other Note - ,hzn_desgn_other_note,,,field,character,,,,,
  layer,layer_note,Soil Layer Notes - ,layer_note,,,field,character,,,,,
  layer,c_method,Bulk Layer Carbon Analysis Method - ,(soc)|(c_tot),,,sample,character,method,,,,
  layer,color,Moist Munsell Color - ,color,soil color,,field,character,,,,,
  layer,burn_ev,Evidence of Burning - (burn),burn_ev,,,field,factor,,,,,
  layer,bd_method,Bulk Density Method - ,^bd_*,,,sample,character,method,,,,
  layer,bd_samp,"Bulk Density, Coarse Fragments Removed - g cm-3",bd_sample,bulk density of the <2mm (fine earth) over-dried fraction,,sample,numeric,value,g cm-3,,,
  layer,bd_tot,Bulk Density With Coarse Fragments - g cm-3,bd_tot,whole soil bulk density - includes fine earth fraction and rocks,,sample,numeric,value,g cm-3,,,
  layer,bd_whole,Bulk Density Minus Estimated Coarse Fragments - g cm-3,bd_whole,whole soil bulk density by clod method at 1/3 bar moisture content,,sample,numeric,value,g cm-3,,,
  layer,bd_other,Bulk Density Other - g cm-3,bd_other,non-standard,,sample,numeric,value,g cm-3,,,
  layer,ph_method,pH Method - ,^ph_*,,,sample,character,method,,,,
  layer,ph_cacl,Soil pH CaCl2 - ,ph_cacl,pH from 1:2 Soil-CaCl2 suspension,,sample,numeric,value,,,,
  layer,ph_h2o,Soil pH 1:1 - ,ph_h2o,pH from 1:1 Soil-water suspension,,sample,numeric,value,,,,
  layer,ph_other,Soil pH Other - ,ph_other,pH non-standard,,sample,numeric,value,,,,
  layer,caco3,CaCO3 - percent,caco3,Percent inorganic carbon ,,sample,numeric,value,percent,,,
  layer,sand_tot_psa,Percent Sand - percent,sand_tot_psa,percentage of sand,,sample,numeric,value,percent,,,
  layer,silt_tot_psa,Percent Silt - percent,silt_tot_psa,percentage of silt,,sample,numeric,value,percent,,,
  layer,clay_tot_psa,Percent Clay - percent,clay_tot_psa,percentage of clay,,sample,numeric,value,percent,,,
  layer,wpg2_method,Coarse Fragments Method - ,wpg2,,,sample,character,method,,,,
  layer,wpg2,Coarse Fragments - percent,wpg2,coarse fragment (>2mm) content by weight,,sample,numeric,value,percent,,,
  layer,cat_exch,Cation Exchange Capacity - cmol H+ kg-1,cat_exch,,,sample,numeric,value,cmol H+ kg-1,,,
  layer,gwc,Gravimentric % Moisture - percent,gwc,,,sample,numeric,value,percent,,,
  layer,vwc,Volumetric % Moisture - percent,vwc,,,sample,numeric,value,percent,,,
  layer,soc,Bulk Layer Calculated Soil Organic Carbon Stock - g cm-2,soc,soil organic carbon,,sample,numeric,value,g cm-2,,,
  layer,soc_type,Bulk Layer Calculated Soil Organic Carbon Stock Type - (ccon_type),soc,,,sample,character,method,,,,
  layer,soc_sigma,Bulk Layer Calculated Soil Organic Carbon Stock Standard Deviation - g cm-2,soc,,,sample,numeric,sigma,,,,
  layer,soc_method,Bulk Layer Calculated Soil Organic Carbon Stock Method - ,soc,,,sample,character,method,,,,
  layer,c_tot,Bulk Layer Total Carbon - percent,c_tot,carbon concentration in a dry combustion analysis,,sample,numeric,value,percent,,,
  layer,oc,Bulk Layer Organic Carbon - percent,oc,organic carbon either acidification+dry combustion or Walkly-Black,,sample,numeric,value,percent,,,
  layer,n_tot,Bulk Layer Total Nitrogen - percent,n_tot,total nitrogen,,sample,numeric,value,percent,,,
  layer,c_to_n,Bulk Layer C:N - mass percent,c_to_n,carbon to nitrogen ratio,,sample,numeric,value,mass percent,,,
  layer,loi,Bulk Layer Loss on Ignition - percent,loi,total mass loss on ignition,,sample,numeric,value,percent,,,
  layer,root_quant_size,Root Quantity and Size - ,root_weight,,,sample,character,method,,,,
  layer,root_weight,<2mm Root Mass - g,root_weight,mass of roots in sample,,sample,numeric,value,g,,,
  layer,15n,Bulk Layer δ15N - permil,15n,15N isotope ratio,,sample,numeric,value,permil,,,
  layer,13c,Bulk Layer δ13C - permil,13c,13C isotope ratio,,sample,numeric,value,permil,,,
  layer,rc_lab,Radiocarbon Laboratory ID - (rc_lab),(15n)|(13c)|(14c_*),,,sample,character,method,,,,
  layer,rc_lab_number,Radiocarbon Laboratory Sample Number - ,(15n)|(13c)|(14c_*),,,sample,character,method,,,,
  layer,rc_year,Radiocarbon Analysis Year - YYYY,(15n)|(13c)|(14c_*),,,sample,character,method,,,,
  layer,14c,Bulk Layer Δ14C - permil,14c,14C isotope ratio,,sample,numeric,value,permil,,,
  layer,14c_sigma,Bulk Layer Δ14C Standard Deviation - permil,14c,,,sample,numeric,sigma,,,,
  layer,14c_age,Bulk Layer Uncalibrated Radiocarbon Age - BP,14c_age,Age from 14C,,sample,numeric,value,BP,,,
  layer,14c_age_sigma,Bulk Layer Uncalibrated Radiocarbon Age Standard Deviation - BP,14c_age,,,sample,numeric,sigma,,,,
  layer,fraction_modern,Bulk Layer Fraction Modern - ,fraction_modern,fraction of modern from other isotope,,sample,numeric,value,,,,
  layer,fraction_modern_sigma,Bulk Layer Fraction Modern Standard Deviation - ,fraction_modern,,,sample,numeric,sigma,,,,
  layer,textureClass,Texture Class - ,textureClass,texture class,,field,character,,,,,
  fraction,fraction_name,Fraction Sample Name - ,,,,treatment,character,,,,,
  fraction,fraction_number,Laboratory Fraction Number - ,,,,treatment,character,method,,,,
  fraction,fraction_scheme,Fractionation Scheme - (fract_scheme),,,,treatment,factor,value,(fract_scheme),,,
  fraction,fraction_property,Fraction Property - ,,,,treatment,character,method,,,,
  fraction,fraction_scheme_units,Fractionation Scheme Units - ,,,,treatment,character,method,,,,
  fraction,fraction_type,Fraction Type - ,,,,treatment,character,method,,,,
  fraction,fraction_agent,Fractionation Agent - ,,,,treatment,character,method,,,,
  fraction,fraction_note,Fraction Notes - ,,,,treatment,character,method,,,,
  fraction,fraction_c_perc,Fraction proportion of sample carbon - percent,,,,sample,numeric,value,percent,,,
  fraction,fraction_mass_perc,Fraction proportion of sample mass - percent,,,,sample,numeric,value,percent,,,
  gas,gas_name,Gas Sample Name - ,,,,sample,,,,,,
  gas,gas_number,Laboratory Gas Sample Number - ,,,,sample,character,method,,,,
  gas,gas_type,Gas Type - (gas_type),,,,sample,factor,method,,,,
  gas,gas_note,Gas Sample Notes - ,,,,sample,character,method,,,,
  gas,gas_time_series,Gas Time Series Name - ,,,,sample,character,method,,,,
  gas,c_method,Gas Carbon Analysis Method - ,,,,sample,character,method,,,,
  gas,collection_method,Gas Sample Collection Method - (core_pit),,,,sample,factor,method,,,,
  gas,collection_loc,Field Vs. Incubation - ,,,,sample,character,method,,,,
  gas,15n,Gas δ15N - permil,,,,sample,numeric,value,permil,,,
  gas,c_conc_CO2,CO2 Concentration - ppm,,,,sample,numeric,value,ppm,,,
  gas,13c_CO2,Gas δ13C (C02) - permil,,,,sample,numeric,value,permil,,,
  gas,14c_CO2,Gas Δ14C (C02) - permil,,,,sample,numeric,value,permil,,,
  gas,14c_CO2_sigma,Gas Δ14C Standard Deviation (CO2) - permil,,,,sample,numeric,sigma,,,,
  gas,14c_age_CO2,Gas Uncalibrated Radiocarbon Age (CO2) - ,,,,sample,numeric,value,,,,
  gas,14c_age_CO2_sigma,Gas Uncalibrated Radiocarbon Age Standard Deviation (CO2) - ,,,,sample,numeric,sigma,,,,
  gas,fraction_modern_CO2,Gas Fraction Modern (CO2) - ,,,,sample,numeric,value,,,,
  gas,fraction_modern_CO2_sigma,Gas Fraction Modern Standard Deviation (CO2) - ,,,,sample,numeric,sigma,,,,
  gas,c_conc_CH4,CH4 Concentration - ppm,,,,sample,numeric,value,ppm,,,
  gas,13c_CH4,Gas δ13C (CH4) - permil,,,,sample,numeric,value,permil,,,
  gas,14c_CH4,Gas Δ14C (CH4) - permil,,,,sample,numeric,value,permil,,,
  gas,14c_CH4_sigma,Gas Δ14C Standard Deviation (CH4) - permil,,,,sample,numeric,sigma,,,,
  gas,14c_age_CH4,Gas Uncalibrated Radiocarbon Age (CH4) - ,,,,sample,numeric,value,,,,
  gas,14c_age_CH4_sigma,Gas Uncalibrated Radiocarbon Age Standard Deviation (CH4) - ,,,,sample,numeric,sigma,,,,
  gas,fraction_modern_CH4,Gas Fraction Modern (CH4) - ,,,,sample,numeric,value,,,,
  gas,fraction_modern_CH4_sigma,Gas Fraction Modern Standard Deviation (CH4) - ,,,,sample,numeric,sigma,,,,
  other,other_name,Other Sample Name - ,,,,field,character,,,,,
  other,other_number,Laboratory Other Sample Number - ,,,,field,character,,,,,
  other,other_type,Other Sample Type - (other_type),,,,field,character,,,,,
  other,other_note,Other Sample Type Notes - ,,,,field,character,,,,,
  other,collection_method,Other Sample Collection Method - (core_pit),,,,field,factor,,,,,
  other,processing_method,Other Sample Processing Method - ,,,,field,character,,,,,
  other,plant_species,Plant Species - ,,,,field,character,,,,,
  other,root_live_flag,Roots Live/Dead - ,,,,field,character,,,,,
  other,root_quant_size,Root Quantity and Size - ,,,,field,character,,,,,
  other,compound_formula,Compound Chemical Formula - ,,,,field,character,,,,,
  other,compound,Compound Name - ,,,,field,character,,,,,
  disturbance,dist_clearcut_date,Forest clearcutting date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_clearcut_removal_type,Clearcut residue removal type - (dist_residue),,,,field,character,,(dist_residue),,,
  disturbance,dist_clearcut_removal_perc,Clearcut residue percent removed - percent,,,,field,character,,percent,,,
  disturbance,dist_clearcut_date_qual,Forest clearcutting date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_clearcut_date_note,Forest clearcutting date comments - ,,,,field,character,,,,,
  disturbance,dist_crop_residue_date,Crop residue management other than at harvest date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_crop_residue_type,Crop residue removal type - (dist_residue),,,,field,character,,(dist_residue),,,
  disturbance,dist_crop_residue_perc,Crop residue percent remaining on the field - percent,,,,field,character,,percent,,,
  disturbance,dist_crop_residue_date_qual,Crop residue management other than at harvest date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_crop_residue_date_note,Crop residue management other than at harvest date comments - ,,,,field,character,,,,,
  disturbance,dist_fire_date,Fire other than wildfire date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_fire_severity,Fire severity - (dist_fire),,,,field,character,,(dist_fire),,,
  disturbance,dist_fire_date_qual,Fire other than wildfire date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_fire_date_note,Fire other than wildfire date comments - ,,,,field,character,,,,,
  disturbance,dist_fwd_removal_date,Fallen wood removal other than by underburning date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_fwd_removal_perc,Fallen wood removal percent removed - percent,,,,field,character,,percent,,,
  disturbance,dist_fwd_removal_date_qual,Fallen wood removal other than by underburning date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_fwd_removal_date_note,Fallen wood removal other than by underburning date comments - ,,,,field,character,,,,,
  disturbance,dist_general_date,General disturbance date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_general_date_qual,General disturbance date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_general_date_note,General disturbance date comments - ,,,,field,character,,,,,
  disturbance,dist_grazed_date,Monthly grazing date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_grazed_spp,Grazing animal type - ,,,,field,character,,,,,
  disturbance,dist_grazed_stocking,Stocking rate of grazing animals - ,,,,field,character,,,,,
  disturbance,dist_grazed_live_weight,Estimated live weight of grazing animals - ,,,,field,character,,,,,
  disturbance,dist_grazed_onplot,Days per month grazing animals on the plot - ,,,,field,character,,,,,
  disturbance,dist_grazed_daynight,Day/night grazing animal management - ,,,,field,character,,,,,
  disturbance,dist_grazed_date_qual,Monthly grazing date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_grazed_date_note,Monthly grazing date comments - ,,,,field,character,,,,,
  disturbance,dist_harvest_date,Crop harvest date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_harvest_residue_type,Harvest residue removal type - (dist_residue),,,,field,character,,(dist_residue),,,
  disturbance,dist_harvest_residue_perc,Harvest residue percent remaining on the field - percent,,,,field,character,,percent,,,
  disturbance,dist_harvest_spp,Harvested species - ,,,,field,character,,,,,
  disturbance,dist_harvest_date_qual,Crop harvest date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_harvest_date_note,Crop harvest date comments - ,,,,field,character,,,,,
  disturbance,dist_herbicide_date,Application of herbicide date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_herbicide_type,Herbicide type - ,,,,field,character,,,,,
  disturbance,dist_herbicide_kgha,Applied herbicide amount - Kg ha-1,,,,field,character,,Kg ha-1,,,
  disturbance,dist_herbicide_date_qual,Application of herbicide date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_herbicide_date_note,Application of herbicide date comments - ,,,,field,character,,,,,
  disturbance,dist_insect_and_pathogen_date,Insects and pathogens date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_insect_and_pathogen_date_qual,Insects and pathogens date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_insect_and_pathogen_date_note,Insects and pathogens date comments - ,,,,field,character,,,,,
  disturbance,dist_irrigation_date,Irrigation date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_irrigation_water,Amount of irrigation water applied during irrigation - mm,,,,field,character,,mm,,,
  disturbance,dist_irrigation_drain_depth,Irrigation drainage depth - m,,,,field,character,,m,,,
  disturbance,dist_irrigation_date_qual,Irrigation date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_irrigation_date_note,Irrigation date comments - ,,,,field,character,,,,,
  disturbance,dist_liming_date,Application of lime date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_liming_kgha,Applied lime amount - Kg ha-1,,,,field,character,,Kg ha-1,,,
  disturbance,dist_liming_date_qual,Application of lime date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_liming_date_note,Application of lime date comments - ,,,,field,character,,,,,
  disturbance,dist_m_fertilization_date,Mineral fertilization date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_m_fertilization_type,Type of mineral fertilizer - (dist_m_fert_type),,,,field,character,,(dist_m_fert_type),,,
  disturbance,dist_m_fertilization_kgha,Amount of mineral fertilizer applied - Kg ha-1,,,,field,character,,Kg ha-1,,,
  disturbance,dist_m_fertilization_form,"Chemical form of mineral N, P and K applied - (dist_m_fert_form)",,,,field,character,,(dist_m_fert_form),,,
  disturbance,dist_m_fertilization_method,Application methods for mineral fertilizer - (dist_m_fert_app),,,,field,character,,(dist_m_fert_app),,,
  disturbance,dist_m_fertilization_date_qual,Mineral fertilization date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_m_fertilization_date_note,Mineral fertilization date comments - ,,,,field,character,,,,,
  disturbance,dist_natural_regeneration_date,Natural regeneration date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_natural_regeneration_type,Regeneration by planting or natural vegetation - (dist_regen),,,,field,character,,(dist_regen),,,
  disturbance,dist_natural_regeneration_date_qual,Natural regeneration date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_natural_regeneration_date_note,Natural regeneration date comments - ,,,,field,character,,,,,
  disturbance,dist_o_fertilization_date,Organic fertilization date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_o_fertilization_type,Organic fertilizer type - ,,,,field,character,,,,,
  disturbance,dist_o_fertilization_kgha,Applied organic fertilizer amount - Kg ha-1,,,,field,character,,Kg ha-1,,,
  disturbance,dist_o_fertilization_c,Estimated total C content of the organic fertilizer - percent,,,,field,character,,percent,,,
  disturbance,dist_o_fertilization_n,Estimated total N content of the organic fertilizer - percent,,,,field,character,,percent,,,
  disturbance,dist_o_fertilization_method,Application methods for organic fertilizer - ,,,,field,character,,,,,
  disturbance,dist_o_fertilization_date_qual,Organic fertilization date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_o_fertilization_date_note,Organic fertilization date comments - ,,,,field,character,,,,,
  disturbance,dist_pesticide_date,Application of pesticide or insecticide date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_pesticide_kgha,Applied pesticide amount - Kg ha-1,,,,field,character,,Kg ha-1,,,
  disturbance,dist_pesticide_type,Pesticide type - ,,,,field,character,,,,,
  disturbance,dist_pesticide_date_qual,Application of pesticide or insecticide date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_pesticide_date_note,Application of pesticide or insecticide date comments - ,,,,field,character,,,,,
  disturbance,dist_planting_date,Sowing or planting date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_planting_seed,Seeds per hectare planted or sowed - N ha-1,,,,field,character,,N ha-1,,,
  disturbance,dist_planting_spp,Planted species - ,,,,field,character,,,,,
  disturbance,dist_planting_date_qual,Sowing or planting date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_planting_date_note,Sowing or planting date comments - ,,,,field,character,,,,,
  disturbance,dist_storm_date,Severe storm date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_storm_date_qual,Severe storm date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_storm_date_note,Severe storm date comments - ,,,,field,character,,,,,
  disturbance,dist_thinning_date,Thinning other than clear cutting date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_thinning_removal_type,Thinning residue removal type - (dist_residue),,,,field,character,,(dist_residue),,,
  disturbance,dist_thinning_perc,Thinning percent of thinning - percent,,,,field,character,,percent,,,
  disturbance,dist_thinning_basal_area,Percent of basal area after thinning - ,,,,field,character,,,,,
  disturbance,dist_thinning_date_qual,Thinning other than clear cutting date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_thinning_date_note,Thinning other than clear cutting date comments - ,,,,field,character,,,,,
  disturbance,dist_tillage_date,Tillage or site preparation date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_tillage_type,Tillage type - (dist_tillage),,,,field,character,,(dist_tillage),,,
  disturbance,dist_tillage_depth,Tillage depth - m,,,,field,character,,m,,,
  disturbance,dist_tillage_date_qual,Tillage or site preparation date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_tillage_date_note,Tillage or site preparation date comments - ,,,,field,character,,,,,
  disturbance,dist_ungrazed_date,Site was not grazed date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_ungrazed_duration,Number of months after the disturbance date that no grazing occurred - ,,,,field,character,,,,,
  disturbance,dist_ungrazed_date_qual,Site was not grazed date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_ungrazed_date_note,Site was not grazed date comments - ,,,,field,character,,,,,
  disturbance,dist_wildfire_date,Wildfire date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_wildfire_severity,Fire severity - (dist_fire),,,,field,character,,(dist_fire),,,
  disturbance,dist_wildfire_date_qual,Wildfire date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_wildfire_date_note,Wildfire date comments - ,,,,field,character,,,,,
  disturbance,dist_underburn_date,Fallen wood removal by underburning date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_underburn_perc,Underburnning percent removed - percent,,,,field,character,,percent,,,
  disturbance,dist_underburn_date_qual,Fallen wood removal by underburning date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_underburn_date_note,Fallen wood removal by underburning date comments - ,,,,field,character,,,,,
  disturbance,dist_windthrow_date,Wind throw date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_windthrow_perc,Wind throw percent removed - percent,,,,field,character,,percent,,,
  disturbance,dist_windthrow_date_qual,Wind throw date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_windthrow_date_note,Wind throw date comments - ,,,,field,character,,,,,
  disturbance,dist_woody_encroachment_date,Woody encroachment date - YYYY-MM-DD or MM/DD/YYYY,,,,field,character,,YYYY-MM-DD or MM/DD/YYYY,,,
  disturbance,dist_woody_encroachment_date_qual,Woody encroachment date qualifier - (date_qual),,,,field,character,,(date_qual),,,
  disturbance,dist_woody_encroachment_date_note,Woody encroachment date comments - ,,,,field,character,,,,,')
  
  ###### dowload and datafiles identified #########
  delete_dataDir <- is.null(dataDir)
  if(is.null(dataDir)){
    dataDir <- tempdir()
  }
  
  download.url <- c(Treat='http://iscn.fluxdata.org/wp-content/uploads/sites/15/ISCNtemplate_Treat_peatProps_v2.xlsx',
                    Alamos='http://iscn.fluxdata.org/wp-content/uploads/sites/15/ISCNtemplate_Alamos.xlsx')
  datafile.arr <- file.path(dataDir, basename(download.url))
  names(datafile.arr) <- names(download.url)
  
  ###### Download the data #########
  for(dataset in names(datafile.arr)){
    if(!file.exists(datafile.arr[dataset])){
      download.file(download.url[dataset], datafile.arr[dataset] , quiet=FALSE)
    }
  }
  
  ####### Alamose has some formatting issues so let's deal with that first ####
  AlamosDownload <- datafile.arr['Alamos']
  AlamosFilename <- file.path(dataDir, c('ISCNtemplate_Alamos_metadata.csv',
                                         'ISCNtemplate_Alamos_site.csv',
                                         'ISCNtemplate_Alamos_profile.csv',
                                         'ISCNtemplate_Alamos_layer.csv'))
  temp <-  readxl::read_excel(AlamosDownload, sheet='metadata')
  readr::write_csv(path=AlamosFilename[1],
                   x=temp) #metadata sheet is fine
  readr::write_csv(path=AlamosFilename[2],
                   x= readxl::read_excel(AlamosDownload, sheet='site')[-3,]) #remove empty row
  readr::write_csv(path=AlamosFilename[3],
                   readxl::read_excel(AlamosDownload, sheet='profile')[-3,]) #remove empty row
  readr::write_csv(path=AlamosFilename[4],
                   #remove empty row
                   readxl::read_excel(AlamosDownload, sheet='layer')[-3,] %>% 
                     #and fill in rows with non-NA values above
                     tidyr::fill(site_name) %>%
                     #make the layer name unique
                     dplyr::mutate(layer_name = paste(profile_name, layer_name, sep='-')))
  
  ###### Read in data ########
  Treat <- soilDataR::readKeyedData(filename=datafile.arr['Treat'], key.df=key.df, dropRows=1:2)
  Treat$long <- Treat$key %>% 
    dplyr::filter(!is.na(hardUnit)) %>% #units are the only thing hard coded here
    dplyr::select(var, hardUnit) %>% unique %>% 
    dplyr::right_join(Treat$long, by=c('var')) %>% 
    dplyr::rename(unit=hardUnit)
  
  Alamos <- soilDataR::readKeyedData(filename=AlamosFilename, 
                                     verticalTable = c(AlamosFilename[1]),
                                     key.df=key.df, dropRows=1:2)
  
  Alamos$long <- Alamos$key %>% 
    dplyr::filter(!is.na(hardUnit)) %>% #units are the only thing hard coded here
    dplyr::select(var, hardUnit) %>% unique %>% 
    dplyr::right_join(Alamos$long, by=c('var')) %>% 
    dplyr::rename(unit=hardUnit)
  
  ####### Merge data #########
  data.ls <- list()
  data.ls$study <- Alamos$wide %>% 
    dplyr::bind_rows(Treat$wide) %>% #merge two datasets
    dplyr::select_(.dots=intersect(names(.), c((Alamos$key %>% dplyr::filter(dataframe == 'study'))$var,
                                        (Treat$key %>% dplyr::filter(dataframe == 'study'))$var))) %>% #select vars
    unique %>%
    dplyr::group_by_(.dots=unique(c((Alamos$key %>% 
                                       dplyr::filter(dataframe == 'study', !is.na(flagID)))$var,
                             (Treat$key %>% 
                                dplyr::filter(dataframe == 'study', !is.na(flagID)))$var))) #group by key
  
  data.ls$field <- Alamos$wide %>% 
    dplyr::bind_rows(Treat$wide) %>% #merge two datasets
    dplyr::select_(.dots=intersect(names(.), 
                            c((Alamos$key %>% dplyr::filter(dataframe == 'field' |
                                                       !is.na(flagID)))$var,
                              (Treat$key %>% dplyr::filter(dataframe == 'field'|
                                                      !is.na(flagID)))$var))) %>% #select vars
    unique %>%
    dplyr::mutate_at(dplyr::vars('layer_top', 'layer_bottom', 'lon', 'lat'), dplyr::funs(as.numeric)) %>%
    dplyr::mutate(observation_date =  ifelse(grepl('/', observation_date),
                         #this will return warning messages as it tries to evaluate non-'/' dates
                                   lubridate::decimal_date( lubridate::mdy(observation_date)), 
                                   as.numeric(observation_date))) %>%
    dplyr::mutate_at(dplyr::vars(-lon, -lat, -layer_top, -layer_bottom, -observation_date),
                     dplyr::funs(factor)) %>%
    dplyr::group_by(dataset_name, layer_name, profile_name, site_name) #group by names
  
  data.ls$sample <- Alamos$long %>% 
    dplyr::mutate(dataset_name=unique(Alamos$wide$dataset_name)) %>%
    dplyr::bind_rows(Treat$long %>% 
                       dplyr::mutate(dataset_name=unique(Treat$wide$dataset_name))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(dplyr::vars(-value, -sigma),dplyr::funs(factor)) %>%
    dplyr::mutate_at(dplyr::vars('value', 'sigma'), dplyr::funs(as.numeric)) %>%
    dplyr::group_by_(.dots=dplyr::groups(data.ls$field))
  
  ###### Clean up ##########
  if(delete_dataDir){
    unlink(dataDir, recursive = T)
  }
  data.ls$ISCN2016_key <- key.df
  return(data.ls)
}