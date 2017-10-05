#' Ingest data from Berhe AA, Harden JW, Torn MS, Kleber M, Burton SD, Harte J, 2012, Persistence of soil organic matter in eroding versus depositional landense fraction (DF)orm positions, Journal of Geophysical Research, 117, 1-16
#' The excel file processed here was provided to the Power Center Working Group by AA Berhe.
#'
#' @param dir directory string where the Excel file is stored
#' @param verbose boolean flag for debugging statements (not used)
#'
#' @return return a list of dataframes that are consistant with the internal representation
#' @export
processData_Berhe2012 <- function(dir='repoData/Berhe2012', verbose=FALSE){

  #### Read in data ####
  #dir <- '../soils-long-tail-recovery/repoData/Berhe2012'
  meta.df <- readxl::read_excel(path=sprintf('%s/Copy of Berhe et al 2012 JGR-B data for Powell center-101416.xlsx', dir), sheet='metadata')
  data.df <- readxl::read_excel(path=sprintf('%s/Copy of Berhe et al 2012 JGR-B data for Powell center-101416.xlsx', dir), sheet='data')
  radioCarbon.df <- readxl::read_excel(path=sprintf('%s/Copy of Berhe et al 2012 JGR-B data for Powell center-101416.xlsx', dir), sheet='radiocarbon')

  #### Clean up data ####
  names(meta.df) <- c('colName', 'description')
  meta.df <- meta.df %>% filter(!is.na(meta.df$colName) & !grepl('^SHEET', colName))

  radioCarbon.df <- radioCarbon.df %>%
    select(1:6) %>%
    rename(mid_depth=`mid depth of sampled layer`) %>%
    filter(is.finite(mid_depth))

  #### Set up keys ####
  ISCNname_fieldKey <- list("sample id" = NA,
                            "landform position" = 'profile_note',
                            "depth (top)"= 'layer_top',
                            "mid depth" = NA,
                            "thickness" = NA,
                            "mid depth of sampled layer" = NA)

  ISCNname_measurmentKey <- list(
    "bulk density (g/cm3)" = 'bd_sample',
    #Blake and Hartge, 1986; core and clod methods used
    "rock fraction" = 'wpg2', #check this!
    "CEC (meq/100g soil)" = 'cec_sum',
    #check this! barium acetate, calcium replacement technique [Janitzky, 1986]
    "Mn (DTPA) (ppm)" = NA, #Not clear on what this is
    "Sand (%)" = 'sand_tot_psa',
    "Silt (%)" = 'silt_tot_psa',
    "Clay (%)" = 'clay_tot_psa',
    "Al (Citrate) (g/kg)" = 'al_dith',
    "Al (Oxalate) (g/kg)" = 'al_ox',
    "Al (PyroPO4) (g/kg)" = 'al_other',
    "Fe (Oxalate) (g/kg)" = 'fe_ox',
    "Fe (PyroPO4) (g/kg)" = 'fe_other',
    "Fe (Citrate) (g/kg)" = 'fe_dith',
    "Fe(ox-py) (g/kg)" = NA,
    "Al (ox-py) (g/kg)" = NA,
    "Fe(d-ox) (g/kg)" = NA,
    "Mox (g/kg)" = NA,
    "Mpy (g/kg)" = NA,
    "M(ox-py) (g/kg)" = NA,
    "Fe(o/d)"= NA,
    "C:Mpy"= NA,
    "C (g/kg)" = 'c_tot',
    "C/N (bulk)" = 'c_to_n',
    "C inv (g/m2)" = 'soc', #??is this right
    ##fractionation notation is delt with seperately,
    ##...include it here for completeness but only key the
    ##...fractionation-stripped strings
    'D14C' = '14c',
    "N (g/kg)" = 'n_tot',
    "C:N" = 'c_to_n',
    "15N" = '15n',
    "13C" = '13c',
    "HF-N (g/kg)" = NA,
    "HF-15N" = NA,
    "HF-C (g/kg)" = NA,
    "HF-13C" = NA,
    "HF-C:N" = NA,
    "fLF-N (g/kg)" = NA,
    "fLF-15N" = NA,
    "fLF-C (g/kg)" = NA,
    "fLF-13C" = NA,
    "fLF-C:N" = NA,
    "oLF-N (g/kg)" = NA,
    "oLF-15N" = NA,
    "oLF-C (g/kg)" = NA,
    "oLF-13C" = NA,
    "oLF-C:N" = NA,
    "D14C_Bulk" = NA,
    "D14C_fLF" =NA,
    "D14C_oLF" = NA,
    "D14C_HF" = NA)
  ISCNKey.df <- data.frame(measurementID=names(c(ISCNname_fieldKey,ISCNname_measurmentKey)),
                           ISCN_id=unlist(c(ISCNname_fieldKey,ISCNname_measurmentKey))) %>%
    mutate(units = stringr::str_extract(measurementID,
                                        '((g/cm3)|(ppm)|(%)|(meq/100g soil)|(g/kg))'))

  treatment.df <- data.frame(labTreatmentID=c('HF', 'fLF', 'oLF'),
                             description=c('heavy fraction see Berhe et al 2012 JGR-B',
                                           'free light fraction see Berhe et al 2012 JGR-B',
                                    'occluded light fraction see Berhe et al 2012 JGR-B'))

  ##Pull sample measurements
  temp <- radioCarbon.df %>%
    gather(measurementID, value, D14C_Bulk:D14C_HF) %>%
    rename(`mid depth`=mid_depth) %>%
    mutate(`landform position` = tolower(`landform position`)) %>%
    separate(measurementID, c('measurementID', 'labTreatmentID'), sep='_') %>%
    mutate(`sample id` = paste(`landform position`,
                               `mid depth`, sep='_')) %>%
    full_join(ISCNKey.df, by='measurementID') %>%
    filter(is.finite(value))

  sample.df <- data.df %>%
    select(-contains('ox-py'), -contains('d-ox')) %>% #remove difference columns
    select(`sample id`, `bulk density (g/cm3)`:`oLF-C:N`) %>%
    gather(measurementID, value, `bulk density (g/cm3)`:`oLF-C:N`) %>%
    separate(measurementID, c('labTreatmentID', 'measurementID'), sep='-', fill='left') %>%
    full_join(temp[, c('sample id', 'measurementID', 'labTreatmentID', 'value')]) %>%
    left_join(ISCNKey.df, by='measurementID') %>%
    rename(sampleID=`sample id`)

  sample.df$labTreatmentID[sample.df$labTreatmentID == 'Bulk'] <- NA

  #### pull field id ####
  layerTemp <- data.df %>%
    select(`landform position`:`thickness`) %>%
    unique

  temp <- temp %>%
    left_join(layerTemp) ##pull layer information

  field.df <- data.df %>%
    select(`sample id`:`thickness`) %>%
    unique %>%
    full_join(temp[, c('sample id', "landform position",
                       "mid depth", "depth (top)", "thickness")]) %>%
    rename(layer_top=`depth (top)`, profile_note = `landform position`) %>%
    mutate(layer_bot = layer_top + thickness)%>%
    rename(fieldID=`sample id`) %>%
    full_join(data.frame(profile_note = c("summit", "slope", "plain", "hollow"),
                         soil_taxon=c('Lithic Haplustolls','Lithic Haplustolls',
                                      "Oxyaquic Haplustolls", "Oxyaquic Haplustolls")),
              by='profile_note') %>%
    mutate(observation_date = NA,
           state = 'California', county='Marin', country = 'USA',
           lat='37.848 N', long='122.5318 W', #where did this come from?
           map=1200, mat=14, climate_cat='coastal mediterranean',
           site_name='Tennessee Valley',
           datum='WGS84', #where is this from?
           parent_material='metamorphic',
           ecoregion='mediterranean',
           veg_note='coastal shrub',
           land_cover='shrubland')

  ans <- list(study= data.frame(studyID = 'Berhe_2012',
                                doi = '10.1029/2011JG001790',
                                curator_name='AA Berhe',
                                curator_organization='University of California, Merced',
                                curator_email='aaberhe@ucmerced.edu',
                                bibliographical_reference='Berhe AA, Harden JW, Torn MS, Kleber M, Burton SD, Harte J, 2012, Persistence of soil organic matter in eroding versus depositional landense fraction (DF)orm positions, Journal of Geophysical Research, 117, 1-16',
                                modification_date = NA,
                                permissions = NA),
              labTreatment=treatment.df,
              fieldTreatment = data.frame(fieldTreatmentID = NA),
              measurement = unique(sample.df[,c('measurementID', 'ISCN_id', 'units')]) %>%
                mutate(type=ISCN_id),
              field=field.df,
              sample=sample.df[c('sampleID', 'labTreatmentID', 'measurementID', 'value')])

  return(ans)
}
