#' Write out data to the Powell Center Working Group format as specified by a template file.
#'
#' WARNING currently there is no QA/QC to enforce unit harmonziation.
#'
#' @param data.df data frame with internal formating and a column (ISCN_id) specifying the harnomized measure names
#' @param outputDir target directory string for the outputs
#' @param templatefile string pointing to a template file
#'
#' @return returns a list of dataframes that reflect the output
#' @export
outputData_PowellCenter <- function(data.df, outputDir, templatefile){

  #templatefile='templates/PowellCenterTemplate_With Minerals and Dissolutions.xlsx'
  sheetHeaders <- list(metadata = readxl::read_excel(path=templatefile, sheet='metadata'),
                       site = readxl::read_excel(path=templatefile, sheet='site'),
                       profile=readxl::read_excel(path=templatefile, sheet='profile'),
                       layer = readxl::read_excel(path=templatefile, sheet='layer'),
                       fraction = readxl::read_excel(path=templatefile, sheet='fraction'),
                       control_vocab = readxl::read_excel(path=templatefile,
                                                          sheet='controlled vocabulary',
                                                          skip=1))

  metadata.df <- data.df$study %>%
    rename(dataset_name = studyID, doi_number=doi) %>%
    select(one_of(names(sheetHeaders$metadata))) %>%
    select(one_of(intersect(names(sheetHeaders$metadata), names(.))))

  site.df <- data.df$field %>%
    select(one_of(names(sheetHeaders$site))) %>%
    unique %>%
    mutate(profil = data.df$study$studyID) %>%
    select(one_of(intersect(names(sheetHeaders$site), names(.))))

  profile.df <- data.df$field %>%
    mutate(profile_name = profile_note) %>%
    select(one_of(names(sheetHeaders$profile))) %>%
    unique %>%
    mutate(dataset_name=data.df$study$studyID) %>%
      select(one_of(intersect(names(sheetHeaders$profile), names(.))))

    layerFromField <- data.df$field %>%
      select(one_of(c('fieldID',
                      intersect(names(sheetHeaders$layer), names(data.df$field))))) %>%
      unique

  layer.df <- data.df$sample %>%
    full_join(data.df$measurement[,c('measurementID', 'ISCN_id')]) %>%
    filter(is.na(labTreatmentID), #only take bulk measurements
           ISCN_id %in% names(sheetHeaders$layer)) %>%
    rename(fieldID=sampleID) %>%
    left_join(layerFromField) %>%
    select(-measurementID, -labTreatmentID) %>%
    spread(ISCN_id, value) %>%
    rename(layer_name=fieldID)%>%
    select(one_of(intersect(names(sheetHeaders$layer), names(.))))

  fraction.df <-  data.df$sample %>%
    full_join(data.df$measurement[,c('measurementID', 'ISCN_id')]) %>%
    mutate(fracID = sprintf('f_%s', ISCN_id)) %>%
    filter(!is.na(labTreatmentID), #only take bulk measurements
           fracID %in% names(sheetHeaders$fraction)) %>%
    rename(fieldID=sampleID, f_property=labTreatmentID) %>%
    left_join(layerFromField) %>%
    select(-measurementID, -ISCN_id) %>%
    spread(fracID, value) %>%
    rename(layer_name=fieldID) %>%
    select(one_of(intersect(names(sheetHeaders$fraction), names(.))))

  write.csv(metadata.df, file=sprintf('%s/metadata.csv', outputDir))
  write.csv(site.df, file=sprintf('%s/site.csv', outputDir))
  write.csv(profile.df, file=sprintf('%s/profile.csv', outputDir))
  write.csv(layer.df, file=sprintf('%s/layer.csv', outputDir))
  write.csv(fraction.df, file=sprintf('%s/fraction.csv', outputDir))
  write.csv(data.df$measurement, file=sprintf('%s/measurementTypes.csv', outputDir))

  ##buggy unfortunately
  # xlsx::write.xlsx(metadata.df, file=sprintf('%s/output.xlsx', outputDir),
  #                  sheetName='metadata', row.names=FALSE)
  # xlsx::write.xlsx(site.df, file=sprintf('%s/output.xlsx', outputDir),
  #                  sheetName='site', row.names=FALSE, append=TRUE)
  # xlsx::write.xlsx(profile.df, file=sprintf('%s/output.xlsx', outputDir),
  #                  sheetName='profile', row.names=FALSE, append=TRUE)
  # xlsx::write.xlsx(layer.df, file=sprintf('%s/output.xlsx', outputDir),
  #                  sheetName='layer', row.names=FALSE, append=TRUE)
  # xlsx::write.xlsx(fraction.df, file=sprintf('%s/output.xlsx', outputDir),
  #                  sheetName='fraction', row.names=FALSE, append=TRUE)


  return(list(metadata=metadata.df,
              site=site.df, profile=profile.df,
              layer=layer.df, fraction=fraction.df))
}
