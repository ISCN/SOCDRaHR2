#' Empty data skeleton
#'
#' This function returns an empty list of data frames with the internal standard fields.
#' You can either use this as a basis to build new ingestion scripts and this is the
#'  structure that scripts should be tested against in testthat checks.
#'
#' @return A list that reflects the structure of the data representation
#' @export
#'
#' @examples
#' emptyFramework <- processData_emptyStandard()
processData_emptyStandard <- function(){
  ans <- list(study = data.frame(name = NA, doi = NA, permissions = NA),
              labTreatment = data.frame(labTreatmentID = NA),
              fieldTreatment = data.frame(fieldTreatmentID = NA),
              measurement = data.frame(type = NA, method = NA, measurementID = NA),
              sample = data.frame(fieldID = NA, measurementID = NA, value = NA, unit = NA),
              field =  c(fieldID = NA,
                         lat = NA, long = NA,
                         layer_top = NA, layer_bottom = NA, layer_units = NA,
                         observation_date = NA,
                         state = NA, country = NA#,
                         #site_name = NA, profile_name = NA, layer_name = NA,
                         #hzn_desgn_other = NA, hz = NA, hz_desgn = NA, layer_note = NA,
                         #color = NA, vegclass_local = NA, soil_taxon = NA, soil_series = NA,
                         #root_quant_size = NA, textureClass = NA,
                         #datum = NA, locator_parent_alias = NA
              ))

  return(ans)
}
