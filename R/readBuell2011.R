#' Raw read for USGS_S3C (2011)
#' 
#' Download and read in raw data from Myers-Smith, Isla. 2005. Soil data for cores from a transect from the center of the BBC collapse scar into the surrounding burn, Bonanza Creek LTER - University of Alaska Fairbanks. BNZ:192, http://www.lter.uaf.edu/data/data-detail/id/192. doi:10.6073/pasta/b0e9120983438c27bf1a30f37f0e5200
#' 
#' This is not post-processed or QA/QCed by this package.
#'
#' @param data_dir filename for download directory
#' @param download boolean that will download the files from repository
#'
#' @return a data frame containing newly ingested data from the Myers-Smith dataset
#' 
#' @export
#' @importFrom readr read_delim
#'
readBuell2011 <- function(data_dir, download=TRUE, verbose=FALSE){
  #### Download ####
  
  BuellDownload_url <- 'https://pubs.usgs.gov/of/2004/1227/s3c_ascii.zip'
  BuellDownload_target <- file.path(data_dir, 's3c_ascii.zip') 
  
  BuellDataFiles <- file.path(data_dir, 's3c_ascii',
                             c("horiz01.txt","pedon01.txt", "taxon01.txt"))
  
  if(!(file.exists(BuellDownload_target))){
    download.file(BuellDownload_url, BuellDownload_target, quiet=FALSE)
  }
  
  unzip(BuellDownload_target, exdir=file.path(data_dir, 's3c_ascii'))
  
  #### Read into table ####
  
  #BuellDataFiles <- readr::read_tsv(file = '~/Downloads/s3c_ascii/horiz01.txt')
  ans <- list(horiz01 = readr::read_tsv(file = '~/Downloads/s3c_ascii/horiz01.txt'),
              pedon01 = readr::read_tsv(file = '~/Downloads/s3c_ascii/pedon01.txt'),
              taxon01 = readr::read_tsv(file = '~/Downloads/s3c_ascii/taxon01.txt'),
              horiz01_filename = BuellDataFiles[1],
              pedon01_filename = BuellDataFiles[2],
              taxon01_filename = BuellDataFiles[3],
              dataCitation= "G.R. Buell, H.W. Markewich, R. Kulisek, S. Pollard, and T.T. Cook. Site-Specific Soil-Carbon (S3C) Database for Mineral Soils of the Mississippi River Basin, USA. USGS Open File Report 2004-1227. http://pubs.usgs.gov/of/2004/1227/.", 
              dataAbstract = "This dataset is derived from the USGS 'S3C' (Site-specific soil C) database. The database is a pedon-based, site-specific SOC database for the MS River Basin Carbon Project. The database contains data from a variety of sources described in the Buell et al. (2004) USGS Open File Report. Note that all data in this dataset that originated from the USDA-NRCS NCSCD in 2001 have been omitted; these data are superseded by those acquired directly from the USDA-NRCS NCSCD in 2011."
        )
  
  
  return(ans)
}