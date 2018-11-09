ISCN5 <- function(dataDir){
  
  CUF2018 <- readCanandaUplandForest2018(dataDir=file.path(dataDir, 'CanUpSoils'))
  CPEAT2018 <- readCPEAT(dataDir=file.path(dataDir, 'CPEAT'), workLocal=TRUE)
  ##pull in https://daac.ornl.gov/SOILS/guides/Global_Microbial_Biomass_C_N_P.html
  
  #convert to 3tables
  
}