#read1590Series <- function(dataDir, verbose=FALSE){
  library(tidyverse)
  dataDir <- '~/Documents/Datasets/USGS1590'
  
  download_info <- data.frame(names = sprintf('Part_%s_USGS_1590Series', c('a', 'b', 'c', 'd', 'e', 'f', 'g')),
                              pdf_url = c("https://pubs.usgs.gov/bul/1590a/report.pdf",
                                          "https://pubs.usgs.gov/bul/1590b/report.pdf",
                                          "https://pubs.usgs.gov/bul/1590c/report.pdf",
                                          "https://pubs.usgs.gov/bul/1590d/report.pdf",
                                          "https://pubs.usgs.gov/bul/1590e/report.pdf",
                                          "https://pubs.usgs.gov/bul/1590f/report.pdf",
                                          "https://pubs.usgs.gov/bul/1590g/report.pdf")) %>%
    mutate(download_filename = file.path(dataDir, paste0(names, '.pdf')))
  #https://pubs.usgs.gov/of/2002/0277/pdf/of02-277.pdf not part of 1590 but similar
  
  for(ii in 1:nrow(download_info)){
    if(!file.exists(download_info$download_filename[ii])){
      download.file(url=download_info$pdf_url[ii], destfile = download_info$download_filename[ii])
    }
  }
  
  ###Report A
  #need to mannually code key on pg36
  #need to manually code pg37
  
  reportA_txt <- pdftools::pdf_text(download_info$download_filename[1])
  
  #Field Tables pgs 38-41 = 4
  #Physical Properties pgs 42-44 = 3
  #Extractive chemical analyses pg 45-47 = 3
  #Extractive chemical analyses 2 pg 48-50 = 3
  #Mineralogy pg 50-53= 4
  #Total Chem analysis pg 53-57 = 5
  #Total Chem analysis - fraction 2 pg 57-61 = 5
  # 27 pages per report for 7 reports at 0.5 hours per page is 95 hours of work
  
  #report A - pg 38-73 => 36
  #report B - pg 29-41 => 13
  #report C - pg 32-47 => 16
  #report D - pg 42-79 => 38
  #report E - pg 24-29 =>  6
  #report F - pg 37-55 => 19
  #report G - pg 69-131 => 63
  #191 pages at 0.5 hours per page is 95.5 hours of work
  
  tableInfo.ls <- list(reportA = list(
    ################################
    #### Field description #########
    ################################
    field_desc = list(title = 'Field descriptions',
                      analysts = 'J. W. Harden U. S. Geological Survey',
                      columeNames = c('No', 'Sample', 'Horizon', 'Basal depth (cm)', 'Lower boundary', 
                                      'Moist color', 'Dry color', 'Texture', 'Structure',   
                                      'Consistence:Dry', 'Consistence:Moisture', 'Consitence:Wet',
                                      'Roots', 'Pores', 'Clay films', 'pH', 
                                      'Assumed Parent Material:Texture', 'Assumed Parent Material:Wet consistence'),
                      part1 = list(page = 38,
                                   columnCuts = c(10, 19, 30, 41, 51, 64, 75, 89, 105, 113, 124, 133, 143, 150, 156, 166, 176),
                                   subtables = c(rep(NA, 13), rep('Modern River Alluvium', 4), rep(NA, 4), 
                                                 rep('Post-Modesto Deposits, 0.2 Ka', 7), rep(NA, 2), rep('Post-Modesto Deposits, 0.2 Ka', 10), rep(NA, 4), 
                                                 rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 2), 
                                                 rep('Post-Modesto Deposits, 3 Ka', 11), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 9), rep(NA, 2), 
                                                 rep('Post-Modesto Deposits, 3 Ka', 10), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 2), 
                                                 rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 6))),
                      part2 = list(page = 39,
                                   columnCuts = c(7, 16, 26, 38, 48, 62, 75, 86, 102, 113, 124, 133, 143, 155, 165, 173, 184), #define column number splits
                                   subtables = c(rep(NA, 11), 
                                                 rep('Modesto Formation, upper member, 10 Ka', 19), rep(NA, 5), #define data rows by subtables
                                                 rep('Modesto Formation, lower member, 40 Ka', 1+46-36), rep(NA, 4),
                                                 rep('Riverbank Formation, upper member, 130 Ka', 12), rep(NA, 2),
                                                 rep('Riverbank Formation, upper member, 130 Ka', 20), rep(NA, 4),
                                                 rep('Riverbank Formation, middle member. 250 Ka', 8), rep(NA, 6))),
                      part3 = list(page = 40,
                                   columnCuts = c(6, 16, 26, 38, 48, 62, 75, 86, 102, 110, 121, 130, 138, 149, 159, 167, 174),
                                   subtables = c(rep(NA, 1+14-1), #inclusive rows between line 14 and 1 => 1 + 14 - 1
                                                 rep('Riverbank Formation, middle member, 250 Ka', 13), rep(NA, 3),
                                                 rep('Riverbank Formation, middle member, 250 Ka', 1+46-31), rep(NA, 1+52-47),
                                                 rep('Riverbank Formation, lower member, 330 Ka', 1+63-53), rep(NA, 1+69-64),
                                                 rep('Turlock Lake Formation, 600 Ka', 1+75-70), rep(NA, 1+78-76),
                                                 rep('Turlock Lake Formation, 600 Ka', 1+91-79), rep(NA, 1+94-92),
                                                 rep('Turlock Lake Formation, 600 Ka', 1+111-95), rep(NA, 1+117-112))),
                      part4 = list(page = 41,
                                   columnCuts = c(6, 16, 26, 38, 48, 60, 74, 84, 96, 105, 115, 125, 133, 144, 156, 165, 176),
                                   subtables = c(rep(NA, 1+14-1), #inclusive rows between line 14 and 1 => 1 + 14 - 1
                                                 rep('China Hat Gravel member of Laguna Formation, 3,000 Ka', 1+37-15), rep(NA, 1+40-38),
                                                 rep('China Hat Gravel member of Laguna Formation, 3,000 Ka', 1+61-41), rep(NA, 1+64-62),
                                                 rep('China Hat Gravel member of Laguna Formation, 3,000 Ka', 1+81-65), rep(NA, 1+87-82)))
    ),
    ##############################
    ##### Physical properties#####
    ##############################
    phys_prop = list(title = 'Physical properties',
                     analysts ='A.J. Busacca, Peter Janitsky, and R. Meixner, University of California, Davis', 
                     columeNames = c('No', 'Sample', 'Horizon', 'Basal depth (cm)', '>2-mm', 
                                     'Total sand', 'vco sand', 'co sand', 'm sand', 'fi+vfi sand', 'silt', '<2-\\mu clay', '< 1-\\mu clay', 
                                     'Bulk density (g/cm^3)'),
                     part1 = list(page = 42,
                                  columnCuts = c(13, 21, 35, 42, 51, 60, 68, 76, 84, 92, 101, 109, 118),
                                  subtables = c(rep(NA, 1+9-1), 
                                                rep('Modern River Alluvium', 1 + 13 - 10), rep(NA, 1+15-14), 
                                                rep('Post-Modesto Deposits, 0.2 Ka',1 + 29 - 16), rep(NA, 1+31-30),
                                                rep('Post-Modesto Deposits, 0.2 Ka',1 + 40-32 ), rep(NA, 1+41-41), 
                                                rep('Post-Modesto Deposits, 3 Ka', 1+51-42), rep(NA, 1+52-52), 
                                                rep('Post-Modesto Deposits, 3 Ka', 1+57-53), rep(NA, 1+58-58), 
                                                rep('Post-Modesto Deposits, 3 Ka', 1+69-59), rep(NA, 1+73-70))),
                     part2 = list(page = 43,
                                  columnCuts = c(7, 15, 25, 35, 44, 52, 60, 68, 76, 83, 91, 99, 107),
                                  #NA, 1+1-8; MFU, 1+18-9; NA, 1+20-19; MFL, 1 + 27-21; NA, 1+30-28; RFU, 1+47-31; NA, 1+49-48; RFM, 1+69-50; NA, 73-70
                                  subtables = c(rep(NA, 1+8-1), 
                                                rep('Modesto Formation, upper member, 10 Ka', 1+18-9), rep(NA, 1+20-19),
                                                rep('Modesto Formation, lower member, 40 Ka', 1 + 27-21), rep(NA, 1+30-28),
                                                rep('Riverbank Formation, upper member, 130 Ka',1+47-31), rep(NA, 1+49-48),
                                                rep('Riverbank Formation, middle member. 250 Ka', 1+69-50), rep(NA, 1+73-70))),
                     part3 = list(page = 44,
                                  columnCuts = c(14, 21, 31, 43, 49, 57, 65, 75, 81, 89, 99, 105, 112),
                                  subtables = c(rep(NA, 1+9-1), 
                                                rep('Riverbank Formation, lower member, 330 Ka', 1+15-10), rep(NA, 1+18-16),
                                                rep('Turlock Lake Formation, 600 Ka', 1+27-19), rep(NA, 1+28-28),
                                                rep('Turlock Lake Formation, 600 Ka', 1+38-29), rep(NA, 1+41-39),
                                                rep('China Hat Gravel member of Laguna Formation, 3,000 Ka', 1+56-42), rep(NA, 1+57-57),
                                                rep('China Hat Gravel member of Laguna Formation, 3,000 Ka', 1+68-58), rep(NA, 1+74-69)))
    ),
    #########################################
    ##### Extractive chemical analyses ######
    ########################################
    chem_extractive = list(title = 'Extractive chemical analyses',
                           analysts ='A. L. Walker (U.S. Geological Survey) with A.J. Busacca, Peter Janitsky, and R. Meixner (University of California, Davis)', 
                           note = 'CEC, cation-exchange capacity; m, with magnetic minerals; w, without magnetic minerals.',
                           columeNames = c('No', 'Sample', 'Horizon', 'Basal depth (cm)', 
                                           'Percentage of <2-mm: Total N', 'Percentage of <2-mm: Organic C', 
                                           'mg/100 g soil: Exchange Na', 'mg/100 g soil: Exchange K', 
                                           'mg/100 g soil: Exchange Ca', 'mg/100 g soil: Exchange Mg', 
                                           'mg/100 g soil: Exchange N', 'mg/100 g soil: CEC', 
                                           'mg/100 g soil: pH 1:1H_2O', 'mg/100 g soil: pH 1:1KCl', 'mg/100 g soil: pH Saturated'),
                           part1 = list(page = 45,
                                        columnCuts = c(7, 15, 24, 31, 43, 51, 65, 79, 93, 105, 117, 127, 137, 147),
                                        subtables = c(rep(NA, 1+11-1), 
                                                      rep(c('Modern River Alluvium', NA), 4), rep(NA, 1+21-20), 
                                                      rep('Post-Modesto Deposits, 0.2 Ka',1 + 27-22), rep(NA, 1+28-28),
                                                      rep('Post-Modesto Deposits, 0.2 Ka',1 + 36-29), rep(NA, 1+39-37), 
                                                      rep('Post-Modesto Deposits, 3 Ka', 1+43-40), rep(NA, 1+44-44), 
                                                      rep('Post-Modesto Deposits, 3 Ka', 1+51-45), rep(NA, 1+52-52), 
                                                      rep('Post-Modesto Deposits, 3 Ka', 1+59-53), rep(NA, 1+60-60),
                                                      rep('Post-Modesto Deposits, 3 Ka', 1+67-61), rep(NA, 1+68-68),
                                                      rep('Post-Modesto Deposits, 3 Ka', 1+73-69), rep(NA, 1+74-74),
                                                      rep('Post-Modesto Deposits, 3 Ka', 1+79-75), rep(NA, 1+85-80))),
                           part2 = list(page = 46,
                                        columnCuts = c(7, 15, 25, 33, 45, 55, 67, 77, 89, 101, 111, 121, 129, 138),
                                        subtables = c(rep(NA, 1+8-1), 
                                                      rep('Modesto Formation, upper member, 10 Ka', 1+13-9), rep(NA, 1+14-14),
                                                      rep('Modesto Formation, upper member, 10 Ka', 1+19-15), rep(NA, 1+22-20),
                                                      rep('Modesto Formation, lower member, 40 Ka', 1 +29-23), rep(NA, 1+32-30),
                                                      rep('Riverbank Formation, upper member, 130 Ka',1+39-33), rep(NA, 1+40-40),
                                                      rep('Riverbank Formation, upper member, 130 Ka',1+50-41), rep(NA, 1+53-51),
                                                      rep('Riverbank Formation, middle member. 250 Ka', 1+75-54), rep(NA, 1+77-76), 
                                                      #oh hey, don't need to skip empty lines
                                                      rep('Riverbank Formation, lower member, 330 Ka',1 + 83-78), rep(NA, 1+89-84))),
                           part3 = list(page = 47,
                                        columnCuts = c(7, 15, 27, 37, 49, 59, 71, 83, 95, 107, 117, 125, 135, 145),
                                        subtables = c(rep(NA, 1+8-1), 
                                                      rep('Turlock Lake Formation, 600 Ka', 1+29-9), rep(NA, 1+32-30),
                                                      rep('China Hat gravel member of Laguna Formation, 3,000 Ka', 1+61-33), rep(NA, 1+67-62)))
      
    ),
    ###########################################
    ##### Extractive chemical analyses 2 ######
    ###########################################
    chem_extractive2 = list(title = 'Extractive chemical analyses [2]',
                           analysts ='A. L. Walker (U.S. Geological Survey) with A.J. Busacca, Peter Janitsky, and R. Meixner (University of California, Davis)', 
                           note = 'CEC, cation-exchange capacity; m, with magnetic minerals; w, without magnetic minerals.',
                           columeNames = c('No', 'Sample', 'Horizon', 'Basal depth (cm)', 
                                           'Percentage of <2-mm: Fe_d(m)', 'Percentage of <2-mm: Fe_d(w)',
                                           'Percentage of <2-mm: Al_d(w)', 'Percentage of <2-mm: mags',
                                           'Percentage of <2-mm: Fe_0', 'Percentage of <2-mm: Al_0'
                                           ),
                           part1 = list(page = 48,
                                        columnCuts = c(33, 41, 51, 61, 71, 83, 93, 101, 111),
                                        subtables = c(rep(NA, 1+14-1), 
                                                      rep('Modern River Alluvium', 1+18-15), rep(NA, 1+20-19), 
                                                      rep('Post-Modesto Deposits, 0.2 Ka',1 + 34-21), rep(NA, 1+36-35),
                                                      rep('Post-Modesto Deposits, 3 Ka', 1+72-37), rep(NA, 1+74-73), 
                                                      rep('Modesto Formation, upper member, 10 Ka', 1+84-75), rep(NA, 1+90-85))),
                           part2 = list(page = 49,
                                        columnCuts = c(5, 15, 25, 35, 45, 57, 68, 77, 87),
                                        subtables = c(rep(NA, 1+8-1), 
                                                      rep('Modesto Formation, lower member, 40 Ka', 1+15-9), rep(NA, 1+18-16),
                                                      rep('Riverbank Formation, upper member, 130 Ka',1+36-19), rep(NA, 1+38-37),
                                                      rep('Riverbank Formation, middle member. 250 Ka', 1+59-39), rep(NA, 1+61-60),
                                                      rep('Riverbank Formation, upper member, 330 Ka',1 + 67-62), rep(NA, 1+69-68),
                                                      rep('Turlock Lake Formation, 600 Ka', 1+88-70), rep(NA, 1+94-89))),
                           part3 = list(page = 50,
                                        columnCuts = c(51, 61, 73, 81, 91, 101, 111, 123, 133),
                                        subtables = c(rep(NA, 1+9-1), 
                                                      rep('China Hat gravel member of Laguna Formation, 3,000 Ka', 1+38-10), rep(NA, 1+80-39)))
    )
                           
  ))
  
  ###Dev work#####
  table_info.ls <- tableInfo.ls$reportA$field_desc
  part_info.ls <- table_info.ls$part1
  
  write_file(reportA_txt[38], file =  'temp/text.txt')
  write_file(reportA_txt[part_info.ls$page], file =  'temp/text.txt')
  
  temp1 <- str_split(reportA_txt[part_info.ls$page], '\n') %>%
    as_tibble(.name_repair = make.names) %>%     # convert to tibble and assign unique column names
    mutate(subtable = part_info.ls$subtables,
           line_number = sprintf('%.3d', 1:nrow(.)),
           pdf_page = part_info.ls$page,
           report = 'USGS Bulletin 1590-A') %>%
    select(report, pdf_page, line_number, subtable, X) %>%
    separate(col = X, sep = part_info.ls$columnCuts, into = table_info.ls$columeNames)
  
  id_cols <- c('No', 'Sample', 'Horizon', 'subtable', 'report', 'pdf_page')
  
  temp2 <- temp1 %>%
    filter(!is.na(subtable)) %>%
    mutate(across(everything(), str_trim)) %>%
    mutate(across(any_of(id_cols), ~na_if(., ''))) %>%
    tidyr::fill(any_of(id_cols), .direction = 'down') %>%
    group_by(across(any_of(id_cols))) %>%
    dplyr::summarise(across(!any_of(id_cols), ~paste0(., collapse = ' ')))
  
  #####example to pull together a table from the parts######
  reportA_tables <- plyr::llply(tableInfo.ls$reportA, function(dataTableMeta.ls){
    #dataTableMeta.ls <- tableInfo.ls$reportA$phys_prop
    sup_data_tables <- plyr::ldply(dataTableMeta.ls[grepl('part', names(dataTableMeta.ls))], 
                                   function(part_info.ls, colNames = dataTableMeta.ls$columeNames){
                                     
                                     id_cols <- c('No', 'Sample', 'Horizon', 'subtable', 'report', 'pdf_page')
                                     
                                     ans <- str_split(reportA_txt[part_info.ls$page], '\n') %>%
                                       as_tibble(.name_repair = make.names) %>%     # convert to tibble and assign unique column names
                                     mutate(subtable = part_info.ls$subtables,
                                              line_number = sprintf('%.3d', 1:nrow(.)),
                                              pdf_page = part_info.ls$page,
                                              report = 'USGS Bulletin 1590-A') %>%
                                       select(report, pdf_page, line_number, subtable, X) %>%
                                       separate(col = X, sep = part_info.ls$columnCuts, into = dataTableMeta.ls$columeNames) %>%
                                       filter(!is.na(subtable)) %>%
                                       mutate(across(everything(), str_trim)) %>%
                                       mutate(across(any_of(id_cols), ~na_if(., ''))) %>%
                                       tidyr::fill(any_of(id_cols), .direction = 'down') %>%
                                       group_by(across(any_of(id_cols))) %>%
                                       dplyr::summarise(across(!any_of(id_cols), ~paste0(., collapse = ' ')), .groups = 'drop')
                                     return(ans)
                                   }, .id = dataTableMeta.ls$title)
    
    #sup_data_tables <- sup_data_tables %>%
    #  arrange(pdf_page, line_number)
    return(sup_data_tables)
  })
  
  read_errors <- list(reportA = list(field_desc =
                                       list( column_recode = list(Sample = c('PHI 5' = 'PM15', 'RIO' = 'R10', 'Til' = 'T11'),
                                                                  `Basal depth (cm)` = c('2DO-230+' = '200-230+')))))
  
  stage01_data <- reportA_tables$field_desc %>%
    mutate(Sample = recode(Sample, !!!read_errors$reportA$field_desc$column_recode$Sample))
  
  checkSampleNames <- plyr::ldply(reportA, function(xx){
    xx %>% group_by(Sample) %>%
      tally()
  }) %>%
    pivot_wider(names_from = '.id', values_from='n') %>%
    filter(is.na(field_desc + phys_prop + chem_extractive + chem_extractive2))
  
  
#}
