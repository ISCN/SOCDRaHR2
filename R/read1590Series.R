read1590Series <- function(dataDir, verbose=FALSE){
  
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
  
  reportA <- pdftools::pdf_text(download_info$download_filename[1])
  
  #Field Tables pgs 38-41 = 4
  #Physical Properties pgs 42-44 = 3
  #Extractive chemical analyses pg 45-47 = 3
  #Extractive chemical analyses 2 pg 48-50 = 3
  #Mineralogy pg 50-53= 4
  #Total Chem analysis pg 53-57 = 5
  #Total Chem analysis - fraction 2 pg 57-61 = 5
  # 27 pages per report for 7 reports at 0.5 hours per page is 95 hours of work
  
  
  #
  
  tableInfo.ls <- list(reportA_STable1_pg1 = list( page = 38,
                                               columnCuts = c(10, 19, 30, 41, 51, 64, 75, 89, 105, 113, 124, 133, 143, 150, 156, 166, 176),
                                               columeNames = c('No', 'Sample', 'Horizon', 'Basal depth (cm)', 'Lower boundary', 
                                                               'Moist color', 'Dry color', 'Texture', 'Structure',   
                                                               'Consistence:Dry', 'Consistence:Moisture', 'Consitence:Wet',
                                                               'Roots', 'Pores', 'Clay films', 'pH', 
                                                               'Assumed Parent Material:Texture', 'Assumed Parent Material:Wet consistence'),
                                               subtables = c(rep(NA, 13), rep('Modern River Alluvium', 4), rep(NA, 4), 
                                                             rep('Post-Modesto Deposits, 0.2 Ka', 7), rep(NA, 2), rep('Post-Modesto Deposits, 0.2 Ka', 10), rep(NA, 4), 
                                                             rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 2), 
                                                             rep('Post-Modesto Deposits, 3 Ka', 11), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 9), rep(NA, 2), 
                                                             rep('Post-Modesto Deposits, 3 Ka', 10), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 2), 
                                                             rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 6))),
                       reportA_STable1_pg2 = list(page = 39,
                                                  columnCuts = c(7, 16, 26, 38, 48, 62, 75, 86, 102, 113, 124, 133, 143, 155, 165, 173, 184),
                                                  columeNames = c('No', 'Sample', 'Horizon', 'Basal depth (cm)', 'Lower boundary', 
                                                                  'Moist color', 'Dry color', 'Texture', 'Structure',   
                                                                  'Consistence:Dry', 'Consistence:Moisture', 'Consitence:Wet',
                                                                  'Roots', 'Pores', 'Clay films', 'pH', 
                                                                  'Assumed Parent Material:Texture', 'Assumed Parent Material:Wet consistence'),
                                                  subtables = c(rep(NA, 11), rep('Modesto Formation, upper member, 10 Ka', 19), rep(NA, 5),
                                                                rep('Modesto Formation, upper member, 10 Ka', 11), rep(NA, 4),
                                                                rep('Riverbank Formation, upper member, 130 Ka', 12), rep(NA, 2),
                                                                rep('Riverbank Formation, upper member, 130 Ka', 20), rep(NA, 4),
                                                                rep('Riverbank Formation, middle member. 250 Ka', 8), rep(NA, 6))))
  
  page_info.ls <- tableInfo.ls$reportA_STable1_pg2
  
  write_file(reportA[page_info.ls$page], file =  'temp/text.txt')
  
  temp <- str_split(reportA[page_info.ls$page], '\n') %>%
    as_tibble(.name_repair = make.names) %>%     # convert to tibble and assign unique column names
    mutate(Subtable = page_info.ls$subtables) %>%
    select(Subtable, X) %>%
    separate(col = X, sep = page_info.ls$columnCuts, into = page_info.ls$columeNames) %>%
    filter(!is.na(Subtable)) %>%
    mutate(across(-Subtable, ~gsub('^\\s+', '', .))) %>%
    mutate(across(-Subtable, ~gsub('\\s+$', '', .))) %>%
    mutate(across(c('No', 'Sample'), ~na_if(., ''))) %>%
    tidyr::fill(No, Sample, .direction = 'down') %>%
    group_by(Subtable, No, Sample) %>%
    summarize(across(everything(), ~paste0(., collapse = ' ')), .groups = 'drop') %>%
    mutate(across(-Subtable, ~gsub('\\s+$', '', .))) %>%
    mutate(across(No, as.numeric) ) %>%
    arrange(No)
  
}
