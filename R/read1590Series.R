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
  
  #Field Tables pgs 38-41
  #Physical Properties pgs 42-44
  #Extractive chemical analyses pg 45-47
  #Extractive chemical analyses 2 pg 48-50
  #Mineralogy pg 50-53
  #Total Chem analysis pg 53-57
  #Total Chem analysis - fraction 2 pg 57-61
  
  
  #write_file(reportA[38], file =  'temp/text.txt')
  
  columnCuts <- c(10, 19, 30, 41, 51, 64, 75, 89, 105, 113, 124, 133, 143, 150, 156, 166, 176)
  columeNames <- c('No', 'Sample', 'Horizon', 'Basal depth (cm)', 'Lower boundary', 
                   'Moist color', 'Dry color', 'Texture', 'Structure',   
                   'Consistence:Dry', 'Consistence:Moisture', 'Consitence:Wet',
                   'Roots', 'Pores', 'Clay films', 'pH', 
                   'Assumed Parent Material:Texture', 'Assumed Parent Material:Wet consistence')
  subtables <- c(rep(NA, 13), rep('Modern River Alluvium', 4), rep(NA, 4), 
                 rep('Post-Modesto Deposits, 0.2 Ka', 7), rep(NA, 2), rep('Post-Modesto Deposits, 0.2 Ka', 10), rep(NA, 4), 
                 rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 11), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 9), rep(NA, 2), 
                 rep('Post-Modesto Deposits, 3 Ka', 10), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 2), rep('Post-Modesto Deposits, 3 Ka', 5), rep(NA, 6))
  
  ReportA_SupTable1 <- str_split(reportA[38], '\n') %>%
    # convert to tibble and assign unique column names
    as_tibble(.name_repair = make.names) %>%
    mutate(Subtable = subtables) %>%
    select(Subtable, X) %>%
    separate(col = X, sep = columnCuts, into = columeNames) %>%
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