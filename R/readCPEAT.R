#' CPEAT project reads
#' 
#' This reads in records of the CPEAT project from PANGAEA. 
#'
#' @param dataDir identify the download directory
#' @param workLocal flag to ignore any files you don't have locally (don't download)
#' @param verbose flag to print out verbose debugging messages 
#'
#' @return a list of data frames, the first data frame with the meta data and
#'  a second data frame with the records
#'  @import magrittr
#'  @importFrom dplyr select mutate vars starts_with select if_else recode filter full_join
#'  @importFrom readr read_file read_tsv
#'  @importFrom plyr ddply
#'  @importFrom tidyr separate gather spread
#' @export
#'
readCPEAT <- function(dataDir, workLocal = FALSE, verbose=FALSE){
  downloadDOI <- read.csv(text=gsub(' ', '', gsub(' core ', '_c', 'URL,Author,Site_core,orgFile,extra
https://doi.org/10.1594/PANGAEA.890471,Garneau,Aero,Aero.csv,
https://doi.org/10.1594/PANGAEA.890528,Yu,Altay core 1,Altay.csv,
https://doi.org/10.1594/PANGAEA.890198,Nichols,Bear core 1,Bear.csv,
https://doi.org/10.1594/PANGAEA.890472,Charman,Burnt Village core 1,Burnt_Village.csv,
https://doi.org/10.1594/PANGAEA.890473,Lavoie,Covey Hill,Covey_Hill. csv,
https://doi.org/10.1594/PANGAEA.890474,MacDonald,D127 core 1,D127.csv,
https://doi.org/10.1594/PANGAEA.890475,MacDonald,E110 core 1,E110.csv,
https://doi.org/10.1594/PANGAEA.890345,Sannel,Ennadai core 1,Ennadai.csv,
https://doi.org/10.1594/PANGAEA.890529,Anderson,Glen Carron core 1,Glen_Carron.csv,
https://doi.org/10.1594/PANGAEA.890530,Anderson,Glen Torridon core 1,Glen_Torridon.csv,
https://doi.org/10.1594/PANGAEA.890346,Yu,Goldeye fen,Goldeye.csv,
https://doi.org/10.1594/PANGAEA.890397,Packalen,HL02,HL02.csv,
https://doi.org/10.1594/PANGAEA.890531,Large,Hongyuan core HYLK1,Hongyuan.csv,
https://doi.org/10.1594/PANGAEA.890199,Jones,Horse Trail core 1,Horse_Trail.csv,
https://doi.org/10.1594/PANGAEA.890398,Holmquist,JBL1,JBL1.csv,
https://doi.org/10.1594/PANGAEA.890399,Holmquist,JBL2,JBL2.csv,
https://doi.org/10.1594/PANGAEA.890400,Holmquist,JBL3,JBL3.csv,
https://doi.org/10.1594/PANGAEA.890401,Holmquist,JBL4,JBL4.csv,
https://doi.org/10.1594/PANGAEA.890402,Holmquist,JBL5,JBL5.csv,
https://doi.org/10.1594/PANGAEA.890403,Holmquist,JBL7,JBL7.csv,
https://doi.org/10.1594/PANGAEA.890404,Holmquist,JBL8,JBL8.csv,
https://doi.org/10.1594/PANGAEA.890405,Camill,Joey core 12,Joey.csv,
https://doi.org/10.1594/PANGAEA.890406,Camill,Joey core 15,Joey.csv,
https://doi.org/10.1594/PANGAEA.890407,Camill,Joey core 17,Joey.csv,
https://doi.org/10.1594/PANGAEA.890408,Camill,Joey core 2,Joey.csv,
https://doi.org/10.1594/PANGAEA.890409,Camill,Joey core 5,Joey.csv,
https://doi.org/10.1594/PANGAEA.890410,Camill,Joey core 7,Joey.csv,
https://doi.org/10.1594/PANGAEA.890532,Loisel,KAM core C1,KAM12-C1.csv,
https://doi.org/10.1594/PANGAEA.890533,Bochicchio,KAM core C4,KAM12-C4.csv,
https://doi.org/10.1594/PANGAEA.890200,Yu,Kenai Gasfield,Kenai_Gasfield.csv,
https://doi.org/10.1594/PANGAEA.890411,Packalen,KJ2-3,KJ2-3.csv,
https://doi.org/10.1594/PANGAEA.890412,Lamarre,KUJU,KUJU.csv,
https://doi.org/10.1594/PANGAEA.890527,Borren,Kvartal core Zh0,86-Kvartal.csv,
https://doi.org/10.1594/PANGAEA.890413,Garneau,La Grande core L2T2C2-2,La_Grande2.csv,
https://doi.org/10.1594/PANGAEA.890414,Garneau,La Grande core L3T1C2,La_Grande3.csv,
https://doi.org/10.1594/PANGAEA.890476,van Bellen,Lac Le Caron ,Lac_Le_Caron.csv,
https://doi.org/10.1594/PANGAEA.890415,Camill,Lake 396 core 3,Lake396.csv,
https://doi.org/10.1594/PANGAEA.890416,Camill,Lake 785 core 4,Lake785.csv,
https://doi.org/10.1594/PANGAEA.890477,Magnan,Lebel core 1,Lebel.csv,
https://doi.org/10.1594/PANGAEA.890478,Mathijssen,Lompolojankka core 1,Lompolojankka.csv,
https://doi.org/10.1594/PANGAEA.890347,Yu,Mariana core 1,Mariana.csv,
https://doi.org/10.1594/PANGAEA.890348,Yu,Mariana core 2,Mariana.csv,
https://doi.org/10.1594/PANGAEA.890349,Yu,Mariana core 3,Mariana.csv,
https://doi.org/10.1594/PANGAEA.890350,Robinson,Martin core 1,Martin.csv,
https://doi.org/10.1594/PANGAEA.890479,van Bellen,Mosaik core Central,Mosaik.csv,
https://doi.org/10.1594/PANGAEA.890201,Yu,No Name Creek,No_Name_Creek.csv,
https://doi.org/10.1594/PANGAEA.890186,Yu,Nuikluk core 10-1,Nuikluk.csv,
https://doi.org/10.1594/PANGAEA.890202,Yu,Nuikluk core 10-2,Nuikluk.csv,
https://doi.org/10.1594/PANGAEA.890203,Tarnocai,NW-BG core 10,NW-BG.csv,
https://doi.org/10.1594/PANGAEA.890204,Tarnocai,NW-BG core 2,NW-BG.csv,
https://doi.org/10.1594/PANGAEA.890205,Tarnocai,NW-BG core 3,NW-BG.csv,
https://doi.org/10.1594/PANGAEA.890206,Tarnocai,NW-BG core 8,NW-BG.csv,
https://doi.org/10.1594/PANGAEA.890480,Garneau,Ours core 1,Ours.csv,
https://doi.org/10.1594/PANGAEA.890481,Garneau,Ours core 4,Ours.csv,
https://doi.org/10.1594/PANGAEA.890482,Garneau,Ours core 5,Ours.csv,
https://doi.org/10.1594/PANGAEA.890351,Yu,Patuanak core 1,Patuanak.csv,
https://doi.org/10.1594/PANGAEA.890208,Loisel,Petersville,Petersville.csv,
https://doi.org/10.1594/PANGAEA.890534,Charman,Petite Bog core 1,Petite_Bog.csv,
https://doi.org/10.1594/PANGAEA.890483,Magnan,Plaine core 1,Plaine.csv,
https://doi.org/10.1594/PANGAEA.890484,Oksanen,Rogovaya core 2,Rogovaya.csv,
https://doi.org/10.1594/PANGAEA.890485,Oksanen,Rogovaya core 3,Rogovaya.csv,
https://doi.org/10.1594/PANGAEA.890486,Makila,Saarisuo core B800,Saarisuo.csv,
https://doi.org/10.1594/PANGAEA.890352,Sannel,Selwyn Lake,Selwyn.csv,
https://doi.org/10.1594/PANGAEA.890417,Camill,Shuttle core 2,Shuttle.csv,
https://doi.org/10.1594/PANGAEA.890535,MacDonald,SIB06 core 1,SIB06.csv,
https://doi.org/10.1594/PANGAEA.890487,Charman,Sidney core 1,Sidney.csv,
https://doi.org/10.1594/PANGAEA.890488,Mathijssen,Siikavena core 1,Siikavena.csv,
https://doi.org/10.1594/PANGAEA.890353,Kuhry,Slave Lake ,Slave.csv,
https://doi.org/10.1594/PANGAEA.890489,van Bellen,Sterne,Sterne.csv,
https://doi.org/10.1594/PANGAEA.890490,Kokfelt,Stordalen,Stordalen.csv,
https://doi.org/10.1594/PANGAEA.890354,Yu,Sundance core 2,Sundance.csv,
https://doi.org/10.1594/PANGAEA.890355,Yu,Sundance core 3,Sundance.csv,
https://doi.org/10.1594/PANGAEA.889936,Jones,Swanson Fen,Swanson.csv,
https://doi.org/10.1594/PANGAEA.890356,Tarnocai,T1 core 1,T1.csv,
https://doi.org/10.1594/PANGAEA.890418,Camill,Unit core 4,Unit.csv,
https://doi.org/10.1594/PANGAEA.890357,Yu,Upper Pinto,Upper_Pinto.csv,
https://doi.org/10.1594/PANGAEA.890536,Oksanen,Usinsk core USI1,Usinsk.csv,
https://doi.org/10.1594/PANGAEA.890358,Yu,Utikuma core 1,Utikuma.csv,
https://doi.org/10.1594/PANGAEA.890537,MacDonald,V34 core 1,V34.csv,
https://doi.org/10.1594/PANGAEA.890538,Borren,Vasyugan core V21,Vasyugan.csv,
https://doi.org/10.1594/PANGAEA.890539,Bunbury,VC04-06 core 1,VC04-06.csv,
https://doi.org/10.1594/PANGAEA.890540,Zhao,Zoige core 1,Zoige.csv,')),
                          stringsAsFactors = FALSE)  %>%
    dplyr::select(-extra) %>%
    dplyr::mutate(downloadURL = gsub('\\s+$', '', gsub('^\\s+', '', 
                                                       paste(gsub('doi.org', 'doi.pangaea.de', URL),
                                                             '?format=textfile', sep=''))),
                  ##prototype provided as csv files, actually downloading as tsv
                  localFile = file.path(dataDir, paste0(Site_core, '.tab'))) 
  
  ##Download the specified files above if you don't have them
  if(any(!file.exists(downloadDOI$localFile)) & !workLocal){
    if(verbose) print(paste('downloading:', 
                            downloadDOI$localFile[!file.exists(downloadDOI$localFile)]))
    download.file(downloadDOI$downloadURL[!file.exists(downloadDOI$localFile)], 
                  downloadDOI$localFile[!file.exists(downloadDOI$localFile)])
  }
  
  ##Trim out any files that failed to download
  if(workLocal){
    downloadDOI <- downloadDOI %>% 
      filter(file.exists(localFile))
  }
  
  ##Grab the tabiluar data from all the files
  allData <- plyr::ddply(downloadDOI, c('Site_core'), function(xx){
    if(verbose) {paste('processing data table:', xx$localFile)}
    readText <- readr::read_file(xx$localFile)
    #header <- regmatches(readText, regexpr('/\\* .*\n\\*/', readText))
    ##Trim the header between /* and \*
    return(readr::read_tsv(gsub('/\\* .*\n\\*/\n', '', readText)))
  })
  
  ##Grab the header text
  allheader <- plyr::ddply(downloadDOI, c('Site_core'), function(xx){
    if(verbose) print(paste('processing data table:', xx$localFile))
    readText <- readr::read_file(xx$localFile)
    header <- regmatches(readText, regexpr('/\\* .*\n\\*/', readText))
    ans <- unlist(strsplit(x=header, split='\n.+:\t', perl=TRUE))
    ans <- ans[-1] #pop off the header
    names(ans) <- gsub('\\n|\\t|:', '', 
                               unlist(regmatches(header, gregexpr('\n.+:\t', header, per=TRUE))))
    return(as.data.frame(as.list(ans)))
  })
  
  
  ##I really hate this bit of code. It feels like there should be 
  ##...an elegant parsing solution but I can't find it in time alotted. Very sad.
  #This pangaear solution was never quite there. but leaving it here for future tinkering
  #devtools::install_github("ropensci/pangaear@cache-path", force=TRUE)
  #tester <- pangaear::pg_data('10.1594/PANGAEA.890471')
  #tester[[1]]$metadata$events
  #record_test <- pangaear::pg_get_record('oai:pangaea.de:doi:10.1594/PANGAEA.890471')
  metaData <- allheader %>% 
    dplyr::mutate(Size = as.numeric(gsub(' data points\n\\*/', '', Size))) %>%
    dplyr::mutate_at(dplyr::vars(License), as.factor) %>%
    tidyr::separate(Coverage, into=paste('section', 1:4),
                    sep='(\\s+\\*\\s+)|\t') %>%
    tidyr::gather(key='key', value='parseME', dplyr::starts_with('section')) %>%
    dplyr::select(-key) %>%
    tidyr::separate(parseME, into=c('header', 'value'), sep=':') %>%
    dplyr::mutate(header = dplyr::if_else(grepl(' m$', value), paste(header, '[m]'), header),
                  value = gsub(' m$', '', value)) %>%
    ##trim spaces
    dplyr::mutate(header=gsub('\\s+$', '', gsub('^\\s+', '', header)),
                  value=gsub('\\s+$', '', gsub('^\\s+', '', value))) %>%
    tidyr::spread(key='header', value='value') %>%
    tidyr::separate(col=Event.s., into=c('Space', 'StudyComments'), sep=' \\* COMMENT: ')
  
  space_DF <- metaData %>% dplyr::select(Space) %>% unique %>%
    tidyr::separate(col=Space, into=c('core_name', paste0('parse_string', 1:6)), sep='\\*', remove=FALSE) %>%
    tidyr::gather(key='segment', value='parseME', dplyr::starts_with('parse_string'), na.rm=TRUE) %>%
    dplyr::select(-segment) %>%
    tidyr::separate(parseME, into=c('header', 'value'), sep=': ') %>%
    dplyr::mutate(header=dplyr::recode(header, ' Penetration' = " Recovery")) %>%
    dplyr::mutate(header = gsub('\\s$', '', gsub('^\\s+', '', header)),
                  value = gsub('\\s$', '', gsub('^\\s+', '', value))) %>%
    dplyr::mutate(header = dplyr::if_else(grepl('cm$', value),
                                          paste(header,'[cm]'), 
                                          dplyr::if_else(grepl(' m$', value), paste(header, '[m]'), header)),
                  value = gsub(' c?m$', '', value)) %>%
    tidyr::spread(key='header', value='value') 
  
  comments_DF <- metaData %>% dplyr::select(StudyComments) %>% unique %>%
    dplyr::mutate(parse_String = dplyr::if_else(grepl('^[^;]+:[^;]+:', StudyComments),
                                                gsub('^[^:]+: ?', '', StudyComments), StudyComments)) %>%
    tidyr::separate(parse_String, into=paste0('x', 1:11), sep='(;)|(.,) ') %>%
    tidyr::gather(key='position', value='parse_string', dplyr::starts_with('x'), na.rm=TRUE) %>% 
    dplyr::select(-position) %>%
    dplyr::mutate(parse_string = dplyr::recode(parse_string, 
                                               `carbon rate site Y3` = 'carbon rate site: Y3',
                                               ` basal age 14065 calBP` ='basal age: 14065 calBP')) %>%
    tidyr::separate(parse_string, into=c('label', 'value'), sep=': ', remove=FALSE) %>%
    dplyr::mutate(value=dplyr::if_else(grepl('uncal', value), paste0(gsub('uncal.*$', '',value), 'uncal'), value)) %>%
    ##Deal with units
    dplyr::mutate(label=tolower(gsub('^\\s+', '',
                                     dplyr::if_else(grepl('\\d+ \\w+\\.?$', value), 
                                       sprintf('%s [%s]', label, gsub('^(\\d|\\.)+ ', '', value)), label)) ))%>%
    dplyr::mutate(value=dplyr::if_else(grepl('\\d+ \\w+\\.?$', value), 
                                     gsub(' \\D+$', '', value), value)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(-parse_string) %>%
    tidyr::spread(key='label', value='value') %>%
    dplyr::mutate(`peat properties sample size` = gsub(' ?cm.*', '', `peat properties sample size`)) %>%
    dplyr::rename('peat properties sample size [cm^3]' = `peat properties sample size`)
  
  temp <- metaData %>% 
    dplyr::full_join(space_DF) %>% 
    dplyr::full_join(comments_DF) %>%
    dplyr::select(-Space, -StudyComments)
  
  return(list(site=temp, sample=allData, files=downloadDOI))
  

}
