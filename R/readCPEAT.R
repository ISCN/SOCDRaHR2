#' CPEAT project reads
#' 
#' This reads in the specified records of the CPEAT project. Currently under development, not all
#' the metadata is parsed
#'
#' @param dataDir identify the download directory 
#'
#' @return a list of data frames, the first data frame with the meta data and
#'  a second data frame with the records
#' @export
#'
#' @examples
readCPEAT <- function(dataDir){
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
    dplyr::mutate(downloadURL = glue::trim(paste(gsub('doi.org', 'doi.pangaea.de', URL),
                                      '?format=textfile', sep='')),
      localFile = file.path(dataDir, paste0(Site_core, '.tab'))) 
  
  download.file(downloadDOI$downloadURL, downloadDOI$localFile)
  
  allData <- plyr::ddply(downloadDOI, c('Site_core'), function(xx){
    #print(xx$localFile)
    readText <- read_file(xx$localFile)
    #header <- regmatches(readText, regexpr('/\\* .*\n\\*/', readText))
    return(read_tsv(gsub('/\\* .*\n\\*/\n', '', readText)))
  })
  
  allheader <- plyr::ddply(downloadDOI, c('Site_core'), function(xx){
    #print(xx$localFile)
    readText <- read_file(xx$localFile)
    header <- regmatches(readText, regexpr('/\\* .*\n\\*/', readText))
    ans <- unlist(strsplit(x=header, split='\n.+:\t', perl=TRUE))
    ans <- ans[-1] #pop off the header
    names(ans) <- gsub('\\n|\\t|:', '', 
                               unlist(regmatches(header, gregexpr('\n.+:\t', header, per=TRUE))))
    return(as.data.frame(as.list(ans)))
  })
  
  metaData <- allheader %>% 
    dplyr::mutate(Size = as.numeric(gsub(' data points\n\\*/', '', Size))) %>%
    dplyr::mutate_at(vars(License), as.factor) %>%
    tidyr::separate(Coverage, c('lat_lab', 'lat', 'lon_lab', 'lon', 
                                'min_depth_lab', 'min_depth', 'max_depth_lab', 'max_depth'),
                    sep='(: )|( \\* )|(\n\t)') %>%
    #dplyr::select(-lat_lab, -lon_lab, -min_depth_lab, -max_depth_lab) %>%
    dplyr::mutate(min_depth = gsub(' m$', '', min_depth),
                  max_depth = gsub(' m$', '', max_depth)) %>%
    dplyr::mutate_at(vars(min_depth, max_depth, lat, lon), as.numeric) %>%
    dplyr::group_by_all()
  #TODO this needs to be parsed
  # %>%
  #   do((function(xx){
  #     #print(xx$Event.s.)
  #     xxtemp <- gsub('COMMENT:','',  as.character(xx$Event.s.))
  #     #xxtemp <- gsub('Details.+:', '', xxtemp, perl = TRUE)
  #     xxtemp <- strsplit(unlist(strsplit(as.character(xxtemp), '( \\* )|(; )|( : )|(, )')), ': ')
  #     xxtemp[[1]] <- c('name', xxtemp[[1]][1])
  #     xxtemp <- lapply(xxtemp, function(yy){
  #       if(length(yy) > 2){return(yy[length(yy)-1:0])} else {return(yy)}})
  #     
  #     xxtemp2 <- t(as.data.frame(xxtemp))
  #     xxtemp3 <- data.frame(as.list(setNames(xxtemp2[,2], xxtemp2[,1])))
  #     print(xxtemp3)
  #     return(xxtemp3)
  #   })(.))
  
  return(site=metaData, sample=allData)
  #tester <- pangaear::pg_data('10.1594/PANGAEA.890471')
}