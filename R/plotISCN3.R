#' Plot International Soil Carbon Network 3
#'
#'This function first takes the reformatted data output of the International Soil Carbon Network 3_1 function. It accounts for whether or not a dataset has profile-level data, as well as the locations of its sites for mapping purposes. It generates tables, maps, and histograms using this cleaned data. It then returns these visualizations.
#'
#'
#' @param ISCN3
#' @param datasetName 
#' 
#'
#' @return
#' @export
#' @importFrom dplyr filter group_by mutate_all select pull select_if
#' @importFrom knitr kable
#' @importFrom ggplot2 coord_cartesian map_data geom_polygon coord_fixed xlab ylab labs aes geom_point ggplot facet_wrap geom_line theme_set
#' @importFrom tidyr pivot_longer
#'

plotGenerate <- function(ISCN3, datasetName) {
  
  # TODO specify library that functions are imported from
  # TODO list functions used in @importFrom
  # TODO double check EVERY dataset runs properly
  
  #library(tidyverse)
  #source('R/ISCN3_1.R')
  #ISCN3 <- ISCN3_1('~/Documents/Datasets/ISCN')
  #datasetName <- 'UMBS_FASET'
  
  #################################
  #### Variable Initialization ####
  #################################
  
  # filtering ISCN3 for the specific dataset's information
  datasetStudy <- ISCN3$study %>%
    dplyr::filter(dataset_name == datasetName)
  datasetProfile <- ISCN3$profile %>%
    dplyr::filter(dataset_name_sub == datasetName)
  datasetLayer <- ISCN3$layer %>%
    dplyr::filter(dataset_name_sub == datasetName)
  
  # setting extra blanks in knitr tables to print as a space
  options(knitr.kable.NA = '')
  
  # if no profile data, printing this message in place of profile table
  profileTable <- paste('[',
                      datasetName,  # printing specific dataset's name
                      '] does not contain profile data.', sep = '')
  # setting as NA in case the dataset does not have profile data
  profileMap <- NA
  histograms <- NA
  
  # removing NA values for later plotting
  datasetProfileRemNA <- datasetProfile %>%
    dplyr::select(where(function(xx) {!all(is.na(xx))}))
  datasetLayerRemNA <- datasetLayer %>%
    dplyr::select(where(function(xx) {!all(is.na(xx))}))


  ########################
  #### Summary Tables ####
  ########################

  studyTable <- knitr::kable(t(datasetStudy %>%
                               dplyr::select(where(function(xx) {!all(is.na(xx))}))),
                           col.names = '',
                           caption = paste('A summary of [', datasetName,
                                           '] study and contact information.',
                                           sep = ''))

  # checking for existence of profile data before creating table
  if(datasetName %in% ISCN3$profile$dataset_name_sub) {
    profileTable <- knitr::kable(summary(datasetProfileRemNA %>%
                                         dplyr::select_if(is.factor) %>%
                                         dplyr::mutate_all(droplevels)),
                               caption = paste('A summary of [', datasetName,
                                               '] profile data,  \nwhich is 2D resolved information.',
                                               sep = ''))
  } else {} # keeping profileTable as NA
  
  layerTable <- knitr::kable(summary(datasetLayerRemNA %>%
                                       dyplr::select_if(is.factor) %>%
                                       dplyr::mutate_all(droplevels)),
                           caption = paste('A summary of [', datasetName,
                                           '] layer data,  \ncontaining 3D resolved information.',
                                           sep = ''))


  ###########################
  #### Mapping Variables ####
  ###########################

  # can shift y-coordinate 360 degrees into the correct orientation if needed
  shift360 <- 0   # usually only needed for ggplot's 'world2' map
  # hard-coding borders to Alaska maps
  alaskaCoords <- list(ggplot2::coord_cartesian(xlim = c(184.6,231.4),
                                       ylim = c(53,71)))
  
  # dataset's country(s) to later see if a world map is needed
  datasetCountry <- unique(datasetLayer %>% dplyr::select(`country (country)`))
  # dataset's state (to use in state-specific mapping)
  datasetState <- unique(datasetLayer$`state (state_province)`)
  # formatting dataset's state to match that in ggplot map data
  mapStateData <- ggplot2::map_data('state') %>% 
    dplyr::filter(region %in% tolower(datasetState))


  # generic mapping code to build off of or modify with other ggplot arguments
  ggMapBase <- list(ggplot2::geom_polygon(ggplot2::aes(x=long, y = lat, group = group), 
                               fill = 'grey', color = 'black'),
                  ggplot2::coord_fixed(1.3),   # scaling map proportionally
                  ggplot2::xlab('Longitude'), ggplot2::ylab('Latitude'))

  # coloring all of the US light blue
  ggUSAFillBlue <- ggplot2::geom_polygon(data = ggplot2::map_data('world2', 'usa'),
                              ggplot2::aes(x = long, y = lat, group = group),
                              fill = 'lightblue', color = 'black')

  # only coloring the dataset's state blue
  ggStateFillBlue <- ggplot2::geom_polygon(data = mapStateData,
                                ggplot2::aes(x=long, y = lat, group = group),
                                fill = 'light blue', color = 'black')

  # creating variables to easily call the correct title and captions for maps
  mapTitleProfile <- ggplot2::labs(title = 'Profile Data Map',
                        caption = paste('A map of [', datasetName,
                                        '] sampling sites,  \nusing coordinates found within the profile data.',
                                        sep = ''))
  mapTitleLayer <- ggplot2::labs(title = 'Layer Data Map',
                      caption = paste('A map of [', datasetName,
                                      '] sampling sites,  \nusing coordinates found within the layer data.',
                                      sep = ''))

  # Variables for auto-scaling map
  # creating easier reference for latitudes
  baseLat <- datasetProfile %>%
                dplyr::pull('lat (dec. deg)')
  # doing same for longitudes - will be used in creating map boundaries
  baseLong <- datasetProfile %>%
                dplyr::pull('long (dec. deg)') 

  # checking if range of longitudes is <= 10 (as they showed more variation), 
  if(diff(range(baseLong, na.rm = TRUE)) <= 10) {
        # multiplying by a larger number to make map more zoomed in
        rangeLat <- diff(range(baseLong, na.rm = TRUE)) * 12
  } else {
        # since range of coordinates is larger, does not to be as zoomed in
        rangeLat <- diff(range(baseLong, na.rm = TRUE)) * 8
  }

  # scaling the map so that longitude:latitude ratio creates a proportional map
  calcLat <- rangeLat * 2.6
  
  # creating new map limits based on the range of coordinates
  adjustedCoords <- ggplot2::coord_cartesian(  # shifting 360 degrees for 'world2' map
                    xlim = c(mean(baseLong, na.rm = TRUE) + 360 - calcLat,
                             mean(baseLong, na.rm = TRUE) + 360 + calcLat),
                    ylim = c(mean(baseLat, na.rm = TRUE) - rangeLat,
                             mean(baseLat, na.rm = TRUE) + rangeLat))



  #####################
  #### Profile Map ####
  #####################

  # initializing profile-specific map variables
  datasetInfo <- datasetProfile   # setting profile data to be used for mapping
  
  # variable that will put a red X for each site on profile map
  ggRedX <- ggplot2::geom_point(data= datasetInfo, 
                     ggplot2::aes(x = `long (dec. deg)` +
                           shift360,   # shifting 0 unless specified to 360
                         y = `lat (dec. deg)`),
                     shape = 'x', color = 'red', size = 3)

  # PROFILE MAP GENERATION
  # checking for existence of profile data
  if(datasetName %in% ISCN3$profile$dataset_name_sub) {
    # checking if dataset has data in more than one country
    if(nrow(unique(datasetProfile %>%  dplyr::select(`country (country)`))) > 1 ) {
      # using world map to show multiple countries
      profileMap <- ggplot(data =  map_data('world')) +
        ggMapBase + ggRedX + 
        mapTitleProfile
    } else if(datasetCountry ==  'United States') {
      # checking if in Alaska, since is at least in US
      if(datasetState == 'Alaska') {
        # shifting coordinates 360 degrees to correct orientation for 'world2'
        shift360 <- 360
        
        profileMap <- ggplot(data =  map_data('world2')) +
          ggMapBase + ggUSAFillBlue +  # highlights US in light blue 
          ggRedX + alaskaCoords +  # using hard-coded Alaska map boundaries
          mapTitleProfile
      } else {
        # mapping for any other US state
        profileMap <- ggplot(data =  map_data('state')) + # continental US map
          ggMapBase + ggStateFillBlue +  # colors state of interest with blue
          ggRedX + mapTitleProfile  
      }
    } else { # use auto-scaling map code (if not in US)
      shift360 <-  360   # shifting longitude variables 360 degrees
      
      profileMap <- ggplot(data =  map_data('world2')) +
        ggMapBase + ggRedX +
        adjustedCoords + # map coordinate limits based on longitude range
        mapTitleProfile 
    }
  } else if(datasetCountry ==  'United States' & datasetState != 'Alaska') {
    # highlighting state that layer data is located when no profile data
    profileMap <- ggplot(data =  map_data('state')) +
      ggMapBase + ggStateFillBlue +
      ggplot2::labs(title = 'Highlighted Layer Data Map',
           caption = paste('A U.S. map of where [', datasetName,
                           '] sampling occured,  \nusing layer information
                        since this dataset does not have profile information.',
                           sep = ''))
  } else {}   # keeping profileMap as NA


  ###################
  #### Layer Map ####
  ###################

  # initializing profile-specific map variables
  datasetInfo <- datasetLayerRemNA %>%    # now setting layer data for mapping
    dplyr::select(`long (dec. deg)`, `lat (dec. deg)`) %>%
    unique() # removing duplicate sites
  
  # updating red X code with layer data
  ggRedX <- list(ggplot2::geom_point(data= datasetInfo, 
                            ggplot2::aes(x = `long (dec. deg)` + shift360,
                                y = `lat (dec. deg)`),
                            shape = 'x', color = 'red', size = 3))
  
  # LAYER MAP GENERATION
  # checking if layer data is in more than one country
  if(nrow(datasetCountry) > 1 ) {
    # using world map to show multiple countries
    layerMap <-ggplot2::ggplot(data =  ggplot2::map_data('world')) +
      ggMapBase + ggRedX +
      mapTitleLayer    
  } else if(datasetCountry ==  'United States' &  # testing for continental US
            na.omit(datasetState) != 'Alaska') {
    # continental US mapping 
    layerMap <- ggplot2::ggplot(data =  mapStateData) +  # mapping with dataset's state
      ggMapBase + ggStateFillBlue +
      ggRedX + mapTitleLayer
  } else {
    # shifting coordinate system because of shift in 'world2' map
    shift360 <- 360     
    
    layerMap <- ggplot2::ggplot(data =  ggplot2::map_data('world2')) +
      ggMapBase + ggUSAFillBlue + ggRedX +  # highlighting US
      alaskaCoords +   # Alaska-specific map boundaries
      mapTitleLayer
  }


  ############################
  #### Profile Histograms ####
  ############################
  
  # checking if dataset has profile data
  if(datasetName %in% ISCN3$profile$dataset_name_sub) {
    histograms <-  ggplot2::ggplot(datasetProfileRemNA %>%
                            # plotting columns shared by num_cols and dataset's profile data
                            tidyr::pivot_longer(cols =  intersect(names(.),
                                                           ISCN3$type_columns$num_cols),
                                         # setting axes
                                         values_to = 'measurement', 
                                         names_to = 'type')) +
      ggplot2::geom_histogram(ggplot2::aes(x=measurement)) + # creating histogram
      ggplot2::facet_wrap(~type, scales='free') + # automatically scaling
      ggplot2::labs(title = 'Profile Value Histograms',
           caption = paste('Histograms detailing measurements in [',
                           datasetName, '] profile data.',
                           sep = ''))
  } else {}

  ###########################
  #### Layer Depth Plots ####
  ###########################
  
  # creating a line graph to show different measurements by depth
  depthPlots <- ggplot2::ggplot(datasetLayerRemNA %>% 
                         # setting which columns to make x-axis
                         tidyr::pivot_longer(cols=c('layer_top (cm)',
                                             'layer_bot (cm)'),
                                      values_to='depth') %>%
                         # setting which measurements to include for each chart/y-axis
                         tidyr::pivot_longer(cols = intersect(names(.),
                                                       ISCN3$type_columns$num_cols), 
                                      values_to = 'measurement',
                                      names_to = 'type')) +
    # adding line to connect data points
    ggplot2::geom_line(ggplot2::aes(x=depth, y= measurement, group = profile_name),
              alpha = 0.5) +
    ggplot2::facet_wrap(~type, scales='free') + # automatic scaling
    ggplot2::labs(title = 'Layer Data Depth Plots',
         caption = paste('Plots of [', datasetName,
                         '] layer data by depth.', sep = ''))
  
  
  ggplot2::theme_set(theme_grey())  # setting global ggplot theme



 
return(list(summaryStudyTable = studyTable,
            profileTable = profileTable,
            layerTable = layerTable,
            profileMap = profileMap,
            layerMap = layerMap,
            profileValueHistograms = histograms,
            depthValuePlots = depthPlots))  
}