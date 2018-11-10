
#' Title of function here
#' 
#' description here This is a test function that converts soil average depths into soil depth bounds.
#'
#' @param soilDepthAvg 
#' @param topRef 
#'
#' @return 
#'

soilDepthAvgToBound <- function(soilDepthAvg,topRef= 0){
  # soilDepthAvg <- c(12,48,62,75,92,112,130,148)
  # x <- SoilDepthAvgToBound(soilDepthAvg,0)
  
  #soilDepthAvg <- c(12,48,62,75,92,112,130,148)

n <- length(soilDepthAvg)

ans <- data.frame(soilDepthAvg,
  layer_top=rep(NA, n),
                  layer_bottom=rep(NA, n))

ans$layer_top[1] <- 0 
ans$layer_bottom[1] <- with(ans, (soilDepthAvg[1]-layer_top[1]) + soilDepthAvg[1])

for(ii in 2:n){
  ans$layer_top[ii] <- ans$layer_bottom[ii-1]
  ans$layer_bottom[ii] <- with(ans, (soilDepthAvg[ii]-layer_top[ii]) + soilDepthAvg[ii])
}

return(ans)
}

