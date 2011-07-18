library( raster)
library( rgdal)
library( ggplot2)
library( xtable)
library( Hmisc)


printAreas <- function( acres) {
  return( paste( round( acres /10^6, digits=1), "Ma (",
                round( acres /10^6 *0.404685642, digits=1), "Mha)",
                sep=" "))
}

getPeelBand <- function( peelBrick, cover) {
  unstack( peelBrick)[[ peelBands[[ cover]]]]
}

rmseRast <- function(obsRast, predRast) {
  sqErr <- overlay( obsRast, predRast,
                   fun=function( obs, pred) return(( obs -pred) ^2))
  return( sqrt( cellStats( sqErr, 'mean')))
}

biasRast <- function(obsRast, predRast) {
  err <- overlay( obsRast, predRast,
                   fun=function( obs, pred) return( obs -pred))
  return( cellStats( err, 'mean'))
}


  
rmseSummary <- function( obsNameFun, predNameFun) {
  sapply( covers,
         function( c) {
           obsRast  <- raster( as.spgdf( handle( obsNameFun(c))))
           predRast <- raster( as.spgdf( handle( predNameFun(c))))
           if( extent( obsRast) != extent( predRast)) {
             intExt <- intersectExtent( obsRast, predRast)
             obsRast <- crop( obsRast, intExt)
             predRast <- crop( predRast, intExt)
           }
           return( c( rmse_frac= rmseRast( obsRast, predRast),
                      bias_frac= biasRast( obsRast, predRast),
                     rmse_acres= rmseRast( areaAcres(obsRast), areaAcres( predRast)),
                     bias_acres= biasRast( areaAcres(obsRast), areaAcres( predRast))))
         })
}
