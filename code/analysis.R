library( raster)
library( rgdal)
library( ggplot2)
library( xtable)
library( Hmisc)

## library( lattice)
## library( RColorBrewer)
## divTheme <- sp.theme( 
##     regions= list(
##         col= colorRampPalette( brewer.pal( 5, "BrBG"), 
##           space= "Lab")(100)))
## seqTheme <- sp.theme(
##     regions= list(
##         col= colorRampPalette( brewer.pal( 5, "YlGn"),
##           space="Lab")(100)))

##                                         # list the subdatasets and split on equal sign
##                                         # e.g. SUBDATASET_2_NAME=RASTERLITE:cusa.sqlite,table=agc_crop 
## maps <- function() {
##   unlist( lapply( strsplit( grep( "NAME",
##                                 attr( GDALinfo( db, silent=TRUE),
##                                      "subdsmdata"),
##                                 value=T),
##                            "="),
##                  function(x) return(x[3])))
## }


## covers <- unlist( lapply( strsplit( maps()[ grep( "^agc", maps())], "_"),
##                          function(x) return( x[2])))

##                                         # arg: map name string
##                                         # res: fully-qualified DSN in db
## dataset  <- function( s) {
##   return( paste( "RASTERLITE:", db, ",table=", s,
##                 sep=""))
## }

##                                         # arg: map name string
##                                         # res: rgdal handle
## handle <- function(map) {
##   return( new( "GDALReadOnlyDataset", dataset( map)))
## }

##                                         # arg: rgdal handle
##                                         # res: spatial grid data frame
## as.spgdf <- function( handle) {
##   result <- as( handle, "SpatialGridDataFrame")
##   #names( result) <- names( handle)
##   return( result)
## }

## #nlcdCrop <- raster( as.spgdf( handle( "nlcd_crop")))
## #agcCrop <-  raster( as.spgdf( handle( "agc_crop")))


## grepHandles <- function( regex) 
##   return( sapply( maps()[ grep( regex, maps())],
##                  handle))

## stackHandles <- function( handles) {
##   result <- stack( sapply( handles, function(x) raster( as.spgdf( x))))
##   attr( result, "layernames") <- names( handles)
##   return( result)
## }

## areaAcres <- function( rast) {
##   result <- rast *area( rast) *247.105381
##   attr( result, "layernames") <- attr( rast, "layernames")
##   return( result)
##}

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
