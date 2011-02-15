library(raster)
library(rgdal)

db <- "cusa.sqlite"

                                        # list the subdatasets and split on equal sign
                                        # e.g. SUBDATASET_2_NAME=RASTERLITE:cusa.sqlite,table=agc_crop 
maps <- function() {
  unlist( lapply( strsplit( grep( "NAME",
                                attr( GDALinfo( db, silent=TRUE),
                                     "subdsmdata"),
                                value=T),
                           "="),
                 function(x) return(x[3])))
}


covers <- unlist( lapply( strsplit( maps()[ grep( "^agc", maps())], "_"),
                         function(x) return( x[2])))

                                        # arg: map name string
                                        # res: fully-qualified DSN in db
dataset  <- function( s) {
  return( paste( "RASTERLITE:", db, ",table=", s,
                sep=""))
}

                                        # arg: map name string
                                        # res: rgdal handle
handle <- function(map) {
  return( new( "GDALReadOnlyDataset", dataset( map)))
}

                                        # arg: rgdal handle
                                        # res: spatial grid data frame
as.spgdf <- function( handle) {
  as( handle, "SpatialGridDataFrame")
}


nlcdCrop <- raster( as.spgdf( handle( "nlcd_crop")))
agcCrop <-  raster( as.spgdf( handle( "agc_crop")))

sqrt( cellStats( overlay( agcCrop,
                         crop( nlcdCrop, agcCrop),
                         fun=function( pred, obs) return(( obs -pred) ^2)),
                'mean'))
## [1] 0.1533531


rmseRast <- function(obsRast, predRast) {
  if( extent( obsRast) > extent( predRast))
    obsRast <- crop( obsRast, predRast)
  if( extent( obsRast) < extent( predRast))
    predRast <- crop( predRast, obsRast)
  sqErr <- overlay( obsRast, predRast,
                   fun=function( obs, pred) return(( obs -pred) ^2))
  return( sqrt( cellStats( sqErr, 'mean')))
}


  
                                        # arg: map name string
                                        # res: 
rmseFrac <- function( obs, pred) {
  obsRast  <- raster( as.spgdf( handle( obs)))
  predRast <- raster( as.spgdf( handle( pred)))
  return( rmseRast( obsRast, predRast))
}

areaAcres <- function( rast)
  return( rast *area( rast) *247.105381)

rmseAcres <- function( obs, pred) {
  obsRast  <- raster( as.spgdf( handle( obs)))
  predRast <- raster( as.spgdf( handle( pred)))
  return( rmseRast( areaAcres( obsRast),
                    areaAcres( predRast)))
}
  

maps[ grep( "^agc", maps)]


rmseSummary <- function( obsNameFun, predNameFun) {
  sapply( covers,
         function( c) {
           return( c( frac= rmseFrac(  obsNameFun(c), predNameFun(c)),
                     acres= rmseAcres( obsNameFun(c), predNameFun(c))))
         })
}

rmseAgc <- rmseSummary( function(c) paste(  "agc", c, sep="_"),
                        function(c) paste( "nlcd", c, sep="_"))

rmseAs00 <- rmseSummary( function(c) paste( "mlct_2001", c, "As00", sep="_"),
                         function(c) paste( "nlcd", c, sep="_"))

rmseAs05 <- rmseSummary( function(c) paste( "mlct_2001", c, "As05", sep="_"),
                         function(c) paste( "nlcd", c, sep="_"))


## getting ready to plot

handles <- sapply( maps()[grep("^agc", maps())], handle)
stackAgc <- stack( sapply( handles, function(x) raster( as.spgdf( x))))
attr( stackAgc, "layernames") <-  covers
plot(stackAgc)

