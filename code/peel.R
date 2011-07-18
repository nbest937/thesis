library( raster)
library( ggplot2)
library( xtable)
library( RColorBrewer)
library( Hmisc)

library( foreach)
library(doMC)
registerDoMC( cores= 9)
#registerDoSEQ()

mlctList <- function( priFile, secFile, pctFile) {
                                        # creates a list of raster objects
                                        # stacking messes up the overlay functions
  priRaster <- raster( priFile) 
  mlct <- 
    list( pri= priRaster,
          sec= if( missing( secFile)) raster( priRaster) else raster( secFile),
          pct= if( missing( pctFile)) raster( priRaster) else raster( pctFile))
  ##sapply( mlct, setMinMax)
}


mlctReclassMatrix <- 
  matrix( c( 0,  0,  0,
             1,  5,  1,
             6,  8,  2,
             9, 10,  3,
            11, 11,  4,
            12, 12,  5,
            13, 13,  6,
            14, 14,  7,
            15, 16,  8,
            253, 253, NA),
         ncol=3, byrow=TRUE)
peelClasses <- mlctReclassMatrix[ 1:9, 3]
names( peelClasses) <- c("water", "forest", "shrub", "open", "wetland", "crop", "urban", "mosaic", "barren")

#peelLegend <- igbpLegend[ mlctReclassMatrix[, 2] +1]


## just in case, save these for later
## paste( deparse( peelLegend), collapse="")

peelLegend <- c("#2041B3", "#069228", "#85AA5B", "#A4D07E", "#73ABAE", "#CCD253", "#D90000", "#9DE36E", "#949494")
names( peelLegend) <- names( peelClasses)

nlcdReclassMatrix <- 
  matrix( c( 11,  11,  0,               # water
             98,  99,  0,
             41,  43,  1,               # forest
             51,  52,  2,               # shrub
             94,  94,  2,
             71,  74,  3,               # open
             81,  81,  3,
             90,  93,  4,               # wetland
             95,  97,  4,
             82,  82,  5,               # crop
             21,  24,  6,               # urban
                                        # no mosaic
             12,  12,  8,               # barren
             31,  32,  8),
         ncol=3, byrow=TRUE)

mlctReclass <- function( mlct, reclassMatrix, overwrite=FALSE, ...) {
                                        # replaces primary and secondary rasters
                                        # but color tables are lost
  reclassFilename <- function( r) {
    parts <- unlist( strsplit( basename( filename( r)), ".", fixed=TRUE))
    paste( parts[ 1], "_reclass.tif", sep="")
  }
  if( overwrite) {
    mlct$pri <- reclass( mlct$pri, reclassMatrix, 
                        filename= reclassFilename( mlct$pri),
                        #datatype= "INT1U",
                        overwrite= TRUE, ...)
    if( "sec" %in% names( mlct)) {
      mlct$sec <- reclass( mlct$sec, reclassMatrix, 
                          filename= reclassFilename( mlct$sec),
                          #datatype= "INT1U",
                          overwrite= TRUE, ...)
    }
  } else {
    mlct$pri <- raster( reclassFilename( mlct$pri))
    if( "sec" %in% names( mlct))
          mlct$sec <- raster( reclassFilename( mlct$sec))
  }
  mlct
}


primaryFraction <- function( mlct, Amin=1.0, overwrite=FALSE, ...) {
                                        # appends an A_p raster to the MLCT list 
                                        # and returns the appended list
  primaryFractionFile <- 
    paste( deparse( substitute( mlct)), "Amin", 
          paste( Amin, "tif", sep= "."),
          sep= "_")
  mlct$Amin <- Amin
  priFracCalcFunc <- function( st) {
    pri <- st[ 1]
    sec <- st[ 2]
    pct <- st[ 3]
    ifelse( is.na(pri), NA,
           ifelse( is.na( sec), 1,
                  ifelse( is.na( pct), Amin,
                         Amin +( 1 -Amin) *pct /100)))
  }

  if( Amin <1 && overwrite)
    mlct$Ap <- calc( stack(mlct$pri, mlct$sec, mlct$pct), 
                       fun= priFracCalcFunc,
                       filename= primaryFractionFile,
                       overwrite= TRUE,
                       ...)
  else if( Amin <1 && !overwrite)
    mlct$Ap <- raster( primaryFractionFile)
  else mlct$Ap <- NULL
  mlct
}

coverFractions <- function( mlct, mosaic= TRUE, overwrite= FALSE, ...)  {
  Amin <- mlct$Amin
  mlctName <- deparse( substitute( mlct))
  classes <- peelClasses[ if( mosaic) 1:length(peelClasses)
                          else names( peelClasses) != "mosaic"]
  fracsBrickFile <-
    if( Amin < 1)
      paste( mlctName,
            "Amin", mlct$Amin, "fracs.tif", 
            sep="_")
    else
      paste( mlctName, 
            "fracs.tif", sep="_")
  if( overwrite) {
    if( Amin < 1.0) {
      fracDoparFun <- function( priFilename, secFilename, ApFilename, ...) {
        foreach( cover= names( classes), .packages= "raster") %dopar% {
          class <- classes[[ cover]]
          frac <-
            calc( stack( raster( priFilename),
                        raster( secFilename),
                        raster( ApFilename)),
                 fun= function( st) {
                   pri <- st[ 1]
                   sec <- st[ 2]
                   Ap <- st[ 3]
                   res <- ifelse( is.na( pri), NA,
                                 ifelse( pri ==class, Ap, 0)
                                 +ifelse( !is.na(sec) & sec ==class, 1 -Ap, 0))
                   #if( res > 1 || res < 0) browser()
                   return( res)
                 },
                 filename= paste( mlctName, cover, "Amin", 
                   paste( Amin, ".tif", sep=""),
                   sep="_"),
                 overwrite= TRUE, ...)
          return( filename( frac))
        }
      }
    } else {
      fracDoparFun <- function( priFilename, ...) {
        foreach( cover= names( classes), .packages= "raster") %dopar% {
          class <- classes[[ cover]]
          frac <-
            calc( raster( priFilename),
                 function( pri) {
                   ifelse( is.na( pri), NA,
                          ifelse( pri ==class, 1, 0))
                 },
                 filename= paste(
                   mlctName,
                   paste( cover, ".tif", sep=""),
                   sep="_"),
                 overwrite= TRUE, ...)
          return( filename( frac))
        }
      }
    }
    mlct$fracs <- 
      brick( stack( fracDoparFun( filename( mlct$pri),
                                 secFilename= filename( mlct$sec),
                                 ApFilename= filename( mlct$Ap),
                                 ...)),
            filename= fracsBrickFile,
            overwrite= TRUE,
            ...)
  } else {
    mlct$fracs <- brick( fracsBrickFile)
  }
  layerNames( mlct$fracs) <- names( classes)
  mlct
}


aggregateFractions <- function( mlct, aggRes= 5/60, overwrite= FALSE, ...) {
  aggBrickFile <-
    if( mlct$Amin < 1)
      paste( deparse( substitute( mlct)), 
            "Amin", mlct$Amin, "agg.tif", 
            sep="_")
    else
      paste( deparse( substitute( mlct)), 
            "agg.tif", sep="_")
  mlct$agg <- 
    if( overwrite)
      aggregate( mlct$fracs, 
                fact= as.integer( round( aggRes /res(mlct$fracs))),
                fun= mean,
                expand= FALSE,
                filename= aggBrickFile,
                overwrite= TRUE, ...)
    else
      brick( list.files( getwd(), patt=aggBrickFile,
                        full.names= TRUE))
  layerNames( mlct$agg) <- layerNames( mlct$fracs)
  mlct
}

peelBrickLayer <- function( peel, class) {
  peel[[ peelClasses[[ class]] +1]]
}

decomposeMosaic <- function( mlct, overwrite= FALSE, ...) {
  deltaBrickFile <- paste( deparse( substitute( mlct)), 
                          "Amin", mlct$Amin, "delta.tif", 
                          sep="_")
  nomosBrickFile <- paste( deparse( substitute( mlct)), 
                          "Amin", mlct$Amin, "nomosaic.tif", 
                          sep="_")
  if( overwrite) {
    overlayForest <- function( water, forest, shrub,
                              open, wetland, crop,
                              urban, mosaic, barren) {
      fso <- forest +shrub +open
      ifelse( fso ==0, 
             forest +mosaic /6,
             forest *( 1 +mosaic /2 /fso))
    }
    overlayShrub <- function( water, forest, shrub,
                             open, wetland, crop,
                             urban, mosaic, barren) {
      fso <- forest +shrub +open
      ifelse( fso ==0, 
             shrub +mosaic /6,
             shrub *( 1 +mosaic /2 /fso))    
    }
    overlayOpen <- function( water, forest, shrub,
                            open, wetland, crop,
                            urban, mosaic, barren) {
      fso <- forest +shrub +open
      ifelse( fso ==0, 
             open +mosaic /6,
             open *( 1 +mosaic /2 /fso))        
    }
    overlayCrop <- function( water, forest, shrub,
                            open, wetland, crop,
                            urban, mosaic, barren) {
      crop +mosaic /2
    }
    mlct$nomos <- 
      brick(
            peelBrickLayer( mlct$agg, "water"),
            overlay( mlct$agg, fun= overlayForest,
                    filename= "newAgg_forest.tif",
                    overwrite= TRUE),
            overlay( mlct$agg,
                    fun= overlayShrub,
                    filename= "newAgg_shrub.tif",
                    overwrite= TRUE),
            overlay( mlct$agg,
                    fun= overlayOpen,
                    filename= "newAgg_open.tif",
                    overwrite= TRUE),
            peelBrickLayer( mlct$agg, "wetland"),
            overlay( mlct$agg,
                    fun= overlayCrop,
                    filename= "newAgg_crop.tif",
                    overwrite= TRUE),
            peelBrickLayer( mlct$agg, "urban"),
            peelBrickLayer( mlct$agg, "barren"),
            overlay( mlct$agg,
                    fun= sum,
                    filename= "newAgg_total.tif",
                    overwrite= TRUE),
            filename= nomosBrickFile,
            overwrite= overwrite, ...)
     mlct$delta <-
      brick(
            peelBrickLayer( mlct$nomos, "forest") -peelBrickLayer( mlct$agg, "forest"),
            peelBrickLayer( mlct$nomos, "shrub") -peelBrickLayer( mlct$agg, "shrub"),
            peelBrickLayer( mlct$nomos, "open") -peelBrickLayer( mlct$agg, "open"),
            0 -peelBrickLayer( mlct$agg, "mosaic"),
            peelBrickLayer( mlct$nomos, "crop") -peelBrickLayer( mlct$agg, "crop"),
            filename= deltaBrickFile,
            overwrite= overwrite, ...)
  } else {
    mlct$nomos <- brick( list.files( getwd(),
                                    patt= nomosBrickFile,
                                    full.names= TRUE,
                                    recursive= TRUE ))
    mlct$delta <- brick( list.files( getwd(),
                                    patt= deltaBrickFile,
                                    full.names= TRUE,
                                    recursive= TRUE))
  }
  layerNames( mlct$nomos) <-
    c( names( peelClasses)[ names( peelClasses) !="mosaic"],
      "total")
  layerNames( mlct$delta) <- c( "forest", "shrub", "open", "mosaic", "crop") 
  mlct
}


acreageTable <- function( rasterNames) {

  dataSets <- sapply( rasterNames,
                     function( n) eval( parse( text=n)))

  areas <- llply( dataSets,
                 function( d) {
                   res <- cellStats( d *acres, sum)
                   names( res) <- layerNames( d)
                   res
                 })

  areasDf <- ldply( areas, function( a) melt( t( as.data.frame( a))))

  areasCt <- cast( areasDf, X2 ~ .id, subset= X2 != "total", sum, margins="grand_row")
  rownames( areasCt) <- areasCt[, "X2"]
  areasCt <- areasCt[, -1]
  areasCt <- areasCt[ c( names( peelClasses), "(all)"), rasterNames]
}

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
