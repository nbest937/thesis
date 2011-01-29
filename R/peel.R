library( raster)
##library( rgdal)
##library( lattice)
library( ggplot2)
##library( reshape2)
library( xtable)
##library( Defaults)

library( RColorBrewer)

library( foreach)
library(doMC)
registerDoMC()

mlctList <- function( priFile, secFile, pctFile) {
                                        # creates a list of raster objects
                                        # stacking messes up the overlay functions
  priRaster <- raster( priFile) 
  mlct <- 
    list( pri= priRaster,
          sec= if( missing( secFile)) raster( priRaster) else raster( secFile),
          pct= if( missing( pctFile)) raster( priRaster) else raster( pctFile))
  sapply( mlct, setMinMax)
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
            15, 16,  8),
         ncol=3, byrow=TRUE)
peelClasses <- mlctReclassMatrix[, 3]
names( peelClasses) <- c("water", "forest", "shrub", "open", "wetland", "crop", "urban", "mosaic", "barren")

#peelLegend <- igbpLegend[ mlctReclassMatrix[, 2] +1]


## just in case, save these for later
## paste( deparse( peelLegend), collapse="")
## peelLegend <- c("#2041B3", "#069228", "#85AA5B", "#A4D07E", "#73ABAE", "#CCD253", "#D90000", "#9DE36E", "#949494")

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
                          datatype= "INT1U",
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
  overlayFunction <- function( pri, sec, pct) {
    ifelse( is.na(pri), NA,
           ifelse( is.na( sec), 1,
                  ifelse( is.na( pct), Amin,
                         Amin +( 1 -Amin) *pct /100)))
  }
  mlct$Amin <- Amin
  if( Amin <1 && overwrite)
    mlct$Ap <- overlay( mlct$pri, mlct$sec, mlct$pct, 
                       fun= overlayFunction,
                       filename= primaryFractionFile,
                       overwrite= TRUE,
                       ...)
  else if( Amin <1 && !overwrite)
    mlct$Ap <- raster( primaryFractionFile)
  else mlct$Ap <- NULL
  mlct
}

primaryOnlyFracsFun <-
  function( mosaic) {
    function( pri) {
      v <- rep( ifelse( is.na( pri), NA, 0), times= length( peelClasses))
      if( !is.na( pri)) v[ pri +1] <- 1
      if( mosaic) v else v[ names( peelClasses) != "mosaic"]
    }
  }

    ## if( mosaic) b
    ## else b[ ,peelClasses[ names( peelClasses) != "mosaic"] +1]

    ##   res <- matrix( 0, nrow= length( pri), ncol= cols)
    ##   res[ is.na( pri),] <- rep( NA, times= cols)
      
      
    ## aaply( pri, 1,
    ##       function( x) {
    ##         ifelse( is.na( x), NA,
    ##                ifelse( x == pri, 1, 0))
    ##       })}

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
      fracs <-
        sapply( names( classes),
               function( cover) {
                 class <- classes[[ cover]]
                 overlayFunction <- function( pri, sec, Ap) {
                   ifelse( is.na( pri), NA,
                          ifelse( pri ==class, Ap, 0)
                          +ifelse( !is.na(sec) & sec ==class, 1 -Ap, 0))
                 }
                 overlay( mlct$pri, mlct$sec, mlct$Ap, 
                         fun= overlayFunction,
                         filename= paste( mlctName, cover, "Amin", 
                           paste( Amin, ".tif", sep=""),
                           sep="_"),
                         overwite= TRUE, ...)
               })
    } else {
      fracDoparFun <- function( priFilename, ...) {
        ##      fracFilenames <-
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
      ## mlct$fracs <-
      ##   sapply( names( classes),
      ##          function( cover) {
      ##            class <- classes[[ cover]]
      ##            calc( mlct$pri,
      ##                 function( pri) {
      ##                   ifelse( is.na( pri), NA,
      ##                          ifelse( pri ==class, 1, 0))
      ##                 },
      ##                 filename= paste(
      ##                   mlctName,
      ##                   paste( cover, ".tif", sep=""),
      ##                   sep="_"),
      ##                 overwrite= TRUE, ...)
      ##          })
      ## mlct$fracs <-
      ##   calc( stack( mlct$pri),
      ##        fun= primaryOnlyFracsFun( mosaic),
      ##        filename= fracsBrickFile,  # paste("calc", fracsBrickFile, sep= "_"),
      ##        overwrite= TRUE, ...)
      ##                                   # call to stack() is work-around needed until raster 1.7-27
      ##                                   # still using it in 1.7-29 to avoid "Error in .local(x, fun, ...) : function 'fun' returns more than one value"
      ##                                   # but still get
      ##                                   #      "Error in v[, i] : incorrect number of dimensions
      ##                                   #       In addition: Warning message: In compare(rasters) :
      ##                                   #       There should be at least 2 Raster* objects to compare"
      ##                                   #
      ##                                   # going back to sapply() approach
    }
    ## mlct$fracs <- 
    ##   do.call( brick, 
    ##           #c( unlist( mlct$fracs, use.names=FALSE),
    ##           c( fracFilenames,
    ##             filename= fracsBrickFile,
    ##             overwrite= TRUE,
    ##             ...))
    mlct$fracs <- 
      brick( stack( fracDoparFun( filename( mlct$pri), ...)),
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
      brick( aggBrickFile)
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
    mlct$nomos <- brick( nomosBrickFile)
    mlct$delta <- brick( deltaBrickFile)
  }
  layerNames( mlct$nomos) <-
    c( names( peelClasses)[ names( peelClasses) !="mosaic"],
      "total")
  layerNames( mlct$delta) <- c( "forest", "shrub", "open", "mosaic", "crop") 
  mlct
}


ggplotRaster <- function( r, samp) {
  df <- data.frame( as( sampleRegular( r, ncell( r)*samp, 
                                      asRaster=TRUE), 
                       "SpatialGridDataFrame"))
  ## names(df)[ 1:length( layerNames( r))] <- layerNames( r)
  ggplot( data= df) +
    geom_tile( aes( x= s1, y= s2, fill= values)) +
    theme_bw() +
    scale_x_continuous( expand= c( 0,0)) +
    scale_y_continuous( expand= c( 0,0)) +
    opts( panel.grid.minor= theme_blank(),
          panel.grid.major= theme_blank(),
          panel.background= theme_blank(),
              axis.title.x= theme_blank(),
               axis.text.x= theme_text( angle= 90, hjust=1),
              axis.title.y= theme_blank()) 
}


peelMap <- function( r, samp) {
  p <- ggplotRaster( r, samp)
  p$data$values <- factor( p$data$values, 
                          levels= peelClasses, 
                          labels= names( peelClasses))
  p +
    geom_tile( aes( x= s1, y= s2, 
                    fill= values)) + 
    scale_fill_manual( "",
                      values= peelLegend, 
                      breaks= names( peelClasses))
}

coverMaps <- function( r, samp=1, ...) {
  df <- data.frame( as( sampleRegular( r, ncell( r)*samp, 
                                      asRaster=TRUE), 
                       "SpatialGridDataFrame"))
  names( df)[ grep( "^values", names( df))] <- layerNames( r)
  df <- melt( df, id.vars= c("s1", "s2"))
  ## name paraameters seem to have no effect -- need to upgrade?
  ## p <- p %+% reshape2::melt( p$data,
  ##                           id.vars= c("s1", "s2"),
  ##                           variable.name= "cover",
  ##                           value.name= "frac")
  ggplot( data= df) +
    geom_tile( aes( x= s1, y= s2, fill= value)) +
    theme_bw() +
    scale_x_continuous( expand= c( 0,0)) +
    scale_y_continuous( expand= c( 0,0)) +
    opts( panel.grid.minor= theme_blank(),
          panel.grid.major= theme_blank(),
          panel.background= theme_blank(),
              axis.title.x= theme_blank(),
               axis.text.x= theme_text( angle= 90, hjust=1),
              axis.title.y= theme_blank()) +
    scale_fill_gradientn( colours= rev( brewer.pal( 6, "YlGn")), 
                         limits= c( 1, 0),
                         breaks= seq( 1, 0, by= -0.2)) +
    facet_wrap(~ variable)
}

## coverDiffMaps <- function( r, samp= 1, ...) {
##   coverMaps( r, samp, ...) +
##     scale_fill_gradientn( colours= rev( brewer.pal( 11, "BrBG")), 
##                          limits= c( 1, -1),
##                          breaks= seq( 1, -1, by= -0.2))
