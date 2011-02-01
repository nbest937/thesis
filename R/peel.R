library( raster)
##library( rgdal)
##library( lattice)
library( ggplot2)
##library( reshape2)
library( xtable)
##library( Defaults)

library( RColorBrewer)


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



mlctReclass <- function( mlct, reclassMatrix, overwrite=FALSE, ...) {
                                        # replaces primary and secondary rasters
                                        # but color tables are lost
  reclassFilename <- function( r) {
    parts <- unlist( strsplit( basename( filename( r)), ".", fixed=TRUE))
    paste( parts[ 1], "_reclass.", parts[ 2], sep="")
  }
  if( overwrite) {
    mlct$pri <- reclass( mlct$pri, reclassMatrix, 
                        filename= reclassFilename( mlct$pri),
                        datatype= "INT1U",
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

coverFractions <- function( mlct, overwrite= FALSE, ...) {
  Amin <- mlct$Amin
  fracsBrickFile <-
    if( Amin < 1)
      paste( deparse( substitute( mlct)), 
            "Amin", mlct$Amin, "fracs.tif", 
            sep="_")
    else
      paste( deparse( substitute( mlct)), 
            "fracs.tif", sep="_")
  if( overwrite) {
    fracs <- if( Amin < 1.0) {
      sapply( names( peelClasses),
             function( cover) {
               class <- peelClasses[[ cover]]
               overlayFunction <- function( pri, sec, Ap) {
                 ifelse( is.na( pri), NA,
                        ifelse( pri ==class, Ap, 0)
                        +ifelse( !is.na(sec) & sec ==class, 1 -Ap, 0))
               }
               overlay( mlct$pri, mlct$sec, mlct$Ap, 
                       fun= overlayFunction,
                       filename= paste( deparse( substitute( mlct)), cover, "Amin", 
                         paste( Amin, ".tif", sep=""),
                         sep="_"),
                       overwite= TRUE, ...)
             })
    } else {
      sapply( names( peelClasses),
             function( cover) {
               class <- peelClasses[[ cover]]
               calc( mlct$pri,
                    function( pri) {
                      ifelse( is.na( pri), NA,
                             ifelse( pri ==class, 1, 0))
                    },
                    filename= paste(
                      deparse( substitute( mlct)),
                      paste( cover, ".tif", sep=""),
                      sep="_"),
                    overwrite= TRUE, ...)
             })
    }
    mlct$fracs <- 
      do.call( brick, 
              c( unlist( fracs, use.names=FALSE), 
                filename= fracsBrickFile,
                overwrite= TRUE,
                ...))
  } else {
    mlct$fracs <- brick( fracsBrickFile)
  }
  layerNames( mlct$fracs) <- names( peelClasses)
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
  layerNames( mlct$agg) <- names( peelClasses)
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
            peelBrickLayer( newAgg, "forest") -peelBrickLayer( mlct$agg, "forest"),
            peelBrickLayer( newAgg, "shrub") -peelBrickLayer( mlct$agg, "shrub"),
            peelBrickLayer( newAgg, "open") -peelBrickLayer( mlct$agg, "open"),
            0 -peelBrickLayer( mlct$agg, "mosaic"),
            peelBrickLayer( newAgg, "crop") -peelBrickLayer( mlct$agg, "crop"),
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
