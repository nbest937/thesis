library( raster)
##library( rgdal)
##library( lattice)
library( ggplot2)
library( reshape2)
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



mlctReclass <- function( mlct, reclassMatrix, ...) {
                                        # replaces primary and secondary rasters
                                        # but color tables are lost
  reclassFilename <- function( r) {
    parts <- unlist( strsplit( basename( filename( r)), ".", fixed=TRUE))
    paste( parts[ 1], "_reclass.", parts[ 2], sep="")
  }
  mlct$pri <- reclass( mlct$pri, reclassMatrix, 
                      filename= reclassFilename( mlct$pri), ...)
  mlct$sec <- reclass( mlct$sec, reclassMatrix, 
                      filename= reclassFilename( mlct$sec), ...)
  mlct
}

primaryFraction <- function( mlct, Amin, primaryFractionFile, ...) {
                                        # appends an A_p raster to the MLCT list 
                                        # and returns the appended list
  if( missing( primaryFractionFile))
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
  mlct$Ap <- overlay( mlct$pri, mlct$sec, mlct$pct, 
                     fun= overlayFunction,
                     filename= primaryFractionFile,
                     ...)
  mlct
}

coverFractions <- function( mlct, ...) {
  Amin <- mlct$Amin
  fracs <- 
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
                       sep="_"), ...)
           })
  mlct$fracs <- 
    do.call( brick, 
            c( unlist( fracs, use.names=FALSE), 
              filename= paste( deparse( substitute( mlct)), 
                "Amin", mlct$Amin, "fracs.tif", 
                sep="_"),
              ...))
  layerNames( mlct$fracs) <- names( peelClasses)
  mlct
}


aggregateFractions <- function( mlct, aggRes= 5/60, ...) {
  mlct$agg <- 
    aggregate( mlct$fracs, 
              fact= aggRes /res(mlct$fracs),
              fun= mean,
              expand= FALSE,
              filename= paste( deparse( substitute( mlct)), 
                "Amin", mlct$Amin, "agg.tif", 
                sep="_"),
              ...)
  layerNames( mlct$agg) <- names( peelClasses)
  mlct
}

peelBrickLayer <- function( peel, class) {
  peel[[ peelClasses[[ class]] +1]]
}

decomposeMosaic <- function( mlct, ...) {
  overlayForest <- function( water, forest, shrub, open, mosaic, crop, urban, wetland, barren) {
    fso <- forest +shrub +open
    ifelse( fso ==0, 
           forest +mosaic /6,
           forest *( 1 +mosaic /2 /fso))
  }
  overlayShrub <- function( water, forest, shrub, open, mosaic, crop, urban, wetland, barren) {
    fso <- forest +shrub +open
    ifelse( fso ==0, 
           shrub +mosaic /6,
           shrub *( 1 +mosaic /2 /fso))    
  }
  overlayOpen <- function( water, forest, shrub, open, mosaic, crop, urban, wetland, barren) {
    fso <- forest +shrub +open
    ifelse( fso ==0, 
           open +mosaic /6,
           open *( 1 +mosaic /2 /fso))        
  }
  overlayCrop <- function( water, forest, shrub , open, mosaic, crop, urban, wetland, barren) {
    crop +mosaic /2
  }
  agg <- mlct$agg
  newAgg <- 
    brick(
          peelBrickLayer( agg, "water"),
          overlay( agg, fun= overlayForest,
                  filename= "newAgg_forest.tif",
                  overwrite= TRUE),
          overlay( agg,
                  fun= overlayShrub,
                  filename= "newAgg_shrub.tif",
                  overwrite= TRUE),
          overlay( agg,
                  fun= overlayOpen,
                  filename= "newAgg_open.tif",
                  overwrite= TRUE),
          overlay( agg,
                  fun= overlayCrop,
                  filename= "newAgg_crop.tif",
                  overwrite= TRUE),
          peelBrickLayer( agg, "urban"),
          peelBrickLayer( agg, "wetland"),
          peelBrickLayer( agg, "barren"),
          overlay( agg,
                  fun= sum,
                  filename= "newAgg_total.tif",
                  overwrite= TRUE),
          filename= paste( deparse( substitute( mlct)), 
                "Amin", mlct$Amin, "nomosaic.tif", 
                sep="_"),
          ...)
  layerNames( newAgg) <- c( names( peelClasses)[ names( peelClasses) != "mosaic"],
                           "total")
  delta <-
    brick(
          peelBrickLayer( newAgg, "forest") -peelBrickLayer( agg, "forest"),
          peelBrickLayer( newAgg, "shrub") -peelBrickLayer( agg, "shrub"),
          peelBrickLayer( newAgg, "open") -peelBrickLayer( agg, "open"),
          peelBrickLayer( newAgg, "crop") -peelBrickLayer( agg, "crop"),
          0 -peelBrickLayer( agg, "mosaic"),
          filename= paste( deparse( substitute( mlct)), 
                "Amin", mlct$Amin, "delta.tif", 
                sep="_"),
          ...)
  layerNames( delta) <- c( "forest", "shrub", "open", "mosaic", "crop")
  mlct$delta <- delta
  mlct$agg <- newAgg
  mlct
}


ggplotRaster <- function( r, samp) {
  df <- data.frame( as( sampleRegular( r, ncell( r)*samp, 
                                      asRaster=TRUE), 
                       "SpatialGridDataFrame"))
  names(df)[ 1:length( layerNames( r))] <- layerNames( r)
  ggplot( data= df) +
    scale_x_continuous( expand= c( 0,0)) +
    scale_y_continuous( expand= c( 0,0)) +
    opts( panel.grid.minor = theme_blank(),
         panel.grid.major = theme_blank(),
         panel.background = theme_blank(),
         axis.title.x = theme_blank(), 
         axis.title.y = theme_blank()) 
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

coverMaps <- function( r, samp, ...) {
  p <- ggplotRaster( r, samp)
  ## name paraameters seem to have no effect -- need to upgrade?
  ## p <- p %+% reshape2::melt( p$data,
  ##                           id.vars= c("s1", "s2"),
  ##                           variable.name= "cover",
  ##                           value.name= "frac")
  p <- p %+% melt( p$data, id.vars= c("s1", "s2"))
  p +
    geom_tile( aes( x= s1, y= s2, fill=value)) +
    scale_fill_gradientn( colours= seqTheme(10), limits= c( 0,1)) +
    facet_wrap(~ variable)
}
