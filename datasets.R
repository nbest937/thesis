###################################################
### chunk number 1: initialize
###################################################
#line 14 "/home/nbest/thesis/datasets.Rnw"

                                        # load helper functions
                                        # code will appear in appendix
source("~/thesis/code/peel.R")
setwd( "~/thesis/datasets")

##setwd( "/gpfs/pads/projects/see/nbest/thesis/data")
##quartz.options( type="png")

overwriteRasters <- FALSE
overwriteFigures <- FALSE



###################################################
### chunk number 2: thumb
###################################################
#line 88 "/home/nbest/thesis/datasets.Rnw"

## this works but it's slow
##
## thumb <- crop( raster("2001_lct1.tif"), 
##                extent(-83.5, -(82+25/60), 42+55/60, 44+5/60))
##

## these are subsets exported from GRASS

texWd <- setwd("../data")
dataWd <- getwd()

thumb <- mlctList( "thumb_2001_lct1.tif", 
                   "thumb_2001_lct1_sec.tif", 
                   "thumb_2001_lct1_pct.tif")

igbpLegend <- thumb$pri@legend@colortable
igbpLegend <- igbpLegend[ igbpLegend != "#000000"]

## just in case, save these for later
##  paste( deparse( igbpLegend), collapse="")
## igbpLegend <- c("#2041B3", "#006A0F", "#007C25", "#00A25B", "#00A125", "#069228", "#9E9668", "#C1C48F", "#85AA5B", "#B1B741", "#A4D07E", "#73ABAE", "#CCD253", "#D90000", "#9DE36E", "#B6B5C2", "#949494")"



###################################################
### chunk number 3: mlct-reclass
###################################################
#line 124 "/home/nbest/thesis/datasets.Rnw"


thumb <- mlctReclass( thumb, mlctReclassMatrix, overwrite= overwriteRasters)

thumbPlots <- list( pri= peelMap( thumb$pri, 0.4),
                    sec= peelMap( thumb$sec, 0.4))

thumbPlots$pct <- ggplotRaster( thumb$pct, 0.4) + 
  scale_fill_gradientn( "% confidence", 
                       colours= rev( brewer.pal( 7, "YlGn")), 
                       limits= c( 100, 0),
                       breaks= seq( 100, 0, by= -20))



###################################################
### chunk number 4: fig_thumb_pri_reclass
###################################################
#line 151 "/home/nbest/thesis/datasets.Rnw"
setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_pri_reclass.png")
  print( thumbPlots$pri)
  dev.off()
}



###################################################
### chunk number 5: fig_thumb_sec_reclass
###################################################
#line 171 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_sec_reclass.png")
  print( thumbPlots$sec)
  dev.off()
}



###################################################
### chunk number 6: fig_thumb_pct
###################################################
#line 192 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_pct.png")
  print( thumbPlots$pct)
  dev.off()
}



###################################################
### chunk number 7: fig_thumb_pri_facet
###################################################
#line 220 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_pri_facet.png")
  print( thumbPlots$pri + 
        facet_wrap(~ values) + 
        opts( legend.position= "none"))
  dev.off()
}



###################################################
### chunk number 8: fig_thumb_sec_facet
###################################################
#line 243 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_sec_facet.png")
  print( thumbPlots$sec + 
        facet_wrap(~ values) +
        opts( legend.position= "none"))
  dev.off()
}



###################################################
### chunk number 9: mlct
###################################################
#line 269 "/home/nbest/thesis/datasets.Rnw"

## repeat for cUSA
setwd( dataWd)
mlct <- mlctList( "2001_lct1.tif", 
                  "2001_lct1_sec.tif", 
                  "2001_lct1_pct.tif")
mlct  <- mlctReclass( mlct, mlctReclassMatrix, overwrite= overwriteRasters, datatype="INT1U", progress="text")

mlctPlots <- list( pri= peelMap( mlct$pri, 0.01),
                    sec= peelMap( mlct$sec, 0.01))

mlctPlots$pct <- ggplotRaster( mlct$pct, 0.01) + 
  scale_fill_gradientn( "% confidence", colours=rev( brewer.pal( 7, "YlGn")), 
                       limits= c( 100, 0),
                       breaks= seq( 100, 0, by= -20))


###################################################
### chunk number 10: fig_mlct_pri_reclass
###################################################
#line 291 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_mlct_pri_reclass.png")
  print( mlctPlots$pri + coord_equal())
  dev.off()
}



###################################################
### chunk number 11: fig_mlct_sec_reclass
###################################################
#line 312 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_mlct_sec_reclass.png")
  print( mlctPlots$sec + coord_equal())
  dev.off()
}



###################################################
### chunk number 12: fig_mlct_pct
###################################################
#line 333 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_mlct_pct.png")
  print( mlctPlots$pct + coord_equal())
  dev.off()
}



###################################################
### chunk number 13: fig_mlct_pri_facet
###################################################
#line 358 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_mlct_pri_facet.png")
  print( mlctPlots$pri + 
        facet_wrap(~ values) + 
        coord_equal() +
        opts( legend.position= "none"))
  dev.off()
}



###################################################
### chunk number 14: fig_mlct_sec_facet
###################################################
#line 382 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_mlct_sec_facet.png")
                                        # had to remove some NAs from the data
  mlctPlots$sec$data <- mlctPlots$sec$data[ !is.na(mlctPlots$sec$data$values),]
  print( mlctPlots$sec + 
        facet_wrap(~ values) + 
        coord_equal() +
        opts( legend.position= "none"))
  dev.off()
}



###################################################
### chunk number 15: thumbPlots
###################################################
#line 480 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

## calculate cover fractions and aggregate for detail area

thumb  <- primaryFraction( thumb, Amin=0.5, overwrite= overwriteRasters, progress= "text")
thumb1 <- primaryFraction( thumb, Amin=1.0, overwrite= overwriteRasters, progress= "text")
thumb  <- coverFractions( thumb, overwrite= overwriteRasters, progress= "text")
thumb1 <- coverFractions( thumb1, overwrite= overwriteRasters, progress= "text")
thumb  <- aggregateFractions( thumb, overwrite= overwriteRasters, progress= "text")
thumb1 <- aggregateFractions( thumb1, overwrite= overwriteRasters, progress= "text")

## seems like brick() has a bug such that the filenames have no paths
thumb$fracs <- brick( paste( getwd(), filename( thumb$fracs), sep="/"))
thumb1$fracs <- brick( paste( getwd(), filename( thumb1$fracs), sep="/"))
thumb$agg <- brick( paste( getwd(), filename( thumb$agg), sep="/"))
thumb1$agg <- brick( paste( getwd(), filename( thumb1$agg), sep="/"))


thumbPlots <- list( fracs= coverMaps( thumb$fracs, 0.4),
                   agg= coverMaps( thumb$agg, 1))
thumbPlots1 <- list( fracs= coverMaps( thumb1$fracs, 0.4),
                   agg= coverMaps( thumb1$agg, 1))



###################################################
### chunk number 16: fig_thumb_fracs
###################################################
#line 510 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_fracs.png")
  print( thumbPlots$fracs)
  dev.off()
}



###################################################
### chunk number 17: fig_thumb1_fracs
###################################################
#line 530 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb1_fracs.png")
  print( thumbPlots1$fracs)
  dev.off()
}



###################################################
### chunk number 18: fig_thumb_agg
###################################################
#line 551 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_agg.png")
  print( thumbPlots$agg)
  dev.off()
}



###################################################
### chunk number 19: fig_thumb1_agg
###################################################
#line 571 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb1_agg.png")
  print( thumbPlots1$agg)
  dev.off()
}



###################################################
### chunk number 20: thumbAggDiff
###################################################
#line 592 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

thumbAggDiff <- 
  if( overwriteRasters) {
    overlay( thumb$agg, thumb1$agg, 
            fun= function( t, t1) t -t1, 
            filename= "thumb_agg_diff.tif", 
            overwrite= TRUE)
  } else brick( "thumb_agg_diff.tif")
layerNames( thumbAggDiff) <- layerNames( thumb$agg)

thumbAggDiffPlot <- coverMaps( thumbAggDiff) + 
  scale_fill_gradientn( "diff", colours= rev( brewer.pal( 11, "BrBG")), 
                         limits= c( 0.1, -0.1),
                         breaks= seq( 0.1, -0.1, by= -0.02))



###################################################
### chunk number 21: fig_thumb_agg_diff
###################################################
#line 615 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_agg_diff.png")
  print( thumbAggDiffPlot)
  dev.off()
}



###################################################
### chunk number 22: thumbNomos
###################################################
#line 673 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)
thumb  <- decomposeMosaic( thumb, overwrite= overwriteRasters, progress= "text")
thumb1 <- decomposeMosaic( thumb1, overwrite= overwriteRasters, progress= "text")
thumb$nomos <- brick( paste( getwd(), filename( thumb$nomos), sep="/"))
thumb1$nomos <- brick( paste( getwd(), filename( thumb1$nomos), sep="/"))
thumb$delta <- brick( paste( getwd(), filename( thumb$delta), sep="/"))
thumb1$delta <- brick( paste( getwd(), filename( thumb1$delta), sep="/"))


thumbPlots$nomos <- coverMaps( thumb$agg, 1)  
thumbPlots1$nomos <- coverMaps( thumb1$agg, 1)  

thumbNomosDiff <- 
  if( overwriteRasters) {
    overlay( thumb$agg, thumb1$agg, 
            fun= function( t, t1) t -t1, 
            filename= "thumb_nomos_diff.tif", 
            overwrite= TRUE)
  } else brick( "thumb_nomos_diff.tif")
layerNames( thumbNomosDiff) <- layerNames( thumb$agg)

thumbNomosDiffPlot <- coverMaps( thumbNomosDiff) + 
  scale_fill_gradientn( "diff", colours= rev( brewer.pal( 11, "BrBG")), 
                         limits= c( 0.32, -0.32),
                         ## breaks= { b <- c( 0.3, 0.15, 0.075, 0.075/2, 0.075/4)
                         ##           c( b, 0, rev( -b))
                         ##         })
                         breaks= { b <- c( 0.01, 0.02, 0.04, 0.08, 0.16, 0.32)
                                   c( rev( b), 0, -b)
                                 })



###################################################
### chunk number 23: fig_thumb_nomos
###################################################
#line 711 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb_nomos.png")
  print( thumbPlots$nomos)
  dev.off()
}



###################################################
### chunk number 24: fig_thumb1_nomos
###################################################
#line 732 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures) {
  png( file="fig_thumb1_nomos.png")
  print( thumbPlots1$nomos)
  dev.off()
}



###################################################
### chunk number 25: fig_thumb_nomos_diff
###################################################
#line 753 "/home/nbest/thesis/datasets.Rnw"

setwd( texWd)
if( overwriteFigures ) {
  png( file="fig_thumb_nomos_diff.png")
  print( thumbNomosDiffPlot)
  dev.off()
}



###################################################
### chunk number 26: mlct1
###################################################
#line 776 "/home/nbest/thesis/datasets.Rnw"


setwd( dataWd)

mlct  <- primaryFraction( mlct, Amin= 0.5, overwrite= overwriteRasters, progress="text")
mlct  <- coverFractions( mlct, overwrite= overwriteRasters, progress="text")
mlct  <- aggregateFractions( mlct, overwrite= overwriteRasters, progress="text")
mlct  <- decomposeMosaic( mlct, overwrite= overwriteRasters, progress="text")

mlct1  <- primaryFraction( mlct, Amin=1.0, overwrite= overwriteRasters, progress="text")
mlct1  <- coverFractions( mlct1, overwrite= overwriteRasters, progress="text")
mlct1  <- aggregateFractions( mlct1, overwrite= overwriteRasters, progress="text")
mlct1  <- decomposeMosaic( mlct1, overwrite= overwriteRasters, progress="text")

## might be useful to cross-tabulate the primary and secondary
## frequencies for the cUSA

## table(thumbDf@data$pri, thumbDf@data$sec)



###################################################
### chunk number 27: agland2000 eval=FALSE
###################################################
## #line 820 "/home/nbest/thesis/datasets.Rnw"
## 
## setwd( "~/thesis/data/agland2000")
## agland <- stack( list.files( patt= "2000_5min.nc$"))
## layerNames(agland) <- c("crop", "pasture")
## agland <- setMinMax( agland)
##   
## thumbAgland <- crop( agland,
##                     extent(-83.5, -(82+25/60), 42+55/60, 44+5/60),
##                     filename= "thumbAgland.tif",
##                     progress="text")
## 
## 


###################################################
### chunk number 28: mlu eval=FALSE
###################################################
## #line 846 "/home/nbest/thesis/datasets.Rnw"
## 
## cusaDf <- readOGR("PG:host=db dbname=cim","gadm.cusa")
## 
## cusa <- raster(mlct$pri)
## res(cusa) <- 5/60
## 
## cusa <- rasterize( cusaDf, cusa, field= "id_1", filename= "gadm1_cusa.tif")
## 
## foo <- raster("nlcd_agg.tif")                  
## gadm <- raster( foo)                 
## res(gadm) <- 15/3600                  
## gadm[] <- 0                  
## writeRaster(gadm, filename= "foo.tif", overwrite=TRUE, NAflag=0)
## system( "gdal_translate -ot UInt16 -a_nodata 0 foo.tif gadm1_cusa.tif")                  
##                   
## system( "gdal_rasterize -at -a id_1 -ot UInt16 -l gadm.cusa 'PG:host=db dbname=cim' gadm1_cusa.tif")
## 


###################################################
### chunk number 29: nlcd
###################################################
#line 889 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

setwd( "nlcd")
nlcdWd <- getwd()

thumbNlcd <- list( pri=raster( "thumb_nlcd2001.tif"))
thumbNlcd <- sapply( thumbNlcd, setMinMax)

## thumbNlcdResamp <- raster( thumbNlcd$pri)
## res(thumbNlcdResamp) <- 1.25/3600
## thumbNlcdResamp <- resample( thumbNlcd$pri, thumbNlcdResamp,
##                             method="ngb",
##                             filename="thumb_nlcd2001_resamp.tif",
##                             progress="text")
## thumbNlcd$pri <- thumbNlcdResamp


#nlcd <- list( pri=raster( "nlcd_geo.tif"))
#nlcd <- sapply( nlcd, setMinMax)



###################################################
### chunk number 30: nlcd-reclass
###################################################
#line 918 "/home/nbest/thesis/datasets.Rnw"

setwd( nlcdWd)



thumbNlcd <- mlctReclass( thumbNlcd, nlcdReclassMatrix, overwrite= overwriteRasters, progress="text")

thumbNlcdPlot <- peelMap(thumbNlcd$pri, 0.05)


thumbNlcd$Amin <- 1
thumbNlcd <- coverFractions( thumbNlcd, mosaic=FALSE, overwrite= overwriteRasters, progress= "text")
thumbNlcd <- aggregateFractions( thumbNlcd, overwrite= overwriteRasters, progress="text")

thumbNlcdMask <-
  if( overwriteRasters) {
    calc( thumbNlcd$pri,
         function( x) {
           ifelse( x %in% peelClasses[ c( "water", "wetland", "urban")],
                  x, NA)
         },
         datatype= "INT2U",
         overwrite= TRUE,
         filename= "thumbNlcdMask.tif",
         progress= "text")
  } else {
    raster( "thumbNlcdMask.tif")
  }
thumbNlcdMask <- setMinMax( thumbNlcdMask)


## system( "gdalwarp -of VRT -ts $(( 260*12)) 0  thumb_2001_lct1_reclass.tif thumb_2001_lct1_reclass_resamp.vrt")
## thumbResamp <- raster( "thumb_2001_lct1_reclass_resamp.vrt")

thumbResamp <-
  if( overwriteRasters) {
    resample( thumb$pri, thumbNlcd$pri,
             method="ngb",
             datatype= "INT2U",
             overwrite= TRUE,
             filename= "thumbResamp.tif",
             progress= "text")
  } else raster( "thumbResamp.tif")

thumbResampSec <-
  if( overwriteRasters) {
    resample( thumb$sec, thumbNlcd$pri,
             method="ngb",
             datatype= "INT2U",
             overwrite= TRUE,
             filename= "thumbResampSec.tif",
             progress= "text")
  } else raster( "thumbResampSec.tif")

thumbResampAp <-
  if( overwriteRasters) {
    resample( thumb$Ap, thumbNlcd$pri,
             method="ngb",
             datatype= "FLT4S",
             overwrite= TRUE,
             filename= "thumbResampAp.tif",
             progress= "text")
  } else raster( "thumbResampAp.tif")

thumbNlcdMlct <-
  if( overwriteRasters) {
    mask( thumbResamp,
         thumbNlcdMask,
         datatype= "INT2U",
         overwrite= TRUE,
         filename= "thumbNlcdMlct.tif",
         progress= "text")
  } else {
    raster( "thumbNlcdMlct.tif")
  }

thumbNlcdMlctSec <-
  if( overwriteRasters) {
    mask( thumbResampSec,
         thumbNlcdMask,
         datatype= "INT2U",
         overwrite= TRUE,
         filename= "thumbNlcdMlctSec.tif",
         progress= "text")
  } else {
    raster( "thumbNlcdMlctSec.tif")
  }

thumbNlcdMlctAp <-
  if( overwriteRasters) {
    mask( thumbResampAp,
         thumbNlcdMask,
         datatype= "FLT4S",
         overwrite= TRUE,
         filename= "thumbNlcdMlctAp.tif",
         progress= "text")
  } else {
    raster( "thumbNlcdMlctAp.tif")
  }

##crosstab( thumbNlcdMlct, thumbNlcdMask)

## thumbOffsetsInput <-
##   if( overwriteRasters) {
##     brick( stack( "thumbNlcdMlct.tif", "thumbNlcdMask.tif"),
##           filename= "thumbOffsets_input.tif",
##           overwrite= TRUE)
##   } else brick( "thumbOffsets_input.tif")

## thumbOffsetsInputAp <-
##   if( overwriteRasters) {
##     brick( stack( "thumbNlcdMlct.tif",
##                  "thumbNlcdMlctSec.tif",
##                  "thumbNlcdMlctAp.tif",
##                  "thumbNlcdMask.tif"),
##           filename= "thumbOffsetsInputAp.tif",
##           overwrite= TRUE)
##   } else brick( "thumbOffsetsInputAp.tif")

thumbOffsetsInputAp <-
  if( overwriteRasters) {
    brick( stack( thumbNlcdMlct,
                 thumbNlcdMlctSec,
                 thumbNlcdMlctAp,
                 thumbNlcdMask),
          filename= "thumbOffsetsInputAp.tif",
          overwrite= TRUE,
          progress= "text")
  } else brick( "thumbOffsetsInputAp.tif")




##offsetCalcFunWater <- offsetCalcFun(0)

## offsetCalcFunApWater <- offsetCalcFunAp(0)
## debug( offsetCalcFunApWater)

## thumbOffsets$water <-
##   calc( thumbOffsets$input,
##        fun= offsetCalcFunWater, #offsetCalcFun( 0),
##        datatype= "INT2S",
##        overwrite= TRUE,
##        filename= "thumbOffsets_water.tif",
##        progress= "text")

## thumbOffsetsAp$water <-
##   calc( thumbOffsetsInputAp,
##        fun= offsetCalcFunApWater, #offsetCalcFun( 0),
##        datatype= "INT2S",
##        overwrite= TRUE,
##        filename= "thumbOffsetsAp_water.tif",
##        progress= "text")

## thumbOffsets <-
##   sapply( grep( "water|wetland|urban",
##                names(peelClasses), value=TRUE),
##          function( cover) {
##            fn <- paste( "thumbOffsets",
##                   paste( cover, "tif", sep= "."),
##                   sep= "_")
##            print( paste(cover, fn))
##            if( overwriteRasters || !( file.access( fn) ==0))
##              calc( thumbOffsetsInput,
##                   fun= offsetCalcFun( peelClasses[[ cover]]),
##                   datatype= "INT2S",
##                   overwrite= TRUE,
##                   filename= fn,
##                   progress= "text")
##            else brick( fn)
##          })

## thumbOffsets <-
##   sapply( names( thumbOffsets),
##          function( cover) {
##            fn <- paste( "thumbOffsets",
##                        cover, "agg.tif", sep= "_")
##            print( paste( cover, fn))
##            if( overwriteRasters || !( file.access( fn) ==0))
##              aggregate( thumbOffsets[[ cover]],
##                        fact= 5/60 /res( thumbOffsets[[ cover]]),
##                        expand= FALSE,
##                        filename= fn,
##                        datatype= "FLT4S",
##                        overwrite= TRUE)
##            else brick( fn)
##          })

offsetCalcFunAp <- function( class) {
  fun <- function( st) {
    pri <- st[ 1]
    sec <- st[ 2]
    Ap <- st[ 3]
    nlcd <- st[ 4]
    result <- matrix( 0, nrow= 1, ncol= 9)
    if( !is.na( pri) &&nlcd ==class) {
      result[ 1, pri +1] <- -Ap
      result[ 1, sec +1] <- Ap -1
      result[ 1, nlcd +1] <- result[ 1, nlcd +1] +1
    }
    result
  }
  fun
}

thumbOffsetsAp <-
  sapply( grep( "water|wetland|urban",
               names(peelClasses), value=TRUE),
         function( cover) {
           fn <- paste( "thumbOffsetsAp",
                  paste( cover, "tif", sep= "."),
                  sep= "_")
           print( paste(cover, fn))
           if( overwriteRasters || !( file.access( fn) ==0)) {
             calc( thumbOffsetsInputAp,
                  fun= offsetCalcFunAp( peelClasses[[ cover]]),
                  datatype= "FLT4S",
                  overwrite= TRUE,
                  filename= fn,
                  progress= "text")
           } else brick( fn)
         })


thumbOffsetsAp <-
  sapply( names( thumbOffsetsAp),
         function( cover) {
           fn <- paste( "thumbOffsetsAp",
                       cover, "agg.tif", sep= "_")
           print( paste( cover, fn))
           if( overwriteRasters || !( file.access( fn) ==0))
             aggregate( thumbOffsetsAp[[ cover]],
                       fact= 5/60 /res( thumbOffsetsAp[[ cover]]),
                       expand= FALSE,
                       filename= fn,
                       datatype= "FLT4S",
                       overwrite= TRUE,
                       progress= "text")
           else brick( fn)
         })

thumbOffsetsAp <-
  sapply( thumbOffsetsAp,
         function( r) {
           layerNames( r) <- names( peelClasses)
           r
         })


thumbOffsetsApTotal <-
  writeRaster( thumbOffsetsAp$water +
              thumbOffsetsAp$wetland +
              thumbOffsetsAp$urban,
              filename= "thumbOffsetsAp_total.tif",
              overwrite=TRUE)


thumbOffsetsApTotal <-
  do.call( overlay,
          c( unlist( thumbOffsetsAp, use.names= FALSE),
               fun= sum,
               filename= "thumbOffsetsAp_total.tif",
               overwrite= TRUE,
               progress= "text"))

thumbAdj <- thumb
thumbAdj$agg <- 
  overlay( thumbAdj$agg, thumbOffsetsApTotal,
          fun= sum,
          filename= "thumbAdj.tif",
          overwrite= TRUE)

                  
thumbAdj  <- decomposeMosaic( thumbAdj, overwrite= overwriteRasters, progress= "text")


           


###################################################
### chunk number 31: nlcd-aggr eval=FALSE
###################################################
## #line 1208 "/home/nbest/thesis/datasets.Rnw"
## 
## overwriteRasters <- FALSE
## overwriteFigures <- FALSE
## 
## 
## setwd( nlcdWd)
## 
## nlcd <- mlctReclass( nlcd, nlcdReclassMatrix, overwrite= overwriteRasters, progress="text")
## nlcd$Amin <- 1
## nlcd <- coverFractions( nlcd, mosaic= FALSE, overwrite= overwriteRasters, progress="text")
## nlcd <- aggregateFractions( nlcd, overwrite= overwriteRasters, progress="text")
## 


###################################################
### chunk number 32: cdl eval=FALSE
###################################################
## #line 1241 "/home/nbest/thesis/datasets.Rnw"
## 
## 
## cdl <- list( pri= raster( "/gpfs/pads/projects/see/data/raw/cdl/vrt/cdl_2001.vrt"))
## 
## cdlReclassMatrix <- 
##   matrix( c( 83,  83,  0,               # water
##              85,  85,  0,
##              63,  63,  1,               # forest
##                                         # no shrub
##              62,  62,  3,               # open
##              88,  88,  3,
##              87,  87,  4,               # wetland
##               1,  61,  5,               # crop
##              90,  90,  5,
##              82,  82,  6,               # urban
##              84,  84,  6,
##              86,  86,  6),
##                                         # no mosaic
##                                         # no barren
##          ncol= 3, byrow= TRUE)
## 
## cdl  <- mlctReclass( cdl, cdlReclassMatrix, overwrite= overwriteRasters, progress= "text")
## 
## cdl$Amin <- 1
## cdl <- coverFractions( cdl, mosaic= FALSE, overwrite= TRUE, progress="text")
## cdl <- aggregateFractions( cdl, overwrite=TRUE, progress="text")
## 
## 
## cdl_il <- list( pri= raster( "cdl_il_2001.tif"))
## cdl_il <- mlctReclass( cdl_il, cdlReclassMatrix, overwrite= overwriteRasters, progress= "text")
## cdl_il$Amin <- 1
## cdl_il <- coverFractions( cdl_il, mosaic= FALSE, overwrite= TRUE, progress="text")
## cdl_il <- aggregateFractions( cdl_il, overwrite=TRUE, progress="text")
## 


