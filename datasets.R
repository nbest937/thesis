###################################################
### chunk number 1: initialize
###################################################
#line 16 "/home/nbest/thesis/datasets.Rnw"

                                        # load helper functions
                                        # code will appear in appendix
source("~/thesis/code/peel.R")
source("~/thesis/code/maps.R")
setwd( "~/thesis/datasets")

overwriteRasters <- FALSE
overwriteFigures <- TRUE



###################################################
### chunk number 2: thumb
###################################################
#line 101 "/home/nbest/thesis/datasets.Rnw"

texWd <- setwd("../data")
dataWd <- getwd()


## this works but it's slow
##
## thumb <- crop( raster("2001_lct1.tif"), 
##                extent(-83.5, -(82+25/60), 42+55/60, 44+5/60))
##

## these are subsets exported from GRASS

thumb <- mlctList( "thumb_2001_lct1.tif", 
                   "thumb_2001_lct1_sec.tif", 
                   "thumb_2001_lct1_pct.tif")

igbpLegend <- thumb$pri@legend@colortable
igbpLegend <- igbpLegend[ igbpLegend != "#000000"]

## just in case, save these for later
##  paste( deparse( igbpLegend), collapse="")

## igbpLegend <- c("#2041B3",
##                 "#006A0F",
##                 "#007C25",
##                 "#00A25B",
##                 "#00A125",
##                 "#069228",
##                 "#9E9668",
##                 "#C1C48F",
##                 "#85AA5B",
##                 "#B1B741",
##                 "#A4D07E",
##                 "#73ABAE",
##                 "#CCD253",
##                 "#D90000",
##                 "#9DE36E",
##                 "#B6B5C2",
##                 "#949494")



###################################################
### chunk number 3: mlct-reclass
###################################################
#line 201 "/home/nbest/thesis/datasets.Rnw"


thumb <- mlctReclass( thumb, mlctReclassMatrix, overwrite= overwriteRasters)

if( overwriteFigures) {
  thumbPlots <- list( pri= peelMap( thumb$pri, 0.4),
                     sec= peelMap( thumb$sec, 0.4))
  thumbPlots$pct <- ggplotRaster( thumb$pct, 0.4) + 
    scale_fill_gradientn( "% conf", 
                         colours= rev( brewer.pal( 7, "YlGn")), 
                         limits= c( 100, 0),
                         breaks= seq( 100, 0, by= -20))
}



###################################################
### chunk number 4: fig_thumb_pri_reclass
###################################################
#line 223 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_pri_reclass.png",
            plot= thumbPlots$pri)
}



###################################################
### chunk number 5: fig_thumb_sec_reclass
###################################################
#line 249 "/home/nbest/thesis/datasets.Rnw"


if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_sec_reclass.png",
         plot= thumbPlots$sec)
}



###################################################
### chunk number 6: fig_thumb_pct
###################################################
#line 281 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_pct.png",
            plot= thumbPlots$pct)
}



###################################################
### chunk number 7: fig_thumb_pri_facet
###################################################
#line 319 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_pri_facet.png",
            plot= thumbPlots$pri + 
              facet_wrap(~ values) +
              opts( legend.position= "none"))
}



###################################################
### chunk number 8: fig_thumb_sec_facet
###################################################
#line 340 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_sec_facet.png",
            plot= thumbPlots$sec + 
              facet_wrap(~ values) +
              opts( legend.position= "none"))
}



###################################################
### chunk number 9: mlct_reclass
###################################################
#line 381 "/home/nbest/thesis/datasets.Rnw"

## repeat for cUSA
setwd( dataWd)
mlct <- mlctList( "2001_lct1.tif", 
                  "2001_lct1_sec.tif", 
                  "2001_lct1_pct.tif")
mlct  <- mlctReclass( mlct, mlctReclassMatrix, overwrite= overwriteRasters, datatype="INT1U", progress="text")

if( overwriteFigures) {
  mlctPlots <- list( pri= peelMap( mlct$pri, 16e-4),
                    sec= peelMap( mlct$sec, 16e-4))
  mlctPlots$pct <- ggplotRaster( mlct$pct, 16e-4) + 
    scale_fill_gradientn( "% conf",
                         colours= rev( brewer.pal( 7, "YlGn")), 
                         limits= c( 100, 0),
                         breaks= seq( 100, 0, by= -20))
}




###################################################
### chunk number 10: fig_mlct_pri_reclass
###################################################
#line 407 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_pri_reclass.png",
            plot= mlctPlots$pri, width=7.5)
  system( sprintf( "convert -trim %s/fig_mlct_pri_reclass.png %s/fig_mlct_pri_reclass_trim.png",
                  texWd, texWd))
}


###################################################
### chunk number 11: fig_mlct_sec_reclass
###################################################
#line 427 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_sec_reclass.png",
            plot= mlctPlots$sec, width=7.5)
  system( sprintf( "convert -trim %s/fig_mlct_sec_reclass.png %s/fig_mlct_sec_reclass_trim.png",
                  texWd, texWd))
}



###################################################
### chunk number 12: fig_mlct_pct
###################################################
#line 447 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_pct.png",
           plot= mlctPlots$pct, width=7.5)
  system( sprintf( "convert -trim %s/fig_mlct_pct.png %s/fig_mlct_pct_trim.png",
                  texWd, texWd))
}



###################################################
### chunk number 13: fig_mlct_pri_facet
###################################################
#line 467 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_pri_facet.png",
         plot= peelMap( mlct$pri, 16e-4,
                       classes= names( peelClasses)[1:5]) +
           facet_grid( values ~ .) +
           opts( legend.position= "none"),
         width=4.5, height=8)
}



###################################################
### chunk number 14: fig_mlct_pri_facet2
###################################################
#line 488 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_pri_facet2.png",
         plot= peelMap( mlct$pri, 16e-4,
                       classes= names( peelClasses)[6:9]) +
           facet_grid( values ~ .) +
           opts( legend.position= "none"),
         width=4.5, height=8)
}



###################################################
### chunk number 15: fig_mlct_sec_facet
###################################################
#line 510 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_sec_facet.png",
            plot= peelMap( mlct$sec, 16e-4,
                       classes= names( peelClasses)[1:5]) +
              facet_grid( values ~ .) +
              opts( legend.position= "none"),
            width=4.5, height=8)
}



###################################################
### chunk number 16: fig_mlct_sec_facet2
###################################################
#line 531 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_sec_facet2.png",
            plot= peelMap( mlct$sec, 8e-3,
                       classes= names( peelClasses)[6:9]) +
              facet_grid( values ~ .) +
              opts( legend.position= "none"),
            width=4.5, height=8)
}



###################################################
### chunk number 17: thumbPlots
###################################################
#line 607 "/home/nbest/thesis/datasets.Rnw"


setwd( dataWd)

## calculate cover fractions and aggregate for detail area

thumb  <- primaryFraction( thumb, Amin=0.5,
            overwrite= overwriteRasters, progress= "text")
thumb1 <- primaryFraction( thumb, Amin=1.0,
            overwrite= overwriteRasters, progress= "text")
thumb  <- coverFractions( thumb,
            overwrite= overwriteRasters, progress= "text")
thumb1 <- coverFractions( thumb1,
            overwrite= overwriteRasters, progress= "text")
thumb  <- aggregateFractions( thumb,
            overwrite= overwriteRasters, progress= "text")
thumb1 <- aggregateFractions( thumb1,
            overwrite= overwriteRasters, progress= "text")


if( overwriteFigures) {
  thumbPlots <- list( fracs= coverMaps( thumb$fracs, 0.4),
                     agg= coverMaps( thumb$agg, 1))
  thumbPlots1 <- list( fracs= coverMaps( thumb1$fracs, 0.4),
                      agg= coverMaps( thumb1$agg, 1))
}



###################################################
### chunk number 18: fig_thumb_fracs
###################################################
#line 651 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_fracs.png",
            plot= thumbPlots$fracs)
}



###################################################
### chunk number 19: fig_thumb1_fracs
###################################################
#line 676 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb1_fracs.png",
            plot= thumbPlots1$fracs)
}



###################################################
### chunk number 20: fig_thumb_agg
###################################################
#line 709 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_agg.png",
            plot= thumbPlots$agg)
}



###################################################
### chunk number 21: fig_thumb1_agg
###################################################
#line 727 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb1_agg.png",
            plot= thumbPlots1$agg)
}



###################################################
### chunk number 22: thumbAggDiff
###################################################
#line 755 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

thumbAggDiff <- 
  if( overwriteRasters) {
    overlay( thumb$agg, thumb1$agg, 
            fun= function( t, t1) t -t1, 
            filename= "thumb_agg_diff.tif", 
            overwrite= TRUE)
  } else brick( "thumb_agg_diff.tif")
layerNames( thumbAggDiff) <- layerNames( thumb$agg)

if( overwriteFigures) {
  thumbAggDiffPlot <- coverMaps( thumbAggDiff) + 
    scale_fill_gradientn( "diff", colours= rev( brewer.pal( 11, "BrBG")), 
                         limits= c( 0.1, -0.1),
                         breaks= seq( 0.1, -0.1, by= -0.02))
}



###################################################
### chunk number 23: fig_thumb_agg_diff
###################################################
#line 793 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_agg_diff.png",
            plot= thumbAggDiffPlot)
}



###################################################
### chunk number 24: mlct_agg
###################################################
#line 824 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

mlct  <- primaryFraction( mlct, Amin= 0.5, 
           overwrite= overwriteRasters,
           progress="text")
mlct  <- coverFractions( mlct,
           overwrite= overwriteRasters,
           progress="text")
mlct  <- aggregateFractions( mlct,
           overwrite= overwriteRasters,
           progress="text")

mlct1 <- primaryFraction( mlct, Amin=1.0,
           overwrite= overwriteRasters,
           progress="text")
mlct1 <- coverFractions( mlct1,
          overwrite= overwriteRasters,
          progress="text")
mlct1 <- aggregateFractions( mlct1,
          overwrite= overwriteRasters,
          progress="text")



###################################################
### chunk number 25: thumbNomos
###################################################
#line 898 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)
thumb  <- decomposeMosaic( thumb, overwrite= overwriteRasters, progress= "text")
thumb1 <- decomposeMosaic( thumb1, overwrite= overwriteRasters, progress= "text")

## thumb$nomos <- brick( paste( getwd(), filename( thumb$nomos), sep="/"))
## thumb1$nomos <- brick( paste( getwd(), filename( thumb1$nomos), sep="/"))
## thumb$delta <- brick( paste( getwd(), filename( thumb$delta), sep="/"))
## thumb1$delta <- brick( paste( getwd(), filename( thumb1$delta), sep="/"))

if( overwriteFigures) {
  thumbPlots$nomos <- coverMaps( thumb$nomos, 1)
  thumbPlots1$nomos <- coverMaps( thumb1$nomos, 1)  
}

thumbNomosDiff <- 
  if( overwriteRasters) {
    overlay( thumb$agg, thumb1$agg, 
            fun= function( t, t1) t -t1, 
            filename= "thumb_nomos_diff.tif", 
            overwrite= TRUE)
  } else brick( "thumb_nomos_diff.tif")
layerNames( thumbNomosDiff) <- layerNames( thumb$agg)

if( overwriteFigures) {
  thumbNomosDiffPlot <- coverMaps( thumbNomosDiff) + 
    scale_fill_gradientn( "diff", colours= rev( brewer.pal( 11, "BrBG")), 
                         limits= c( 0.1, -0.1),
                         breaks= seq( 0.1, -0.1, by= -0.02))
                         ## limits= c( 0.32, -0.32),
                         ## breaks= { b <- c( 0.01, 0.02, 0.04,
                         ##                   0.08, 0.16, 0.32)
                         ##           c( rev( b), 0, -b)
                         ##         })
}



###################################################
### chunk number 26: fig_thumb_nomos
###################################################
#line 940 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nomos.png",
            plot= thumbPlots$nomos)
}



###################################################
### chunk number 27: fig_thumb1_nomos
###################################################
#line 959 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb1_nomos.png",
            plot= thumbPlots1$nomos)
}



###################################################
### chunk number 28: fig_thumb_nomos_diff
###################################################
#line 980 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures ) {
  my.ggsave( texWd, "fig_thumb_nomos_diff.png",
         plot= thumbNomosDiffPlot)
}



###################################################
### chunk number 29: mlct_nomos
###################################################
#line 998 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

mlct  <- decomposeMosaic( mlct, overwrite= overwriteRasters, progress="text")

mlct1  <- decomposeMosaic( mlct1, overwrite= overwriteRasters, progress="text")

## might be useful to cross-tabulate the primary and secondary
## frequencies for the cUSA

## table(thumbDf@data$pri, thumbDf@data$sec)



###################################################
### chunk number 30: thumb_nlcd
###################################################
#line 1071 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)
setwd( "nlcd")
nlcdWd <- getwd()

thumbNlcd <- list( pri=raster( "thumbNlcd.tif"))
## thumbNlcd <- sapply( thumbNlcd, setMinMax)



###################################################
### chunk number 31: thumb_nlcd_reclass
###################################################
#line 1149 "/home/nbest/thesis/datasets.Rnw"

setwd( nlcdWd)


thumbNlcd <- mlctReclass( thumbNlcd, nlcdReclassMatrix,
                         overwrite= overwriteRasters,
                         progress="text")

if( overwriteFigures) {
  thumbNlcdPlot <- peelMap(thumbNlcd$pri, 0.1)
}



###################################################
### chunk number 32: fig_thumb_nlcd_reclass
###################################################
#line 1168 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nlcd_reclass.png",
            plot= thumbNlcdPlot, height= 5, width= 5)
}



###################################################
### chunk number 33: fig_thumb_nlcd_facet
###################################################
#line 1186 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nlcd_facet.png",
            plot= thumbNlcdPlot + 
              facet_wrap(~ values) + 
              opts( legend.position= "none"))
}



###################################################
### chunk number 34: thumb_nlcd_aggr
###################################################
#line 1214 "/home/nbest/thesis/datasets.Rnw"

setwd( nlcdWd)
## overwriteRasters <- TRUE
thumbNlcd$Amin <- 1
thumbNlcd <-
  coverFractions( thumbNlcd, mosaic=FALSE,
                 overwrite= overwriteRasters,
                 progress= "text")
thumbNlcd <-
  aggregateFractions( thumbNlcd,
                     overwrite= overwriteRasters,
                     progress="text")

if( overwriteFigures) {
  thumbNlcdAggPlot <- coverMaps( thumbNlcd$agg, 1)
}



###################################################
### chunk number 35: fig_thumb_nlcd_agg
###################################################
#line 1237 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nlcd_agg.png",
            plot= thumbNlcdAggPlot)
}



###################################################
### chunk number 36: nlcd
###################################################
#line 1255 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

## nlcd <- stack( paste( "nlcd", names( peelClasses[ -8]), "5min.tif", sep="_"))

nlcd <- stack( sapply( names( peelClasses[ -8]),
                      function( cover) {
                        list.files( paste( dataWd, "nlcd", sep="/"),
                                   patt= paste( "nlcd", cover, "5min.tif$", sep="_"),
                                   full.names= TRUE)
                      }))
nlcd <- setMinMax( nlcd)
layerNames(nlcd) <- names( peelClasses[ -8])





###################################################
### chunk number 37: fig_nlcd
###################################################
#line 1277 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  nlcdPlot <- coverMaps( nlcd, samp= 0.2,
                        classes= layerNames( nlcd)[ 1:4]) +
    facet_grid( variable ~ .)
  my.ggsave( texWd, "fig_nlcd.png", width=5.5, height=8)
}



###################################################
### chunk number 38: fig_nlcd2
###################################################
#line 1295 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  nlcdPlot2 <- coverMaps( nlcd, 0.2,
                        classes= layerNames( nlcd)[ 5:8]) +
    facet_grid( variable ~ .)
  my.ggsave( texWd, "fig_nlcd2.png", width=5.5, height=8)
}



###################################################
### chunk number 39: agland
###################################################
#line 1343 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)  
setwd( "agland")

agland <- stack( list.files( patt="(cropland|pasture).tif$"))
layerNames(agland) <- c("crop", "open")
agland <- setMinMax( agland)
  
thumbAgland <-
  if( overwriteRasters) {
    crop( agland,
         extent(-83.5, -(82+25/60),
                42+55/60, 44+5/60),
         filename= "thumbAgland.tif",
         progress="text",
         overwrite= overwriteRasters)
  } else brick( list.files( getwd(),
                           "thumbAgland.tif",
                           full.names= TRUE,
                           recursive= TRUE))
layerNames( thumbAgland) <- c("crop", "open")
                                        # crop() returns a brick

## overwriteFigures <- TRUE

if( overwriteFigures) {
  thumbAglandPlot <-
    coverMaps( thumbAgland, 1) +
      facet_grid( variable ~ .)

  aglandPlot <-
    coverMaps( agland, 0.4) +
      facet_grid( variable ~ .)

}

##     sapply( layerNames( thumbAgland),
##            function( cover) {
##              ggplotRaster( agland[[ cover]]) + 
##                scale_fill_gradientn( paste("%", cover),
##                                     colours=rev( brewer.pal( 7, "YlGn")), 
##                                     limits= c( 100, 0),
##                                     breaks= seq( 100, 0, by= -20))
##            })
## }




###################################################
### chunk number 40: fig_thumb_agland
###################################################
#line 1397 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_agland.png",
            plot= thumbAglandPlot)
}



###################################################
### chunk number 41: fig_agland
###################################################
#line 1416 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_agland.png",
         plot= aglandPlot)
  system( sprintf( "convert -trim %s/fig_agland.png %s/fig_agland_trim.png",
                  texWd, texWd))
}



###################################################
### chunk number 42: 175crops
###################################################
#line 1459 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)  

cropsWd <- path.expand( "~/see/data/raw/175crops2000/nc")
## list.files( cropsWd, "vrt$")

cropTable <- read.csv( "/home/nbest/see/data/raw/175crops2000/monfreda2008 table1.csv", header= TRUE)

## For now we consider only herbaceous crops


herbNotForage <- cropTable$type=="herbaceous" & cropTable$group != "Forage"


cropTable$cat <- NA
cropTable <- within( cropTable, {
  cat[ map == "maize"] <- "maize"
  cat[ map == "soybean"] <- "soybean"
  cat[ map == "wheat"] <- "wheat"
  cat[ map == "rice"] <- "rice"
  cat[ group == "Cereals" & is.na( cat)] <- "cereals"
  cat[ map == "sugarcane"] <- "sugarcane"
  cat[ type == "herbaceous" & group == "Forage"] <- "forage"
  cat[ type == "herbaceous" & is.na( cat)] <- "field_crop"
  cat[ type == "shrub"] <- "shrub_crop"
  cat[ type == "tree"] <- "tree_crop"
})


catLists <- dlply( cropTable, .(cat), function( row) row$map)

mapNcName <- function( map) {
  paste( cropsWd,
        paste( map, "5min.vrt",
              sep="_"),
        sep="/")
}

catStacks <- llply( catLists, function( maps) {
  if( length( maps) ==1) {
    subset( brick( mapNcName( maps[ 1])), 1)
  } else {
    do.call( stack, llply( maps, function( map) {
      subset( brick( mapNcName( map)), 1)
    }))
  }})


cusaMask <- raster( "mask_cusa.tif")
cusaExtent <- extent( cusaMask)

catCropped <- llply( names( catStacks), function( c) {
  fn <- paste( c, "crop.tif", sep="_")
  if( overwriteRasters) {
    crop( catStacks[[ c]], cusaExtent,
         filename= fn,
         overwrite= TRUE)
  } else brick( list.files( getwd(), fn, full.names=TRUE))
})
names( catCropped) <- names( catStacks)

catMasked <- llply( names( catCropped), function( c) {
  r <- if( nlayers( catCropped[[ c]]) ==1) {
    catCropped[[ c]]
  } else overlay( catCropped[[ c]], fun= sum)
  raster::mask( r, cusaMask,
       filename= paste( c, "tif", sep="."),
       overwrite= TRUE)
})
names( catMasked) <- names( catStacks)




###################################################
### chunk number 43: fig_crops
###################################################
#line 1537 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  cropsMap <-
    coverMaps( stack( catMasked[ 1:5]), 0.4) +
    facet_grid( variable ~ .)
  my.ggsave( texWd, "fig_crops.png",
            width=5.5, height=8)
}




###################################################
### chunk number 44: fig_crops2
###################################################
#line 1559 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  cropsMap2 <-
    coverMaps( stack( catMasked[ 6:10]), 0.4) +
    facet_grid( variable ~ .)
  my.ggsave( texWd, "fig_crops2.png",
            width=5.5, height=8)
}




