###################################################
### chunk number 1: initialize
###################################################
#line 15 "/home/nbest/thesis/datasets.Rnw"

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
#line 98 "/home/nbest/thesis/datasets.Rnw"

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
#line 187 "/home/nbest/thesis/datasets.Rnw"


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
#line 209 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_pri_reclass.png",
            plot= thumbPlots$pri)
}



###################################################
### chunk number 5: fig_thumb_sec_reclass
###################################################
#line 237 "/home/nbest/thesis/datasets.Rnw"


if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_sec_reclass.png",
         plot= thumbPlots$sec)
}



###################################################
### chunk number 6: fig_thumb_pct
###################################################
#line 269 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_pct.png",
            plot= thumbPlots$pct)
}



###################################################
### chunk number 7: fig_thumb_pri_facet
###################################################
#line 307 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_pri_facet.png",
            plot= thumbPlots$pri + 
              facet_wrap(~ values) +
              opts( legend.position= "none"))
}



###################################################
### chunk number 8: fig_thumb_sec_facet
###################################################
#line 328 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_sec_facet.png",
            plot= thumbPlots$sec + 
              facet_wrap(~ values) +
              opts( legend.position= "none"))
}



###################################################
### chunk number 9: mlct_reclass
###################################################
#line 369 "/home/nbest/thesis/datasets.Rnw"

## repeat for cUSA
setwd( dataWd)
mlct <- mlctList( "2001_lct1.tif", 
                  "2001_lct1_sec.tif", 
                  "2001_lct1_pct.tif")
mlct  <- mlctReclass( mlct, mlctReclassMatrix, overwrite= overwriteRasters, datatype="INT1U", progress="text")

if( overwriteFigures) {
  mlctPlots <- list( pri= peelMap( mlct$pri, 8e-4),
                    sec= peelMap( mlct$sec, 8e-4))
  mlctPlots$pct <- ggplotRaster( mlct$pct, 8e-4) + 
    scale_fill_gradientn( "% confidence",
                         colours= rev( brewer.pal( 7, "YlGn")), 
                         limits= c( 100, 0),
                         breaks= seq( 100, 0, by= -20))
}




###################################################
### chunk number 10: fig_mlct_pri_reclass
###################################################
#line 395 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_pri_reclass.png",
            plot= mlctPlots$pri)
  system( sprintf( "convert -trim %s/fig_mlct_pri_reclass.png %s/fig_mlct_pri_reclass_trim.png", texWd, texWd))
}


###################################################
### chunk number 11: fig_mlct_sec_reclass
###################################################
#line 415 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_sec_reclass.png",
            plot= mlctPlots$sec)
  system( sprintf( "convert -trim %s/fig_mlct_sec_reclass.png %s/fig_mlct_sec_reclass_trim.png", texWd, texWd))
}



###################################################
### chunk number 12: fig_mlct_pct
###################################################
#line 435 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_pct.png",
           plot= mlctPlots$pct)
  system( sprintf( "convert -trim %s/fig_mlct_pc.png %s/fig_mlct_pct_trim.png", texWd, texWd))
}



###################################################
### chunk number 13: fig_mlct_pri_facet
###################################################
#line 455 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_pri_facet.png",
         plot= mlctPlots$pri +
           facet_grid( values ~ .),
         width=4.5, height=8)
}



###################################################
### chunk number 14: fig_mlct_sec_facet
###################################################
#line 477 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_mlct_sec_facet.png",
            plot= mlctPlots$sec +
              facet_grid( values ~ .),
            width=4.5, height=8)
}



###################################################
### chunk number 15: thumbPlots
###################################################
#line 551 "/home/nbest/thesis/datasets.Rnw"


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
### chunk number 16: fig_thumb_fracs
###################################################
#line 595 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_fracs.png",
            plot= thumbPlots$fracs)
}



###################################################
### chunk number 17: fig_thumb1_fracs
###################################################
#line 620 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb1_fracs.png",
            plot= thumbPlots1$fracs)
}



###################################################
### chunk number 18: fig_thumb_agg
###################################################
#line 653 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_agg.png",
            plot= thumbPlots$agg)
}



###################################################
### chunk number 19: fig_thumb1_agg
###################################################
#line 671 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb1_agg.png",
            plot= thumbPlots1$agg)
}



###################################################
### chunk number 20: thumbAggDiff
###################################################
#line 699 "/home/nbest/thesis/datasets.Rnw"

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
### chunk number 21: fig_thumb_agg_diff
###################################################
#line 737 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_agg_diff.png",
            plot= thumbAggDiffPlot)
}



###################################################
### chunk number 22: mlct_agg
###################################################
#line 768 "/home/nbest/thesis/datasets.Rnw"

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
### chunk number 23: thumbNomos
###################################################
#line 842 "/home/nbest/thesis/datasets.Rnw"

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
### chunk number 24: fig_thumb_nomos
###################################################
#line 884 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nomos.png",
            plot= thumbPlots$nomos)
}



###################################################
### chunk number 25: fig_thumb1_nomos
###################################################
#line 903 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb1_nomos.png",
            plot= thumbPlots1$nomos)
}



###################################################
### chunk number 26: fig_thumb_nomos_diff
###################################################
#line 924 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures ) {
  my.ggsave( texWd, "fig_thumb_nomos_diff.png",
         plot= thumbNomosDiffPlot)
}



###################################################
### chunk number 27: mlct_nomos
###################################################
#line 942 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)

mlct  <- decomposeMosaic( mlct, overwrite= overwriteRasters, progress="text")

mlct1  <- decomposeMosaic( mlct1, overwrite= overwriteRasters, progress="text")

## might be useful to cross-tabulate the primary and secondary
## frequencies for the cUSA

## table(thumbDf@data$pri, thumbDf@data$sec)



###################################################
### chunk number 28: thumb_nlcd
###################################################
#line 1018 "/home/nbest/thesis/datasets.Rnw"

setwd( dataWd)
setwd( "nlcd")
nlcdWd <- getwd()

thumbNlcd <- list( pri=raster( "thumbNlcd.tif"))
## thumbNlcd <- sapply( thumbNlcd, setMinMax)



###################################################
### chunk number 29: thumb_nlcd_reclass
###################################################
#line 1082 "/home/nbest/thesis/datasets.Rnw"

setwd( nlcdWd)


thumbNlcd <- mlctReclass( thumbNlcd, nlcdReclassMatrix,
                         overwrite= overwriteRasters,
                         progress="text")

if( overwriteFigures) {
  thumbNlcdPlot <- peelMap(thumbNlcd$pri, 0.05)
}



###################################################
### chunk number 30: fig_thumb_nlcd_reclass
###################################################
#line 1101 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nlcd_reclass.png",
            plot= thumbNlcdPlot)
}



###################################################
### chunk number 31: fig_thumb_nlcd_facet
###################################################
#line 1119 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nlcd_facet.png",
            plot= thumbNlcdPlot + 
              facet_wrap(~ values) + 
              opts( legend.position= "none"))
}



###################################################
### chunk number 32: thumb_nlcd_aggr
###################################################
#line 1147 "/home/nbest/thesis/datasets.Rnw"

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
### chunk number 33: fig_thumb_nlcd_agg
###################################################
#line 1170 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_nlcd_agg.png",
            plot= thumbNlcdAggPlot)
}



###################################################
### chunk number 34: nlcd
###################################################
#line 1189 "/home/nbest/thesis/datasets.Rnw"

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
### chunk number 35: fig_nlcd
###################################################
#line 1211 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  nlcdPlot <- coverMaps( nlcd, 0.1) +
    facet_grid( variable ~ .)
  my.ggsave( texWd, "fig_nlcd.png", width=5.5, height=8)
}



###################################################
### chunk number 36: cdl eval=FALSE
###################################################
## #line 1251 "/home/nbest/thesis/datasets.Rnw"
## 
## 
## cdl <- list( pri= raster( "/gpfs/pads/projects/see/data/raw/cdl/vrt/cdl_2001.vrt"))
## 
## cdlReclassMatrix <- 
##   matrix( c(
##             83,  83,  0,               # water
##             85,  85,  0,
##             63,  63,  1,               # forest
##                                        # no shrub
##             62,  62,  3,               # open
##             88,  88,  3,
##             87,  87,  4,               # wetland
##              1,  61,  5,               # crop
##             90,  90,  5,
##             82,  82,  6,               # urban
##             84,  84,  6,
##             86,  86,  6),
##                                        # no mosaic
##                                        # no barren
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


###################################################
### chunk number 37: agland
###################################################
#line 1329 "/home/nbest/thesis/datasets.Rnw"

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
### chunk number 38: fig_thumb_agland
###################################################
#line 1383 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_thumb_agland.png",
            plot= thumbAglandPlot)
}



###################################################
### chunk number 39: fig_agland
###################################################
#line 1402 "/home/nbest/thesis/datasets.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_agland.png",
         plot= aglandPlot)
  system( sprintf( "convert -trim %s/fig_agland.png %s/fig_agland_trim.png",
                  texWd, texWd))
}



###################################################
### chunk number 40: 175crops eval=FALSE
###################################################
## #line 1444 "/home/nbest/thesis/datasets.Rnw"
## 
## cropsWd <- path.expand( "~/see/data/raw/175crops2000/nc")
## list.files( cropsWd, "vrt$")
## 
## cropTable <- read.csv( "/home/nbest/see/data/raw/175crops2000/monfreda2008 table1.csv", header= TRUE)
## 
## ## For now we consider only herbaceous crops
## ## assume that forage crops will be classified "open"
## 
## herbNotForage <- cropTable$type=="herbaceous" & cropTable$group != "Forage"
## 
## 
## cropTable$cat <- NA
## cropTable <- within( cropTable, {
##   cat[ map == "maize"] <- "maize"
##   cat[ map == "soybean"] <- "soybean"
##   cat[ map == "wheat"] <- "wheat"
##   cat[ map == "rice"] <- "rice"
##   cat[ group == "Cereals" & is.na( cat)] <- "cereals"
##   cat[ map == "sugarcane"] <- "sugarcane"
##   cat[ type == "herbaceous" & group == "Forage"] <- "forage"
##   cat[ type == "herbaceous" & is.na( cat)] <- "field_crop"
##   cat[ type == "shrub"] <- "shrub_crop"
##   cat[ type == "tree"] <- "tree_crop"
## })
## 
## 
## catLists <- dlply( cropTable, .(cat), function( row) row$map)
## 
## mapNcName <- function( map) {
##   paste( cropsWd,
##         paste( map, "5min.vrt",
##               sep="_"),
##         sep="/")
## }
## 
## catStacks <- llply( catLists, function( maps) {
##   if( length( maps) ==1) {
##     subset( brick( mapNcName( maps[ 1])), 1)
##   } else {
##     do.call( stack, llply( maps, function( map) {
##       subset( brick( mapNcName( map)), 1)
##     }))
##   }})
## 
## cusaMask <- raster( list.files( getwd(),
##                                "mask_cusa.tif",
##                                recursive= TRUE))
## cusaExtent <- extent( cusaMask)
## 
## catCropped <- llply( names( catStacks), function( c) {
##   fn <- paste( c, "crop.tif", sep="_")
##   if( overwriteRasters) {
##     crop( catStacks[[ c]], cusaExtent,
##          filename= fn,
##          overwrite= TRUE)
##   } else brick( list.files( getwd(), fn, full.names=TRUE))
## })
## names( catCropped) <- names( catStacks)
## 
## catMasked <- llply( names( catCropped), function( c) {
##   r <- if( nlayers( catCropped[[ c]]) ==1) {
##     catCropped[[ c]]
##   } else overlay( catCropped[[ c]], fun= sum)
##   mask( r, cusaMask,
##        filename= paste( c, "tif", sep="."),
##        overwrite= TRUE)
## })
## names( catMasked) <- names( catStacks)
## 
## 


###################################################
### chunk number 41: fig_crops eval=FALSE
###################################################
## #line 1521 "/home/nbest/thesis/datasets.Rnw"
## 
## if( overwriteFigures) {
##   cropsMap <-
##     coverMaps( stack( catMasked), 0.2) +
##     facet_grid( variable ~ .)
##   my.ggsave( texWd, "fig_crops.png",
##             width=5.5, height=8)
## }
## setwd( dataWd)
## 
## 


