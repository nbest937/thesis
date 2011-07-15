###################################################
### chunk number 1: init
###################################################
#line 21 "/home/nbest/thesis/analysis.Rnw"

options( prompt= " ", continue= " ", width= 60)
options(error= function(){
  ## recover()
  options( prompt= "> ", continue= "+ ", width= 80)
})
  
source( "~/thesis/code/analysis.R")
source( "~/thesis/code/peel.R")
source( "~/thesis/code/maps.R")

   texWd <- path.expand( "~/thesis/analysis")
rasterWd <- path.expand( "~/thesis/data/analysis")
dataPath <- path.expand( "~/thesis/data")
setwd( rasterWd)

overwriteRasters <- TRUE
overwriteFigures <- TRUE

                                        # studyArea used to work out RMSE
                                        # calcs and tables
##studyArea <- "thumb"
studyArea <- "mlct"
                                        # bands are numbered from one but
                                        # classes from zero.  Used for stacks/brick
                                        # where bands correspond to classes
peelBands <- peelClasses +1

                                        # mask and agland exported from GRASS
                                        # no need to mask or crop
cusaMask <- raster( sprintf( "%s/mask_cusa.tif",
                            dataPath))
cusaExtent <- extent( cusaMask)
thumbExtent <- extent( -( 83 +30 /60), -( 82 +25 /60),
                          42 +55 /60,     44  +5 /60 )

                                        # default raster() output
                                        # has geographic proj, full extent
                                        # by default
world <- raster()
res(world) <- 5/60

grid <- raster( cusaMask)
grid[] <- cellsFromExtent( world, grid)
grid <- raster::mask( grid, cusaMask)

nulls <- raster( cusaMask)
nulls[] <- NA

zeroes <- raster( cusaMask)
zeroes[] <- 0

ones <- raster( cusaMask)
ones[] <- 1

if( studyArea == "thumb") {
  cusaMask <- crop( cusaMask, thumbExtent)
}
acresFile <- paste( "acres",
                   paste( studyArea, ".tif", sep=""),
                   sep="_")
if( overwriteRasters) {
  acres <- area( cusaMask) *247.105381
  acres <- writeRaster( acres,
                       filename= acresFile,
                       overwrite= TRUE)
} else acres <- raster( acresFile)

agland <- stack( list.files( paste( dataPath, "agland", sep="/"),
                            patt= "(cropland|pasture).tif$",
                            full.names= TRUE))
layerNames(agland) <- c("crop", "open")
agland <- setMinMax( agland)
if( studyArea == "thumb") {
  agland <- crop( agland, thumbExtent)
}

agg05 <-
  brick( list.files( dataPath,
                    patt= paste( studyArea, "_Amin_0.5_agg.tif", sep=""),
                    full.names= TRUE))
layerNames( agg05) <- names( peelClasses)

nomos05 <-
  brick( list.files( dataPath,
                    patt= paste( studyArea, "_Amin_0.5_nomosaic.tif", sep=""),
                    full.names= TRUE))
layerNames( nomos05) <- c( names( peelClasses)[ -8], "total")

agg1 <-
  brick( list.files( dataPath,
                    patt= paste( studyArea, "1_agg.tif", sep=""),
                    full.names= TRUE)) 
layerNames( agg1) <- names( peelClasses)

nomos1 <-
  brick( list.files( dataPath,
                    patt= paste( studyArea, "1_Amin_1_nomosaic.tif", sep=""),
                    full.names= TRUE))
layerNames( nomos1) <- c( names( peelClasses)[ -8], "total")


nlcd <-
  brick( sapply( names( peelClasses),
                function( cover) {
                  if( cover == "mosaic") {
                    zeroes
                  } else {
                    fn <-
                      list.files( paste( dataPath, "nlcd",
                                        sep = "/"),
                                 patt= paste( "nlcd", cover, "5min.tif$",
                                             sep = "_"),
                                 full.names= TRUE)
                    crop( raster( fn), cusaMask)
                  }}))


nlcd <- writeRaster( nlcd,
                    filename= paste( path.expand(rasterWd), "nlcd.tif", sep= "/"),
                    overwrite= TRUE)

layerNames(nlcd) <- names(peelClasses)


rasterNames <- c( "agland", "nlcd", "agg05", "agg1", "nomos05", "nomos1")

dataSets <- sapply( rasterNames, function( n) eval( parse( text=n)))

areas <- llply( dataSets,
function( d) {
  res <- cellStats( d *acres, sum)
  names( res) <- layerNames( d)
  res
})

## llply( areas, function( a) melt( a, value.name= deparse( substitute( a))))

areasDf <-
  ldply( areas, function( a) {
    melt( t( as.data.frame( a)))
  })
areasDf <-
  areasDf[, c( 1, 3, 4)]
colnames( areasDf) <-
  c( "map", "class", "acres")
areasDf$map <-
  factor( areasDf$map,
         levels= rasterNames)

## causes things to be in the wrong order
## areasDf$class <-
##   factor( areasDf$class,
##          levels= c( names( peelClasses), "total"))

if( overwriteFigures) areasPlot <-
  qplot( map, acres /10^6,
        data= subset(areasDf, class != "total"),
        geom="bar", position= "stack",
        fill= class,
        stat="summary", fun.y="sum") +
  scale_fill_manual( "",
                    values= peelLegend, #peelLegend[ levels( areasDf$class)[1:9]],
                    breaks= names( peelClasses)) +
  scale_y_continuous( "Ma",
      limits= c(0,2000)) +
  theme_bw() +
  scale_x_discrete( "",
      limits= rasterNames[ c( 1, 2, 4, 3, 6, 5)],
      breaks= rasterNames[ c( 1, 2, 4, 3, 6, 5)],
      labels= expression("Agland2000", "NLCD",
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==1.0)),
               phantom(0)),
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==0.5)),
               phantom(0)),
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==1.0)),
               "No Mosaic"),
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==0.5)),
               "No Mosaic")))

## labels= c("Agland2000", "NLCD",
      ##   "MLCT\nA_min=1.0", "MLCT\nA_min=0.5",
      ##   "MLCT\nA_min=1.0\nNo Mosaic", "MLCT\nA_min=0.5\nNo Mosaic"))


## covers in columns
## areasCt <- cast( areasDf, .id ~ X2, subset= X2 != "total", sum, margins="grand_col")
## rownames( areasCt) <- areasCt[, ".id"]
## areasCt <- areasCt[, -1]
## areasCt[, c( names( peelClasses), "(all)")]

## covers in rows
areasCt <- cast( areasDf, class ~ map,
                value= "acres",
                subset= class != "total",
                sum,
                margins="grand_row")[, -1]
rownames( areasCt) <- levels( areasDf$class)
#areasCt <- areasCt[, -1]
#areasCt <- areasCt[ c( names( peelClasses), "(all)"), rasterNames]



###################################################
### chunk number 2: tab_areas
###################################################
#line 323 "/home/nbest/thesis/analysis.Rnw"


local({
  colnames( areasCt) <- c( "Agland2000", "NLCD",
                          "\\pbox[c][][c]{3in}{Aggregated\\\\$A_{min}=0.5$}",
                          "\\pbox[c][][c]{3in}{Aggregated\\\\$A_{min}=1.0$}",
                          "\\pbox[c][][c]{3in}{No Mosaic\\\\$A_{min}=0.5$}",
                          "\\smallskip\\pbox[c][][c]{3in}{No Mosaic\\\\$A_{min}=1.0$}")
  print( xtable( areasCt / 10^6, 
                caption= "Total Acreages by Map and Cover", 
                label= "tab:areas",
                digits= 1),
        add.to.row= list( 
          pos= list( 0, nrow( areasCt)),
          command= rep("\\noalign{\\smallskip}", times= 2)),
        size= "small",
        sanitize.colnames.function= function(x) x)
})





###################################################
### chunk number 3: fig_areas
###################################################
#line 350 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {  
  setwd( texWd)
  ggsave( "fig_areas.pdf", plot= areasPlot)
}




###################################################
### chunk number 4: rmse
###################################################
#line 415 "/home/nbest/thesis/analysis.Rnw"


rmseDf <- ldply( list("nomos05", "nomos1"),
                function( brickName) {
                  rmseRast( getPeelBand( get( brickName), "crop"),
                           unstack( agland)[[1]])
                })
rmseDf <- cbind( c( 0.5, 1.0), rmseDf)
colnames( rmseDf) <- c( "$A_{min}$", "RMSE")

cropScatDf <- 
  data.frame( as( stack( getPeelBand( nomos05, "crop"),
                        getPeelBand( nomos1, "crop"),
                        unstack( agland)[[1]],
                        raster::mask(acres, cusaMask)),
                 "SpatialGridDataFrame"))
colnames(cropScatDf) <-
  c( "nomos05", "nomos1", "agland", "acres", "lon", "lat")
cropScatDf$weight <- with( cropScatDf, acres/ max(acres))


if( overwriteFigures) scatPlot1 <-
  ggplot( data=
         cropScatDf, ##[ sample( 1:nrow(cropScatDf),
                     ##       round( nrow(cropScatDf) *0.05)),],
         aes( agland, nomos1,
             size= weight)) +
  geom_point( alpha= 0.03) +
  scale_area( limits= c(0.7, 1),
             breaks= seq( 0.7, by= 0.1),
             to= c(0.7, 1) *6) +
  geom_abline( size= 2,
              alpha= 0.4) +
  theme_bw() +
  scale_x_continuous( "Agland2000",
                     limits= c( 0, 1),
                     expand= c( 0.025, 0)) +
  scale_y_continuous( "MLCT, A_min = 1.0",
                     limits= c( 0, 1),
                     expand= c( 0.025, 0)) +
  coord_equal() +
  opts( legend.position= "none")


if( overwriteFigures) hexPlot1 <-
  ggplot( data= cropScatDf,
       ## with( cropScatDf,
       ##    cropScatDf[ agland > 0.01 | nomos1 > 0.01,]),
       aes( agland, nomos1)) +
  stat_binhex( binwidth= c( 0.025, 0.025)) +
  scale_fill_gradientn( colours= brewer.pal( 6, "YlGn"),
                       trans= "log10",
                       limits=c( 10, 10000)) +
  geom_abline( alpha=0.4) +
  scale_x_continuous( "Agland2000") +
  scale_y_continuous( expression(paste("MLCT, ", A[min] == 1.0))) +
  theme_bw() +
  coord_equal()



###################################################
### chunk number 5: fig_scatplot1
###################################################
#line 481 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_scatPlot1.png",
         plot= scatPlot1)
}



###################################################
### chunk number 6: fig_hexplot1
###################################################
#line 498 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  ggsave( "fig_hexPlot1.pdf",
         plot= hexPlot1)
}



###################################################
### chunk number 7: fig_scatplot05
###################################################
#line 552 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_scatPlot05.png",
         plot= scatPlot1 +
         aes(agland, nomos05, size=weight) +
         scale_y_continuous( "MLCT, A_min = 0.5",
                            limits= c( 0, 1),
                            expand= c( 0.025, 0)))
}





###################################################
### chunk number 8: fig_hexplot05
###################################################
#line 577 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  ggsave( "fig_hexPlot05.pdf",
         plot= hexPlot1 +
         aes(agland, nomos05) +
         scale_y_continuous( expression(paste("MLCT, ", A[min] == 0.5)),
                            limits= c( 0, 1),
                            breaks= seq( 0, 1, by= 0.2)))
  }






###################################################
### chunk number 9: table_rmse
###################################################
#line 600 "/home/nbest/thesis/analysis.Rnw"

print( xtable( rmseDf,
              caption= "RMSE, MLCT vs. Agland2000 crop",
              label= "tab:rmse",
              digits= c( 0, 1, 3)),
      include.rownames= FALSE,
      sanitize.colnames.function= function(x) x)



###################################################
### chunk number 10: offsets_calc
###################################################
#line 689 "/home/nbest/thesis/analysis.Rnw"

nlcdKeep <- stack( llply( names( peelClasses), function( class) {
  if( class %in% c( "water", "wetland", "urban"))
    ones else zeroes
}))

nlcdIgnore <- stack( llply( names( peelClasses), function( class) {
  if( class %in% c( "water", "wetland", "urban"))
    zeroes else ones
}))

nlcdKeepOffsets <-
  (nlcd -agg05) *nlcdKeep

mlctKeep <- agg05 *nlcdIgnore

## nlcdIgnoreOffsets <-
##   -1 *mlctKeep /sum( mlctKeep) *sum( nlcdKeepOffsets)

nlcdIgnoreOffsets <-
  overlay( mlctKeep, sum( mlctKeep), sum( nlcdKeepOffsets),
          fun= function( mk, smk, snko) {
            ifelse( mk == 0 & smk ==0,
                   0,
                   -1 *mk /smk *snko)
          })


nlcdOffsets <- nlcdKeepOffsets +nlcdIgnoreOffsets

nlcdOffsets <- 
  writeRaster( nlcdOffsets,
              filename= paste( rasterWd, "nlcdOffsets.tif", sep= "/"),
              overwrite= TRUE)


nlcdOffsets <- stack( nlcdOffsets, sum( nlcdOffsets))
layerNames( nlcdOffsets) <- c( names( peelClasses), "total")

thumbNlcdOffsets <- crop( nlcdOffsets, thumbExtent)

## offsetsMap <- coverMaps( nlcdOffsets, 0.4) +
##   coord_equal() +
##   facet_grid( variable ~ .) +
##   scale_fill_gradientn( "diff", colours= rev( brewer.pal( 11, "BrBG")), 
##                        limits= c( 0.1, -0.1),
##                        breaks= seq( 0.1, -0.1, by= -0.02))

offsetsMap1 <- coverDiffMaps( nlcdOffsets, samp= 0.4,
                             classes= layerNames( nlcdOffsets)[ 1:5]) +
               coord_equal()

offsetsMap2 <- coverDiffMaps( nlcdOffsets, samp= 0.4,
                             classes= layerNames( nlcdOffsets)[ 6:10]) +
               coord_equal()



thumbOffsetsMap <-
  coverDiffMaps( thumbNlcdOffsets,
                classes= layerNames( thumbNlcdOffsets)[-10]) +
  facet_wrap( ~variable)




###################################################
### chunk number 11: fig_offsetsmap1
###################################################
#line 760 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_offsets1.png", plot= offsetsMap1, height= 7)
}



###################################################
### chunk number 12: fig_offsetsmap2
###################################################
#line 779 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_offsets2.png", plot= offsetsMap2, height= 7)
}



###################################################
### chunk number 13: cor_offsets
###################################################
#line 807 "/home/nbest/thesis/analysis.Rnw"

corOffsets <- cor( data.frame(as( nlcdOffsets, "SpatialGridDataFrame"))[, 1:9],
                  use= "complete.obs")
colnames( corOffsets) <- names( peelClasses)
rownames( corOffsets) <- names( peelClasses)

## ord <- order.dendrogram( as.dendrogram( hclust( dist( corOffsets))))

corOffsetsPlot <- 
  ggplot( melt( corOffsets),
         aes( x=X1, y=X2, fill= value)) +
  geom_tile() +
  theme_bw() +
  opts( panel.grid.minor= theme_blank(),
       panel.grid.major= theme_blank(),
       panel.background= theme_blank(),
       axis.title.x= theme_blank(),
       axis.text.x= theme_text( angle= 90, hjust=1),
       axis.title.y= theme_blank()) +
  scale_x_discrete( limits= colnames( corOffsets)) +
  scale_y_discrete( limits= colnames( corOffsets)) +
  scale_fill_gradientn( "", colours= rev( brewer.pal( 11, "BrBG")), 
                       limits= c( 1.0, -1.0),
                       breaks= seq( 1.0, -1.0, by= -0.2))

if( overwriteFigures) {
  oldWd <- setwd( texWd)
  ggsave( "fig_corOffsets.pdf", plot= corOffsetsPlot)
  setwd( oldWd)
}





###################################################
### chunk number 14: cusa_offset
###################################################
#line 889 "/home/nbest/thesis/analysis.Rnw"

setwd( rasterWd)

                                        # reload offsets to get rid
                                        # of total layer
nlcdOffsets <- brick( paste( rasterWd, "nlcdOffsets.tif", sep="/"))
layerNames( nlcdOffsets) <- names( peelClasses)

mlctAdj <- list( Amin=0.5)
mlctAdj$agg <-
  if( overwriteRasters) {
    overlay( agg05, nlcdOffsets,
            fun= sum,
            filename= "agg05Adj.tif",
            overwrite= TRUE)
  } else brick( list.files( rasterWd,
                           patt= "agg05Adj.tif",
                           full.names= TRUE))

layerNames( mlctAdj$agg) <- names( peelClasses)

mlctAdj  <- decomposeMosaic( mlctAdj, overwrite= overwriteRasters, progress= "text")



###################################################
### chunk number 15: areas2
###################################################
#line 919 "/home/nbest/thesis/analysis.Rnw"

# reuse area table code from above; better to implement a function?

rasterNames2 <- c( "agland", "nlcd", "agg05", "nomos05",
                  "nlcdOffsets", "mlctAdj$agg", "mlctAdj$nomos")

dataSets2 <- sapply( rasterNames2,
  function( n) eval( parse( text=n)))

areas2 <- llply( dataSets2,
  function( d) {
    res <- cellStats( d *acres, sum)
    names( res) <- layerNames( d)
    res
  })

areasDf2 <- ldply( areas2, function( a) {
  melt( t( as.data.frame( a)))
})
areasDf2 <-
  areasDf2[, c( 1, 3, 4)]
colnames( areasDf2) <-
  c( "map", "class", "acres")

areasDf2 <-
  transform( areasDf2,
            class= factor( class,
              levels= c("crop", "open",
                names( peelClasses)[-c(4,6,8)],
                "mosaic", "total")),
            map= factor( map,
              levels= rasterNames2))

## areasDf2$map <-
##   factor( areasDf2$map,
##          levels= rasterNames2)

## unused
## areasPlot2 <-
##   qplot( map, acres /10^6,
##         data= subset(areasDf2,
##           class != "total" & map != "offset"),
##         geom="bar", position= "stack",
##         fill= class,
##         stat="summary", fun.y="sum") +
##   scale_fill_manual( "",
##                     values= peelLegend, 
##                     breaks= names( peelClasses)) +
##   scale_y_continuous( "Ma",
##       limits= c(0,2000)) +
##   theme_bw()



## covers in rows

##areasCt2 <- cast( areasDf2, X2 ~ .id, subset= X2 != "total", sum, margins="grand_row")

areasCt2 <- cast( areasDf2,
                 class ~ map,
                 subset= class != "total",
                 value= "acres",
                 sum,
                 margins="grand_row")

rownames( areasCt2) <- areasCt2[, "class"]
areasCt2 <- areasCt2[, -1]
areasCt2 <- areasCt2[ c( names( peelClasses), "(all)"), rasterNames2]



###################################################
### chunk number 16: restack_check
###################################################
#line 992 "/home/nbest/thesis/analysis.Rnw"

## check that everything balances
## output of decomposeMosaic is not brick()ed properly
## in the sense that the layer set is incomplete
## and out of order
  

restack <- function( peelBrick) {
  u <- unstack( peelBrick)
  names( u) <- layerNames( peelBrick)
  r <- do.call( stack,
          llply( names( peelClasses),
                function( cover) {
                  if( is.null( u[[ cover]]))
                    zeroes
                  else
                    u[[ cover]]
                }))
  layerNames( r) <- names( peelClasses)
  r
}
                                        # restack() takes any of the bricks/stacks from
                                        # previous functions and rearranges the layers
                                        # to match the PEEL classes, inserting layers of
                                        # zeroes as needed


## check <- llply( mlctAdj[ c("nomos", "delta", "agg")], restack)
## names(check) <- NULL
## do.call( overlay, c(check , fun=function( n, d, a) n-d-a))

restackOverlay <- function( rasterList, fun) {
  l <- llply( rasterList, restack)
  names( l) <- NULL
  do.call( overlay, c( l, fun=fun))
}
                                        # restackOverlay() runs its arguments through restack()
                                        # and applies a function to its outputs

## restackOverlay( mlctAdj[ c("nomos", "delta", "agg")],
##                function( n, d, a) n-d-a)

## restackOverlay( list( mlctAdj$agg, offset, agg05),
##                function( a2, o, a) a2-o-a)



###################################################
### chunk number 17: table_restack_check eval=FALSE
###################################################
## #line 1045 "/home/nbest/thesis/analysis.Rnw"
## 
## check <- restackOverlay( c( mlctAdj[ c("nomos", "delta")],
##                            nlcdOffsets,
##                            agg05),
##                         function( n, d, o, a) n-d-o-a)
## layerNames(check) <- names( peelClasses)
## 
## checkTable <-
##   xtable( cbind( class=peelClasses,
##                 min=minValue( check),
##                 max=maxValue(check)),
##          caption= "Balance of adjustment fractions and original MLCT aggregation", 
##          label= "tab:restack_check")
## digits( checkTable) <- c( 0, 0,-2,-2)
## print( checkTable)
##   


###################################################
### chunk number 18: table_rmse2
###################################################
#line 1070 "/home/nbest/thesis/analysis.Rnw"

                                        # add the RMSE for the new crop map
                                        # and an indication of the NLCD offsets' presence
  
rmseDf2 <-
  cbind( offset=c( TRUE, FALSE, FALSE),
        rbind( c( 0.5,
                 rmseRast( getPeelBand( mlctAdj$nomos, "crop"),
                          unstack( agland)[[ 1]])),
              rmseDf))

                                        # add the RMSE for the open class
rmseDf2 <-
  cbind( rmseDf2,
        rmseOpen=ldply( list(mlctAdj$nomos, nomos05, nomos1),
                function( brickVar) {
                  rmseRast( getPeelBand( brickVar, "open"),
                           unstack( agland)[[ 2]])
                }))
colnames(rmseDf2)[ c(3,4)] <- c( "$RMSE_{crop}$", "$RMSE_{open}$")

           
print( xtable( rmseDf2,
              caption= "RMSE, MLCT vs. Agland2000 crop with NLCD offsets",
              label= "tab:rmse2",
              digits= c( 0, 0, 1, 3, 3)),
      include.rownames= FALSE,
      sanitize.colnames.function= function(x) x)



###################################################
### chunk number 19: tab_areas2
###################################################
#line 1113 "/home/nbest/thesis/analysis.Rnw"


local({
  colnames( areasCt2) <- c( "Agland2000", "NLCD", "MLCT", 
                           "\\pbox[c][][c]{3in}{MLCT\\\\No Mosaic}",
                           "\\pbox[c][][c]{3in}{NLCD\\\\Offsets}", 
                           "\\pbox[c][][c]{3in}{MLCT\\\\Adjusted}",
                           "\\pbox[c][][c]{3in}{\\smallskip{}MLCT\\\\Adjusted\\\\No Mosaic}")
  print( xtable( areasCt2 / 10^6, 
                caption= "Effect of NLCD offsets on total acreages, $A_{min}=0.5$",
                label= "tab:areas2",
                digits= 1),
        size= "small",
        add.to.row= list( 
          pos= list( 0, nrow( areasCt)),
          command= rep("\\noalign{\\smallskip}", times= 2)),        
        sanitize.colnames.function= function(x) x)
        ##,
        ##        sanitize.text.function= function(x) x))
  ##,
  ##      floating= FALSE)
})

if( overwriteFigures) areasPlotAdj <-
  qplot( map, acres /10^6,
        data= subset( areasDf2,
          class != "total" & map != "nlcdOffsets"),
        geom="bar", position= "stack",
        fill= class,
        stat="summary", fun.y="sum") +
  scale_fill_manual( "",
                    values= peelLegend[ levels( areasDf2$class)[1:9]], #peelLegend, 
                    breaks= names( peelClasses)) +
  scale_y_continuous( "Ma",
      limits= c(0,2000)) +
  theme_bw() +
  scale_x_discrete( "",
      limits= rasterNames2[ rasterNames2 != "nlcdOffsets"],
      breaks= rasterNames2[ rasterNames2 != "nlcdOffsets"],
      labels= expression("Agland2000", "NLCD",
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==0.5)),
               phantom(0)),
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==0.5)),
               "No Mosaic"),
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==0.5)),
               "Adjusted"),
          atop( atop( textstyle( "MLCT"),
                     textstyle( A[ min] ==0.5)),
               scriptstyle( "Adjusted, No Mosaic"))))

      ## labels= c("Agland", "NLCD",
      ##   "MLCT\nA_min=0.5", "MLCT\nA_min=0.5\nNo Mosaic",
      ##   "MLCT\nAdjusted", "MLCT\nAdjusted\nNo Mosaic"))



cropScatAdjDf <- 
  data.frame( as( stack(getPeelBand( mlctAdj$nomos, "crop"),
                        unstack( agland)[[1]],
                        raster::mask(acres, cusaMask)),
                 "SpatialGridDataFrame"))
colnames(cropScatAdjDf) <-
  c( "mlctAdj", "agland", "acres", "lon", "lat")
cropScatAdjDf$weight <- with( cropScatAdjDf, acres/ max(acres))


if( overwriteFigures) scatPlotAdj <-
  ggplot( data=
         cropScatAdjDf, ##[ sample( 1:nrow(cropScatAdjDf),
                       ##         round( nrow(cropScatAdjDf) *0.05)),],
         aes( agland, mlctAdj,
             size= weight)) +
  geom_point( alpha= 0.03) +
  scale_area( limits= c(0.7, 1),
             breaks= seq( 0.7, by= 0.1),
             to= c(0.7, 1) *6) +
  geom_abline( size= 2,
              alpha= 0.4) +
  ##  stat_smooth( method= "lm",
  ##              se= FALSE,
  ##              size= 3,
  ##              alpha= 0.2) +
  theme_bw() +
  scale_x_continuous( "Agland2000",
                     limits= c( 0,1),
                     expand= c( 0.025,0)) +
  scale_y_continuous( "MLCT Adjusted",
                     limits= c( 0,1),
                     expand= c( 0.025,0)) +
  coord_equal() +
  opts( legend.position= "none")





###################################################
### chunk number 20: fig_offsets
###################################################
#line 1217 "/home/nbest/thesis/analysis.Rnw"
 
if( overwriteFigures) {
  offsetsPlot <-
    qplot( class, acres /10^6, 
        data= subset( areasDf2,
          map == "nlcdOffsets" & class != "total"),
        geom= "bar",
        fill= class) +
    scale_fill_manual( "",
        values= peelLegend, 
        breaks= names( peelLegend)) +
    scale_y_continuous( "Ma", limits=c( -50, 80)) +
    scale_x_discrete( "", breaks= c( names( peelClasses), "total")) +
    coord_flip() +
    theme_bw() +
    opts( legend.position= "none")
  setwd( texWd)
  ggsave( "fig_offsets.pdf",
         plot= offsetsPlot)
}





###################################################
### chunk number 21: fig_areasAdj
###################################################
#line 1282 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  ggsave( "fig_areasAdj.pdf", plot= areasPlotAdj)
}



###################################################
### chunk number 22: fig_scatPlotAdj
###################################################
#line 1298 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_scatPlotAdj.png",
         plot= scatPlotAdj)
}



###################################################
### chunk number 23: fig_hexPlotAdj
###################################################
#line 1314 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  ggsave( "fig_hexPlotAdj.pdf",
         plot= hexPlot1 %+% cropScatAdjDf +
              aes( agland, mlctAdj) +
              scale_x_continuous( "Agland2000") +
              scale_y_continuous( "MLCT Adjusted",
                  limits= c( 0, 1),
                  breaks= seq( 0, 1, by= 0.2)) +
              coord_equal()
  )
}



###################################################
### chunk number 24: fusion
###################################################
#line 1339 "/home/nbest/thesis/analysis.Rnw"

## thumbAgland <- crop( agland,
##                     extent(-83.5, -(82+25/60), 42+55/60, 44+5/60),
##                     filename= "thumbAgland.tif",
##                     progress="text")

nomosCrop <- getPeelBand( mlctAdj$nomos, "crop")
aglandCrop <- unstack( agland)[[ 1]]

## nomosTruth is the sum of the classes from the NLCD offsets.
## other classes adjusted from now on cannot exceed $1 - nomosTruth

if( overwriteRasters) {
  nomosTruth <- overlay( getPeelBand( mlctAdj$nomos, "water"),
                        getPeelBand( mlctAdj$nomos, "wetland"),
                        getPeelBand( mlctAdj$nomos, "urban"),
                        fun= sum,
                        filename= "nomosTruth.tif",
                        overwrite= TRUE)
} else nomosTruth <-
  raster( list.files( dataPath,
                    patt="nomosTruth.tif",
                    full.names= TRUE)) 


nomosClasses <- layerNames( mlctAdj$nomos)[ -9]
                                        # leaves out 'total'
                                        # mosaic is already gone

## This is the overlay() bug.  These values of peelCrop should be the same
## but they're not.

## peelCrop <-
##   overlay( aglandCrop, nomosCrop, nomosTruth, fun=
##           function( a, n, t) {
##             ifelse( is.na( a), n, min( a, 1 -t))
##           },
##           filename= "peelCrop.tif",
##           overwrite= TRUE)

peelCrop <-
  if( overwriteRasters) {
    calc( stack( aglandCrop, nomosCrop, nomosTruth), fun=
         function( st) {
           a <- st[ 1]
           n <- st[ 2]
           t <- st[ 3]
           ifelse( is.na( a), n, min( a, 1 -t))
         },
         filename= "peelCrop.tif",
         overwrite= TRUE)
  } else raster( "peelCrop.tif")


offsetStack <-
  stack( llply( nomosClasses,
               function( class) {
                 if( class =="crop")
                   peelCrop
                 else
                   zeroes
               }))


noncropFactor <-
  overlay( peelCrop, nomosCrop, nomosTruth, fun=
          function( p, n, t) {
            ifelse( 1 -n -t <= 0,
                   0,
                   ( 1 -p -t) /( 1 -n -t))
          })


factorStack <- 
  stack( llply( nomosClasses,
               function( class) {
                 if( class == "crop")
                   zeroes
                 else if( class %in%
                         c( "water", "wetland", "urban"))
                   ones
                 else
                   noncropFactor
               }))

## stupid overlay bug!
##
## aglandComplete <-
##   if( overwriteRasters || TRUE) {
##     overlay( stack( unstack( mlctAdj$nomos)[ -9]),
##             factorStack,
##             offsetStack,
##             fun= function( x, m, b) m *x +b,
##             filename= "aglandComplete.tif",
##             overwrite= TRUE,
##             progress= "text")
##   } else brick( list.files( rasterWd,
##                          patt="^aglandComplete.tif$",
##                          full.names=TRUE))
## layerNames( aglandComplete) <- names(peelClasses)[-8]

aglandComplete <-
  stack( unstack( mlctAdj$nomos)[ -9]) *factorStack +offsetStack
aglandComplete <- writeRaster( aglandComplete,
                              "aglandComplete.tif",
                              overwrite= TRUE)
layerNames( aglandComplete) <- names(peelClasses)[-8]

aglandCompleteSum <- sum( aglandComplete)

## lt1AgcTotal <- extract( stack( peelCrop,
##                                nomosCrop,
##                                nomosTruth,
##                                noncropFactor,
##                                aglandComplete,
##                                aglandCompleteSum),
##                        which( aglandCompleteSum[] < 0.999))

## colnames( lt1AgcTotal)[ 1:4] <- c( "p", "n", "t", "factor")
## lt1AgcTotal <- data.frame( lt1AgcTotal)
## lt1AgcTotal <- within( lt1AgcTotal, { term1 <- n-p; term2 <- 1-n-t})

## within( head( lt1AgcShrub), { 


## normalize to fix 65 pixels missing area
aglandComplete <- aglandComplete /aglandCompleteSum
layerNames( aglandComplete) <- names(peelClasses)[-8]

agcMap <- coverMaps( aglandComplete, 0.4,
                    classes= layerNames( aglandComplete)[1:4]) +
  coord_equal() +
  facet_grid( variable ~ .)

agcMap2 <- coverMaps( aglandComplete, 0.4,
                    classes= layerNames( aglandComplete)[5:8]) +
  coord_equal() +
  facet_grid( variable ~ .)
                                                                                       



###################################################
### chunk number 25: fig_agc
###################################################
#line 1510 "/home/nbest/thesis/analysis.Rnw"

my.ggsave( texWd, "fig_agc.png",
          plot= agcMap, width=4.5, height=8)


###################################################
### chunk number 26: fig_agc2
###################################################
#line 1524 "/home/nbest/thesis/analysis.Rnw"

my.ggsave( texWd, "fig_agc2.png",
          plot= agcMap2, width=4.5, height=8)


###################################################
### chunk number 27: table_rmse3
###################################################
#line 1540 "/home/nbest/thesis/analysis.Rnw"

setwd( rasterWd)  

rmseDf3 <- 
  cbind( agland=c( TRUE, rep(FALSE, times=3)),
        rbind( c( TRUE, 0.5,
                 rmseRast( getPeelBand( aglandComplete, "crop"),
                          unstack( agland)[[ 1]]),
                 rmseRast( getPeelBand( aglandComplete, "open"),
                          unstack( agland)[[ 2]])),
              rmseDf2))
rmseDf3 <- within(rmseDf3, offset <- as.logical( offset))
                                        # had to change offset column back
                                        # to true/false;  maybe this can be
                                        # avoided with list() instead of c()

rmseXt <- xtable( rmseDf3,
                 caption= "RMSE of Agland Complete vs. Agland2000",
                 label= "tab:rmse3",
                 digits= c( 0, 0, 0, 1, 3, 3))
                                        # looks like some kind of bug in xtable()
                                        # manual correction:
rmseXt$agland <- rmseDf3$agland
rmseXt$offset <- rmseDf3$offset

print( rmseXt,
      include.rownames= FALSE,
      sanitize.colnames.function= function(x) x)



###################################################
### chunk number 28: tab_areas3
###################################################
#line 1572 "/home/nbest/thesis/analysis.Rnw"

areasCt3 <- acreageTable( c( rasterNames2[ c( 1, 2, 4, 7)], "aglandComplete"))

local({
  colnames( areasCt3) <-
    c( "Agland2000", "NLCD",
      "\\pbox[c][][c]{3in}{MLCT\\\\No Mosaic}",
      "\\pbox[c][][c]{3in}{\\smallskip{}MLCT\\\\Adjusted\\\\No Mosaic}",
      "PEEL")
  print( xtable( areasCt3 / 10^6, 
                caption= "Agland Complete (AgC) acreages, $A_{min}=0.5$",
                label= "tab:areas3",
                digits= 1),
        size= "small",
        add.to.row= list( 
          pos= list( 0, nrow( areasCt)),
          command= rep("\\noalign{\\smallskip}", times= 2)),        
        sanitize.colnames.function= function(x) x)
  ##,
  ##      floating= FALSE)
})

cropScatAgcDf <- 
  data.frame( as( stack(getPeelBand( aglandComplete, "crop"),
                        aglandCrop,
                        raster::mask(acres, cusaMask)),
                 "SpatialGridDataFrame"))
colnames(cropScatAgcDf) <-
  c( "agc", "agland", "acres", "lon", "lat")
cropScatAgcDf$weight <- with( cropScatAgcDf, acres/ max(acres))


if( overwriteFigures) scatPlotAgc <-
  ggplot( data=
         cropScatAgcDf, ##[ sample( 1:nrow(cropScatAgcDf),
                       ##         round( nrow(cropScatAgcDf) *0.05)),],
         aes( agland, agc,
             size= weight)) +
  geom_point( alpha= 0.03) +
  scale_area( limits= c(0.7, 1),
             breaks= seq( 0.7, by= 0.1),
             to= c(0.7, 1) *6) +
  geom_abline( size= 2,
              alpha= 0.4) +
  theme_bw() +
  scale_x_continuous( "Agland2000",
                     limits= c( 0, 1),
                     expand= c( 0.025, 0)) +
  scale_y_continuous( "Agland Complete",
                     limits= c( 0, 1),
                     expand= c( 0.025, 0)) +
  coord_equal() +
  opts( legend.position= "none")


cropScatAgcDf <-
  within( cropScatAgcDf,
         { cat <- NA
           cat[    agc == 0] <- 0
           cat[ agc > 0 & is.na( agland)] <- 1
           cat[ agc > 0 & ( agc -agland) < 0.1] <- 2
           cat[ agc > 0 & agc  < agland] <- 3
           cat[ agland == 1] <- 4
         })

agcThemeMap <-
  ggplot( cropScatAgcDf[ !is.na( cropScatAgcDf$cat),],
         aes( x= lon, y= lat)) +
  geom_tile( aes( fill= factor( cat))) +
  scale_fill_brewer( "",
                    breaks= 0:4,
                    labels= c(
                      "PEEL = 0",
                      "PEEL > 0 and Ag2k is null",
                      "PEEL > 0 and PEEL = Ag2k",
                      "Agc > 0 and PEEL < Ag2k",
                      "Ag2k = 1")) +
  coord_equal() +
  theme_map




###################################################
### chunk number 29: fig_scatPlotAgc
###################################################
#line 1663 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_scatPlotAgc.png",
         plot= scatPlotAgc)
}


  


###################################################
### chunk number 30: fig_hexPlotAgc
###################################################
#line 1684 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  ggsave( "fig_hexPlotAgc.pdf",
         plot= hexPlot1 %+% cropScatAgcDf +
              aes( agland, agc) +
           scale_fill_gradientn( colours= brewer.pal( 6, "YlGn"),
                       trans= "log10",
                       limits=c( 10, 20000)) +
              scale_x_continuous( "Agland2000") +
              scale_y_continuous( "Agland Complete",
                  limits= c( 0, 1),
                  breaks= seq( 0, 1, by= 0.2)) +
              coord_equal()
  )
}



###################################################
### chunk number 31: fig_agcThemeMap
###################################################
#line 1713 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  my.ggsave( texWd, "fig_agcThemeMap.png", width=7.5,
         plot= agcThemeMap)
}



###################################################
### chunk number 32: crop_cats
###################################################
#line 1757 "/home/nbest/thesis/analysis.Rnw"

setwd( rasterWd)

cropCats <-
  c("cereals", "field_crop", "forage", "maize",
    "rice", "shrub_crop", "soybean", "sugarcane",
    "tree_crop", "wheat")
names( cropCats) <- cropCats


cropCatsPeel <-
  list( crop= c( "cereals", "field_crop", "forage",
                "maize", "rice", "soybean",
                "sugarcane", "wheat"),
       open= NULL,
       shrub= "shrub_crop",
       forest= "tree_crop")
                     
cropCats <- llply(  cropCats, function(c) {
  raster( paste( dataPath,
                paste( c, "tif", sep="."),
                sep= "/"))
})

cropStack <- stack( cropCats)


cropSum <- overlay( stack( cropCats[ cropCatsPeel$crop]),
                   fun= sum)

cropSum[ is.na( cropSum[])
        & !is.na( getPeelBand( aglandComplete,
                              "crop")[])
        ] <- 0
cropSum <- writeRaster( cropSum,
                   file= "cropSum.tif",
                   overwrite= TRUE)

cropNormalFunc <- function( st) {
  cropCat <- st[ 1]
  cropSum <- st[ 2]
  agc <-     st[ 3]
  ifelse( cropSum == 0,
         0,
         cropCat / cropSum)
}

cropNormal <- stack( llply( cropCats[ cropCatsPeel$crop],
                           function( crop) {
                             calc( stack( crop,
                                         cropSum,
                                         getPeelBand( aglandComplete, "crop")),
                                  fun= cropNormalFunc)
                           }))

cropSubClasses <- getPeelBand( aglandComplete, "crop") *cropNormal
layerNames( cropSubClasses) <- cropCatsPeel$crop




###################################################
### chunk number 33: fig_cropSubClassesMap
###################################################
#line 1824 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  cropSubClassesMap <-
    coverMaps( cropSubClasses, 0.4,
              classes= layerNames(cropSubClasses)[ 1:4]) +
    coord_equal() +
    facet_grid( variable ~ .)
  my.ggsave( texWd, "fig_cropSubClassesMap.png",
            plot= cropSubClassesMap)
}



###################################################
### chunk number 34: fig_cropSubClassesMap2
###################################################
#line 1845 "/home/nbest/thesis/analysis.Rnw"

if( overwriteFigures) {
  cropSubClassesMap2 <-
    coverMaps( cropSubClasses, 0.4,
              classes= layerNames(cropSubClasses)[ 5:7]) +
    coord_equal() +
    facet_grid( variable ~ .)
  my.ggsave( texWd, "fig_cropSubClassesMap2.png",
            plot= cropSubClassesMap2)
}



###################################################
### chunk number 35: restack_crops
###################################################
#line 1888 "/home/nbest/thesis/analysis.Rnw"


check <- 
  getPeelBand( aglandComplete, "crop") -
  sum( cropSubClasses)

mlctCrop <-
  overlay( getPeelBand( aglandComplete, "crop"),
	  cropSum,
        fun= function( agc, ag) {
          ifelse( is.na( ag), agc, 0)
        })

check <- 
  getPeelBand( aglandComplete, "crop") -
  sum( cropSubClasses) -
  mlctCrop


peelData <- stack( aglandComplete, cropSubClasses, mlctCrop, check)
layerNames( peelData)[ 16:17] <- c("other_crop", "check")

peelDf <- data.frame(peelData[])

noisyCells <- rownames( with( peelDf,
    peelDf[ !is.na( check) & check >= 0.001,]))
# 325 cells with noise above this threshold

mlctCrop <- mlctCrop + check

peelData <- stack( aglandComplete, cropSubClasses, mlctCrop)
layerNames( peelData)[ 16] <- c("mlct_crop")

## peelData <- writeRaster( peelData,
##                         filename= sprintf( "%s/peel.tif", rasterWd),
##                         overwrite= TRUE)

peelData <- brick( peelData,
                  filename= sprintf( "%s/peel.tif", rasterWd),
                  overwrite= TRUE)

layerNames( peelData) <-
    c( unlist( llply( c( aglandComplete,
                        cropSubClasses),
                     layerNames)),
      "mlct_crop")


peelDf <- data.frame( peelData[ !is.na( grid[])])
rownames( peelDf) <- grid[][ !is.na(grid[])]

write.csv( format.df( peelDf,
                     dec=3,
                     numeric.dollar=FALSE,
                     na.blank= TRUE),
          file= "peel.csv",
          quote= FALSE)

## copy to data archive
## file.copy( "peel.csv", "~/see/data/cimdb/peel_thesis.csv",
##           overwrite=TRUE)


## peelData <- brick( aglandComplete, cropSubClasses)



###################################################
### chunk number 36: cleanup
###################################################
#line 1957 "/home/nbest/thesis/analysis.Rnw"
options( prompt= "> ", continue= "+ ", width= 80)


