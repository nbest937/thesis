###################################################
### chunk number 1: init
###################################################
#line 20 "/home/nbest/thesis/analysis2.Rnw"

options( prompt= " ", continue= " ", width= 60)
options(error= function(){
  recover()
  options( prompt= "> ", continue= "+ ", width= 80)
})
  
source( "~/thesis/code/analysis.R")
source( "~/thesis/code/peel.R")
source( "~/thesis/code/maps.R")

   texWd <- path.expand( "~/thesis/analysis")
rasterWd <- path.expand( "~/thesis/data/analysis")
dataPath <- path.expand( "~/thesis/data")
setwd( rasterWd)

overwriteRasters <- FALSE
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
cusaMask <- raster( "mask_cusa.tif")
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
grid <- mask( grid, cusaMask)

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
#line 233 "/home/nbest/thesis/analysis2.Rnw"


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
#line 260 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {  
  setwd( texWd)
  ggsave( "fig_areas.pdf", plot= areasPlot)
}




###################################################
### chunk number 4: rmse
###################################################
#line 334 "/home/nbest/thesis/analysis2.Rnw"


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
                        mask(acres, cusaMask)),
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




###################################################
### chunk number 5: fig_scatplot1
###################################################
#line 384 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_scatPlot1.png",
         plot= scatPlot1)
}



###################################################
### chunk number 6: fig_scatplot05
###################################################
#line 404 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_scatPlot05.png",
         plot= scatPlot1 +
         aes(agland, nomos05, size=weight) +
         scale_y_continuous( "MLCT, A_min = 0.5",
                            limits= c( 0, 1),
                            expand= c( 0.025, 0)))
}





###################################################
### chunk number 7: table_rmse
###################################################
#line 427 "/home/nbest/thesis/analysis2.Rnw"

print( xtable( rmseDf,
              caption= "RMSE, MLCT vs. Agland2000 crop",
              label= "tab:rmse",
              digits= c( 0, 1, 3)),
      include.rownames= FALSE,
      sanitize.colnames.function= function(x) x)



###################################################
### chunk number 8: offsets_calc
###################################################
#line 484 "/home/nbest/thesis/analysis2.Rnw"

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

nlcdIgnoreOffsets <-
  -1 *mlctKeep /sum( mlctKeep) *sum( nlcdKeepOffsets)

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
                             classes= layerNames( nlcdOffsets)[ 1:5])

offsetsMap2 <- coverDiffMaps( nlcdOffsets, samp= 0.4,
                             classes= layerNames( nlcdOffsets)[ 6:10])


thumbOffsetsMap <-
  coverDiffMaps( thumbNlcdOffsets,
                classes= layerNames( thumbNlcdOffsets)[-10]) +
  facet_wrap( ~variable)


## offsetsMap$data <-
##   within( offsetsMap$data,
##          cuts <- cut( value, breaks=
##                      c( -1, -0.5, -0.1, -0.05, -0.01,
##                        0.01, 0.05, 0.1, 0.5, 1)))


## ggplot( offsetsMap$data, aes( x= s1, y=s2, fill=cuts)) +
##   geom_tile() +
##   scale_fill_manual( values= brewer.pal( 9, "BrBG")) +
##   facet_grid( variable ~ .) +
##   coord_equal()

## thumbOffsetsMap <- coverMaps( crop( nlcdOffsets, thumbExtent)) +
##   coord_equal() +
##   ##facet_grid( variable ~ .) +
##   scale_fill_gradientn( "diff", colours= rev( brewer.pal( 11, "BrBG")), 
##                        limits= c( 1.0, -1.0),
##                        breaks= seq( 0.5, -0.5, by= -0.1))

## thumbOffsetsMap$data <-
##   within( thumbOffsetsMap$data,
##          cuts <- cut( value, breaks=
##                      c( -1, -0.5, -0.1, -0.05, -0.01,
##                        0.01, 0.05, 0.1, 0.5, 1)))

## ##breaks= c( -1, seq( -0.5, 0.5, by=0.1)[-6], 1)

## ggplot( thumbOffsetsMap$data, aes( x= s1, y=s2, fill=cuts)) +geom_tile() +scale_fill_manual( values= brewer.pal( 9, "BrBG")) +facet_wrap(~ variable)

## thumbOffsetsMap+ geom_tile( aes( x= s1, y=s2, fill=cuts)) +scale_fill_manual( values= brewer.pal( 9, "BrBG"))




###################################################
### chunk number 9: fig_offsetsmap1
###################################################
#line 576 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_offsets1.png", plot= offsetsMap1, height= 7)
}



###################################################
### chunk number 10: fig_offsetsmap2
###################################################
#line 595 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_offsets2.png", plot= offsetsMap2, height= 7)
}



###################################################
### chunk number 11: cor_offsets
###################################################
#line 615 "/home/nbest/thesis/analysis2.Rnw"

corOffsets <- cor( data.frame(as( nlcdOffsets, "SpatialGridDataFrame"))[, 1:9])
colnames( corOffsets) <- names( peelClasses)
rownames( corOffsets) <- names( peelClasses)

ord <- order.dendrogram( as.dendrogram( hclust( dist( corOffsets))))

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
### chunk number 12: cusa_offset
###################################################
#line 663 "/home/nbest/thesis/analysis2.Rnw"

setwd( rasterWd)

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
### chunk number 13: areas2
###################################################
#line 688 "/home/nbest/thesis/analysis2.Rnw"

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
### chunk number 14: restack_check
###################################################
#line 773 "/home/nbest/thesis/analysis2.Rnw"

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
### chunk number 15: results_restack_check
###################################################
#line 822 "/home/nbest/thesis/analysis2.Rnw"

check <- restackOverlay( c( mlctAdj[ c("nomos", "delta")], nlcdOffsets, agg05),
               function( n, d, o, a) n-d-o-a)
layerNames(check) <- names( peelClasses)

check



###################################################
### chunk number 16: table_restack_check
###################################################
#line 833 "/home/nbest/thesis/analysis2.Rnw"
  
checkTable <-
  xtable( cbind( class=peelClasses,
                min=minValue( check),
                max=maxValue(check)),
         caption= "Balance of adjustment fractions and original MLCT aggregation", 
         label= "tab:restack_check")
digits( checkTable) <- c( 0, 0,-2,-2)
print( checkTable)
  


###################################################
### chunk number 17: table_rmse2
###################################################
#line 852 "/home/nbest/thesis/analysis2.Rnw"

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
### chunk number 18: tab_areas2
###################################################
#line 893 "/home/nbest/thesis/analysis2.Rnw"


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
                        mask(acres, cusaMask)),
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
### chunk number 19: fig_offsets
###################################################
#line 997 "/home/nbest/thesis/analysis2.Rnw"
 
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
### chunk number 20: fig_areasAdj
###################################################
#line 1034 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  ggsave( "fig_areasAdj.pdf", plot= areasPlotAdj)
}



###################################################
### chunk number 21: fig_scatPlotAdj
###################################################
#line 1050 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_scatPlotAdj.png",
         plot= scatPlotAdj)
}


  


###################################################
### chunk number 22: agc
###################################################
#line 1071 "/home/nbest/thesis/analysis2.Rnw"

## thumbAgland <- crop( agland,
##                     extent(-83.5, -(82+25/60), 42+55/60, 44+5/60),
##                     filename= "thumbAgland.tif",
##                     progress="text")

nomosCrop <- getPeelBand( mlctAdj$nomos, "crop")
aglandCrop <- unstack( agland)[[ 1]]

if( overwriteRasters || TRUE) {
  ## nomosTruth <- getPeelBand( mlctAdj$nomos, "water") +
  ##   getPeelBand( mlctAdj$nomos, "wetland") +
  ##     getPeelBand( mlctAdj$nomos, "urban")
  ## nomosTruth <- writeRaster( nomosTruth, "nomosTruth.tif")
  nomosTruth <- overlay( getPeelBand( mlctAdj$nomos, "water"),
                        getPeelBand( mlctAdj$nomos, "wetland"),
                        getPeelBand( mlctAdj$nomos, "urban"),
                        fun= sum,
                        filename= "nomosTruth.tif",
                        overwrite= TRUE)
} else nomosTruth <-
  brick( list.files( dataPath,
                    patt="nomosTruth.tif",
                    full.names= TRUE)) 


nomosClasses <- layerNames( mlctAdj$nomos)[ -9]
                                        # leaves out 'total'
                                        # mosaic is already gone


## stupid overlay() bug!
## peelCrop <-
##   overlay( aglandCrop, nomosCrop, nomosTruth, fun=
##           function( a, n, t) {
##             ifelse( is.na( a), n, min( a, 1 -t))
##           })

peelCrop <-
  if( overwriteRasters || TRUE) {
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

peelCrop <-
  calc( stack( aglandCrop, nomosCrop, nomosTruth), fun=
       function( st) {
         a <- st[ 1]
         n <- st[ 2]
         t <- st[ 3]
         ifelse( is.na( a), n, min( a, 1 -t))
       })

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
            ifelse( 1 -n -t ==0,
                   0,
                   1 +( n -p) /( 1 -n -t))
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




aglandComplete <-
  stack( unstack( mlctAdj$nomos)[ -9]) *factorStack +offsetStack
aglandComplete <- writeRaster( aglandComplete,
                              "aglandComplete.tif",
                              overwrite= TRUE)
layerNames( aglandComplete) <- names(peelClasses)[-8]

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



agcMap <- coverMaps( aglandComplete, 0.4) +
  coord_equal() +
  facet_grid( variable ~ .)




###################################################
### chunk number 23: fig_agc
###################################################
#line 1200 "/home/nbest/thesis/analysis2.Rnw"

setwd( texWd)
my.ggsave( "fig_agc.png", width=4.5, height=8)

## png( file="fig_agc.png",
##     height= 8, width= 4.5, units= "in"  # no effect
##     res= 300)
## print( agcMap)
## dev.off()



###################################################
### chunk number 24: table_rmse3
###################################################
#line 1225 "/home/nbest/thesis/analysis2.Rnw"

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
### chunk number 25: fig_scatPlotAgc
###################################################
#line 1260 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_scatPlotAgc.png",
         plot= scatPlotAgc)
}


  


###################################################
### chunk number 26: tab_areas3
###################################################
#line 1278 "/home/nbest/thesis/analysis2.Rnw"

areasCt3 <- acreageTable( c( rasterNames2[ c( 1, 2, 4, 7)], "aglandComplete"))

local({
  colnames( areasCt3) <-
    c( "Agland2000", "NLCD",
      "\\pbox[c][][c]{3in}{MLCT\\\\No Mosaic}",
      "\\pbox[c][][c]{3in}{\\smallskip{}MLCT\\\\Adjusted\\\\No Mosaic}",
      "AgC")
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
                        mask(acres, cusaMask)),
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
           cat[ agc > 0 & agc == agland] <- 2
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
                      "AgC = 0",
                      "AgC > 0 and Ag2k is null",
                      "AgC > 0 and AgC = Ag2k",
                      "Agc > 0 and AgC < Ag2k",
                      "Ag2k = 1")) +
  coord_equal()




###################################################
### chunk number 27: fig_agcThemeMap
###################################################
#line 1365 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_agcThemeMap.png",
         plot= agcThemeMap)
}



###################################################
### chunk number 28: crop_cats
###################################################
#line 1382 "/home/nbest/thesis/analysis2.Rnw"

setwd( rasterWd)
##cropCats <- rep( NA, times=10)
cropCats <-
  c("cereals", "field_crop", "forage", "maize",
    "rice", "shrub_crop", "soybean", "sugarcane",
    "tree_crop", "wheat")
names( cropCats) <- cropCats

cropCatsPeel <- cropCats
cropCatsPeel[ c( "cereals", "field_crop",
                "maize", "rice", "soybean",
                "sugarcane", "wheat")] <- "crop"
cropCatsPeel[ "forage"] <- "open"
cropCatsPeel[ "shrub_crop"] <- "shrub"
cropCatsPeel[ "tree_crop"] <- "forest"

cropCatsPeel <-
  list( crop= c( "cereals", "field_crop",
                "maize", "rice", "soybean",
                "sugarcane", "wheat"),
       open= "forage",
       shrub= "shrub_crop",
       forest= "tree_crop")
                     
cropCats <- llply(  cropCats, function(c) {
  raster( paste( dataPath,
                paste( c, "tif", sep="."),
                sep= "/"))
})

cropStack <- stack( cropCats)


cropSum <- overlay( stack( cropCats[ cropCatsPeel$crop]),
                   fun= sum,
                   file= "cropSum.tif",
                   overwrite= TRUE)
                                        # problem: max is 1.37
                                        # double-cropping?
                                        # what about non-crop covers?

## doubleCrop <-
##   calc( stack( getPeelBand( aglandComplete, "crop"), cropSum),
##        fun= function( st) {
##          agc <- st[ 1]
##          cs <- st[ 2]
##          ifelse( cs > agc, min( cs -agc, agc), 0)
##        },
##        filename= "doubleCrop.tif",
##        overwrite= TRUE)


## cropOffsets <- llply( cropCatsPeel, function( catsVec) {
##   if( length( catsVec) ==1) {
##     cropCats[ catsVec]
##   } else {
##     overlay( stack( cropCats[ catsVec]), fun=sum)
##   }})

cropNormal <- stack( cropCats[ cropCatsPeel$crop]) / cropSum
layerNames( cropNormal) <- cropCatsPeel$crop

cropSubClasses <- getPeelBand( aglandComplete, "crop") *cropNormal
layerNames( cropSubClasses) <- cropCatsPeel$crop

cropSubClassesMap <-
  coverMaps( cropSubClasses, 0.4) +
  coord_equal() +
  facet_grid( variable ~ .)



###################################################
### chunk number 29: fig_cropSubClassesMap
###################################################
#line 1459 "/home/nbest/thesis/analysis2.Rnw"

if( overwriteFigures) {
  setwd( texWd)
  my.ggsave( "fig_cropSubClassesMap.png",
         plot= cropSubClassesMap)
}



###################################################
### chunk number 30: restack_crops
###################################################
#line 1496 "/home/nbest/thesis/analysis2.Rnw"


###################################################
### chunk number 31: cleanup
###################################################
#line 1500 "/home/nbest/thesis/analysis2.Rnw"
options( prompt= "> ", continue= "+ ", width= 80)


