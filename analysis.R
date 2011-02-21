###################################################
### chunk number 1: init
###################################################
#line 16 "/home/nbest/thesis/analysis.Rnw"

setwd( "~/thesis/analysis")

texWd <- setwd("../data")
dataWd <- getwd()
setwd( dataWd)

#rm(list=ls())
db <- "cusa.sqlite"
source( "~/thesis/code/analysis.R")
source( "~/thesis/code/peel.R")

## calculate areas in millions of acres (Ma)
areas <- sapply( list( ag="^ag_", As00="As00$", As05="As05$", agc="^agc", nlcd="^nlcd"),
                 function( re) return( cellStats( areaAcres( stackHandles( grepHandles( re))), 
                                                 sum) 
                                      /10 ^6))
names(areas$ag) <- c("crop", "open")
names(areas$As00) <- sort( c(covers, "mosaic"))
names(areas$As05) <- sort( c(covers, "mosaic"))
names(areas$agc) <- covers
names(areas$nlcd) <- covers

## merge the resulting structure into a data frame

areasDf <- merge( t( unlist( areas$As00)), 
                  t( unlist( areas$As05)), 
                  all=TRUE, sort=FALSE)
areasDf <- merge( areasDf, t( unlist( areas$ag)), 
                 all=TRUE, sort=FALSE)
areasDf <- merge( areasDf, t( unlist( areas$agc)), 
                 all=TRUE, sort=FALSE)
areasDf <- merge( areasDf, t( unlist( areas$nlcd)), 
                 all=TRUE, sort=FALSE)
rownames(areasDf) <- c( "As00", "As05", "ag", "agc", "nlcd")


printAreas <- function( Ma) {
  return( paste( round( Ma, digits=1), "Ma (", round( Ma *0.404685642, digits=1), "Mha)", sep=""))
}



###################################################
### chunk number 2: tabTotal
###################################################
#line 60 "/home/nbest/thesis/analysis.Rnw"
print( xtable( areasDf, 
              caption= "Total Acreages by Map and Cover", 
              label= "tab:total",
              digits= 1))


###################################################
### chunk number 3: bias
###################################################
#line 98 "/home/nbest/thesis/analysis.Rnw"

# calculate RMSE/bias summaries
# comparing everything to NLCD


rmseAgc <- rmseSummary( function(c) paste(  "agc", c, sep="_"),
                        function(c) paste( "nlcd", c, sep="_"))

rmseAs00 <- rmseSummary( function(c) paste( "mlct_2001", c, "As00", sep="_"),
                         function(c) paste( "nlcd", c, sep="_"))

rmseAs05 <- rmseSummary( function(c) paste( "mlct_2001", c, "As05", sep="_"),
                         function(c) paste( "nlcd", c, sep="_"))


###################################################
### chunk number 4: biasTab
###################################################
#line 114 "/home/nbest/thesis/analysis.Rnw"
## t( rmseAgc)
## t( rmseAs00)
## t( rmseAs05)

print( xtable( t( rmseAgc), 
              caption= "Errors and Biases of Aglands Complete relative to NLCD",
              label= "tab:ebagc",
              digits= c( 0, 2, -2, 0, 0)))

print( xtable( t( rmseAs00), 
              caption= "Errors and Biases of MLCT, $A_s = 0.0$ relative to NLCD",
              label= "tab:ebmlct00",
              digits= c( 0, 2, -2, 0, 0)))

print( xtable( t( rmseAs05), 
              caption= "Errors and Biases of MLCT, $A_s = 0.5$ relative to NLCD",
              label= "tab:ebmlct05",
              digits= c( 0, 2, -2, 0, 0)))


###################################################
### chunk number 5: stack
###################################################
#line 135 "/home/nbest/thesis/analysis.Rnw"
## agcAvgAcres <-
##   sapply( paste( "agc_", covers, sep=""),
##          function( map) {
##            mapRast <- raster( as.spgdf( handle( map)))
##            return( cellStats( areaAcres( mapRast), sum)
##                   /( ncell( mapRast) - cellStats( mapRast, 'countNA')))
##          })

         
## getting ready to plot

stackAgc <- stackHandles( grepHandles(  "^agc"))
attr( stackAgc, "layernames") <-  covers

stackNlcd <- stackHandles( grepHandles( "^nlcd"))
attr( stackNlcd, "layernames") <-  covers

stackDiff <- stackAgc -stackNlcd
attr( stackDiff, "layernames") <-  covers



###################################################
### chunk number 6: fig_agc
###################################################
#line 162 "/home/nbest/thesis/analysis.Rnw"

#spgdfAgc <- as.spgdf( stackAgc)
#names( spgdfAgc) <- layerNames( stackAgc)
setwd( texWd)
png( file="fig_agc.png")
print( coverMaps( stackAgc, 0.4))
dev.off()


###################################################
### chunk number 7: fig_nlcd
###################################################
#line 182 "/home/nbest/thesis/analysis.Rnw"

##spgdfNlcd <- as.spgdf( stackNlcd)
##names( spgdfNlcd) <- layerNames( stackNlcd)
setwd( texWd)
png( file="fig_nlcd.png")
print( coverMaps( stackNlcd, 0.4))
dev.off()


###################################################
### chunk number 8: fig_diff
###################################################
#line 202 "/home/nbest/thesis/analysis.Rnw"

##spgdfDiff <- as.spgdf( stackDiff)
##names( spgdfDiff) <- layerNames( stackDiff)
setwd( texWd)
png( file="fig_diff.png")
print( coverMaps( stackDiff, 0.4) + 
  scale_fill_gradientn( "diff", colours= rev( brewer.pal( 11, "BrBG")), 
                         limits= c( 0.1, -0.1),
                         breaks= seq( 0.1, -0.1, by= -0.02)))
dev.off()


###################################################
### chunk number 9: fig_cordiff
###################################################
#line 225 "/home/nbest/thesis/analysis.Rnw"

## look for correlations across the difference maps

corDiff <- cor( as.data.frame( as.spgdf( stackDiff))[,1:8])
colnames( corDiff) <- unlist( lapply( 
    strsplit( colnames( corDiff), "\\."), 
    function( x) return( x[ 2])))
rownames( corDiff) <- unlist( lapply( 
    strsplit( rownames( corDiff), "\\."), 
    function( x) return( x[ 2])))
ord <- order.dendrogram( as.dendrogram( hclust( dist( corDiff))))

corDiffPlot <- 
  ggplot( melt( corDiff),
         aes( x=X1, y=X2, fill= value)) +
  geom_tile() +
  theme_bw() +
  opts( panel.grid.minor= theme_blank(),
       panel.grid.major= theme_blank(),
       panel.background= theme_blank(),
       axis.title.x= theme_blank(),
       axis.text.x= theme_text( angle= 90, hjust=1),
       axis.title.y= theme_blank()) +
  scale_x_discrete( limits= colnames(corDiff)[ord]) +
  scale_y_discrete( limits= colnames(corDiff)[ord]) +
  scale_fill_gradientn( "cor", colours= rev( brewer.pal( 11, "BrBG")), 
                         limits= c( 1.0, -1.0),
                         breaks= seq( 1.0, -1.0, by= -0.2))


setwd( texWd)
png( file="fig_cordiff.png")
print( corDiffPlot)
dev.off()


###################################################
### chunk number 10: analysis
###################################################
#line 272 "/home/nbest/thesis/analysis.Rnw"
setwd( "~/thesis")


