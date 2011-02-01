divTheme <- sp.theme(regions=list(col=colorRampPalette(brewer.pal(5,"BrBG"), space="Lab")(100)))
seqTheme <- sp.theme(regions=list(col=colorRampPalette(brewer.pal(5,"YlGn"), space="Lab")(100)))

fusePath <- "/Users/nbest/mnt/see/grass/global/cusa/cellhd/"
crop05 <- new("GDALReadOnlyDataset", paste(fusePath,"2001_crop_As05_5min",sep=''))
crop00 <- new("GDALReadOnlyDataset", paste(fusePath,"2001_crop_As00_5min",sep=''))

as.spgdf <- function( gdalro) {
  as( gdalro, "SpatialGridDataFrame")
}

crop05grid <- as.spgdf(crop05)
crop00grid <- as.spgdf(crop00)

trellis.par.set(seqTheme)
#quartz()
crop05map <- spplot(as.spgdf(crop05), scales=list(draw=T))
print(crop05map)
#quartz()
crop00map <- spplot(as.spgdf(crop00), scales=list(draw=T))
print(crop00map)

trellis.par.set(divTheme)
cropDiffMap <- spplot( ~( crop05grid -crop00grid), scales= list( draw= T))
print(cropDiffMap)
