theme_set( theme_bw( base_family= "serif"))

theme_update( panel.grid.minor= theme_blank(),
             panel.grid.major= theme_blank(),
             panel.background= theme_blank(),
             axis.title.x= theme_blank(),
             axis.text.x= theme_text( family= "serif",
               angle= 90, hjust= 1 ),
             axis.text.x= theme_text( family= "serif"),
             axis.title.y= theme_blank())

theme_map <- theme_get()

theme_set( theme_bw( base_family= "serif"))

ggplotRaster <- function( r, samp) {
  df <- data.frame( as( sampleRegular( r, ncell( r)*samp, 
                                      asRaster=TRUE), 
                       "SpatialGridDataFrame"))
  ext <- extent( r)
  ggplot( data= df) +
    geom_tile( aes( x= s1, y= s2, fill= values)) +
    scale_x_continuous( limits= c( ext@xmin, ext@xmax),
                       expand= c( 0,0)) +
    scale_y_continuous( limits= c( ext@ymin, ext@ymax),
                       expand= c( 0,0)) +
    theme_map +
    coord_equal()
}


peelMap <- function( r, samp, classes= names( peelClasses)) {
  p <- ggplotRaster( r, samp)
  colnames( p$data)[1] <- "values" 
  p$data$values <- factor( p$data$values, 
                          levels= peelClasses, 
                          labels= names( peelClasses))
  p$data <- p$data[ p$data$values %in% classes,]
  p +
    geom_tile( aes( x= s1, y= s2, 
                    fill= values)) + 
    scale_fill_manual( "",
                      values= peelLegend, 
                      breaks= names( peelClasses)) 
}

             
             
coverMaps <- function( r, samp=1,
                      classes= layerNames(r),
                      ...) {
  df <- data.frame( as( sampleRegular( r, ncell( r)*samp, 
                                      asRaster=TRUE), 
                       "SpatialGridDataFrame"))
  names( df)[ grep( "^values", names( df))] <- layerNames( r)
  df <- df[, c( "s1", "s2", classes)]
  df <- melt( df, id.vars= c("s1", "s2"))
  ggplot( data= df) +
    geom_tile( aes( x= s1, y= s2, fill= value)) +
    scale_x_continuous( expand= c( 0,0)) +
    scale_y_continuous( expand= c( 0,0)) +
    scale_fill_gradientn( colours= rev( brewer.pal( 6, "YlGn")), 
                         limits= c( 1, 0),
                         breaks= seq( 1, 0, by= -0.2)) +
    facet_wrap(~ variable) +
    theme_map +
    coord_equal()
}

my.ggsave <- function( wd= getwd(),
                      filename= default_name(plot),
                      height= 5, width= 5, ...) {
  ggsave(filename= paste( wd, filename, sep= "/"),
         height= height,
         width= width, ...)
}

coverDiffMaps <-
  function( r, samp= 1,
           classes= layerNames( r),
           breaks= c( -1, -0.5, -0.1, -0.05, -0.01,
                       0.01, 0.05, 0.1, 0.5, 1),
           ...) {
  df <- data.frame( as( sampleRegular( r, ncell( r)*samp, 
                                      asRaster=TRUE), 
                       "SpatialGridDataFrame"))
  names( df)[ grep( "^values", names( df))] <- layerNames( r)
  df <- df[, c( "s1", "s2", classes)]
  df <- melt( df, id.vars= c("s1", "s2"))
  df <- within( df,
               cuts <- cut( value,
                           breaks= breaks,
                           include.lowest= TRUE))
  pal <- brewer.pal( length( levels( df$cuts)), "BrBG")
  names( pal) <- levels( df$cuts)
  ggplot( data= df) +
    geom_tile( aes( x= s1, y= s2, fill= cuts)) +
    scale_fill_manual( "offset", values= pal, breaks= rev( names( pal))) +
    facet_grid( variable ~ .) +
    scale_x_continuous( expand= c( 0,0)) +
    scale_y_continuous( expand= c( 0,0)) +
    theme_map
}


