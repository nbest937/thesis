theme_update( panel.grid.minor= theme_blank(),
             panel.grid.major= theme_blank(),
             panel.background= theme_blank(),
             axis.title.x= theme_blank(),
             axis.text.x= theme_text( angle= 90, hjust=1),
             axis.title.y= theme_blank())


ggplotRaster <- function( r, samp) {
  df <- data.frame( as( sampleRegular( r, ncell( r)*samp, 
                                      asRaster=TRUE), 
                       "SpatialGridDataFrame"))
  ## names(df)[ 1:length( layerNames( r))] <- layerNames( r)
  ext <- extent( r)
  ggplot( data= df) +
    geom_tile( aes( x= s1, y= s2, fill= values)) +
    theme_bw() +
    scale_x_continuous( limits= c( ext@xmin, ext@xmax),
                       expand= c( 0,0)) +
    scale_y_continuous(  limits= c( ext@ymin, ext@ymax),
                       expand= c( 0,0)) ##+
  ## opts( panel.grid.minor= theme_blank(),
  ##         panel.grid.major= theme_blank(),
  ##         panel.background= theme_blank(),
  ##             axis.title.x= theme_blank(),
  ##              axis.text.x= theme_text( angle= 90, hjust=1),
  ##             axis.title.y= theme_blank())
  
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
    ## opts( panel.grid.minor= theme_blank(),
    ##       panel.grid.major= theme_blank(),
    ##       panel.background= theme_blank(),
    ##           axis.title.x= theme_blank(),
    ##            axis.text.x= theme_text( angle= 90, hjust=1),
    ##           axis.title.y= theme_blank()) +
    scale_fill_gradientn( colours= rev( brewer.pal( 6, "YlGn")), 
                         limits= c( 1, 0),
                         breaks= seq( 1, 0, by= -0.2)) +
    facet_wrap(~ variable)
}

my.ggsave <- function(filename = default_name(plot),
                      height= 3.5, width= 3.5, dpi= 72, ...) {
  ggsave(filename=filename, height=height, width=width, dpi=dpi, ...)
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


