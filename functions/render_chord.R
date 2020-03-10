library(circlize)
library(ComplexHeatmap)
library(gridBase)


# TO BUILD PLOT LOCALLY (but without legend, issue with viewport)
# un_migration_flow <- load_migration_data()
# un_country_attr <- load_country_attributes()
# un_country_attr <- calculate_chord_max(un_country_attr, un_migration_flow)
# un_migration_flow <- un_migration_flow[year == 2019]
# render_chord(un_migration_flow, un_country_attr, loc=TRUE) 


render_chord <- function(un_migration, un_country_attr, variable="Region", loc=FALSE){
  
  # grab variable column name, colors and max values from the input variable arg
  var <- tolower(gsub(pattern="[\\s|-]", "_", variable, perl=TRUE))
  selected_color <- switch(  var
                           , "income_index"='income_color'
                           , "development_index"='development_color'
                           , "reg_color")
  selected_max <- switch(  var
                         , "income_index"='income_max_chord'
                         , "development_index"='development_max_chord'
                         , "region_max_chord")
  
  # merge variable from country attribute into migration data
  setkey(un_migration, country_to_code)
  setkey(un_country_attr, code)
  un_migration <- un_migration[un_country_attr[, .(code, var_to=get(var))]]
  setkey(un_migration, country_from_code)
  un_migration <- un_migration[un_country_attr[, .(  code
                                                   , var_from=get(var)
                                                   , col=get(selected_color)
                                                   , max_chord=get(selected_max)
                                                   )
                                               ]
                               ]
  
  # define data and metadata for circlize_plot
  plot_data <- un_migration[, .(  value=sum(value, na.rm=TRUE)
                                , color=max(col)
                                , max_chord=max(max_chord)
                                )
                            , .(var_to, var_from)
                            ][value >0]
  plot_data_tibble <- dplyr::as_tibble(plot_data[, .(var_from, var_to, value=value/1e06)])
  metadata <- unique(plot_data[, .(var_from, col1=color, max_chord=max_chord)])
  metadata <- metadata[, order1:=as.numeric(var_from)][order(order1)]
  metadata <- dplyr::as_tibble(metadata)
  
  # Make the plot and its legend in a nice viewport
  plot.new()
  circle_size <- unit(1, "snpc")
  circle_vp <- viewport(  x=0.5
                        , y=1 
                        , width=circle_size
                        , height=0.9*circle_size
                        , name="circle"
                        , just=c("center", "top")
                        )
  pushViewport(circle_vp)
  par(omi=ifelse(rep(loc, 4), rep(0,4), gridOMI()), new=TRUE)
  circlize_plot(plot_data_tibble, metadata)
  #grid.rect(gp=gpar(fill="blue"))
  upViewport()

  label_vp <- viewport(  x=0.5
                       , y=0 
                       , width=circle_size
                       , height=0.2*circle_size
                       , name="label"
                       , just=c("center", "bottom")
                       )
  pushViewport(label_vp)
  lgd_points <- Legend(  at=metadata$order1
                       , type="grid"
                       , direction="horizontal"
                       , nrow=ceiling(length(metadata$var_from)/3)
                       , labels=as.character(metadata$var_from)
                       , labels_gp=gpar(fontsize=12, col='#6B8594')
                       , title=variable
                       , title_position="topleft"
                       , title_gp=gpar(fontsize=14, fontface="bold", col='#6B8594')
                       , title_gap=unit(3, "mm")
                       , legend_gp=gpar(fill=metadata$col1)
                       ) 
  lgd_list <- packLegend(lgd_points, direction="horizontal")
  grid.draw(lgd_list)
  #grid.rect(gp=gpar(fill="red"))
  upViewport()
}


circlize_plot <- function(plot_data_tibble, metadata){
  # intialise the circos plot
  circos.clear()
  par(bg='#F7F7F7')
  circos.par(  start.degree=90
             , track.margin=c(-0.1, 0.1)
             , gap.degree=4
             , points.overflow.warning=FALSE)
  
  max_var <- as.list(metadata$max_chord)
  names(max_var) <- metadata$var_from
  
  # plot the chord diagram
  chordDiagram(  x=plot_data_tibble
               , directional=1
               , order=metadata$var_from
               , grid.col=metadata$col1
               , annotationTrack="grid"
               , transparency=0.15
               , annotationTrackHeight=c(0.05, 0.1)
               , direction.type=c("diffHeight", "arrows")
               , link.arr.type="big.arrow"
               , diffHeight=-0.04
               , link.sort=TRUE
               , link.largest.ontop=TRUE
               , xmax=max_var)
  
  # add labels and axis
  circos.track(  track.index=1
               , bg.border=NA
               , panel.fun = function(x, y) {
                 circos.axis(  h="top"
                             , labels.cex=0.8
                             , labels.niceFacing=FALSE
                             , labels.pos.adjust=FALSE
                             , col='#6B8594'
                             , labels.col='#6B8594')
                 }
               )
}

  