
render_age_gender <- function(age_data_plot, un_country_attr, variable){
  setkey(age_data_plot, code)
  setkey(un_country_attr, code)

  age_data_plot <- age_data_plot[un_country_attr[, .(var=get(variable), code)]]
  # age_data_plot <- age_data_plot[un_country_attr[, .(code,var=get('region'))]]
  age_data_plot <- age_data_plot[, .(  percent_m=round(100*sum(migrant_male, na.rm=TRUE)/sum(pop_male, na.rm=TRUE), 2)
                                     , percent_f=round(100*sum(migrant_female, na.rm=TRUE)/sum(pop_female, na.rm=TRUE), 2))
                                 , .(var, age_group)]
  age_data_plot <- age_data_plot[!is.na(age_group)]
  
  n_categ <- length(unique(as.character(age_data_plot$var)))
  
  x_title_pos <- rep(c(0.15, 0.5, 0.85),3)[1:n_categ]
  y_title_pos <- rep(c(1.0, 0.62, 0.29), each=3)[1:n_categ]
  
  make_title <- function(i){
    list(  x=x_title_pos[i]
           , y=y_title_pos[i]
           , text=sprintf("<b>%s</b>", levels(age_data_plot$var)[i])
           , showarrow=F
           , xref='paper'
           , yref='paper'
           , font=list(  color=get_chord_colors(variable, value=levels(age_data_plot$var)[i])
                       , size=9)
           , xanchor = "center"
           , yanchor = "bottom" 
    )
  }
  
  plotList <- lapply(levels(age_data_plot$var)[1:n_categ], function(categ) plotly_age_gender_pyramid(age_data_plot, categ))
  plotTitles <- lapply(seq(n_categ), make_title)
  
  while(length(plotList) < 9){
    i <- length(plotList) 
    plotList[[i+1]] <- plotly_empty(type='bar')
    plotTitles[[i+1]] <- list()
  }
  
  subplot(plotList, nrows=3, margin=c(0.01,0.01,0.05,0.05), shareY=TRUE) %>%
    layout(annotations=plotTitles, paper_bgcolor='#F7F7F7', plot_bgcolor='#F7F7F7') %>%
    config(displayModeBar=F)
}



# plotly
plotly_age_gender_pyramid <- function(age_data_plot, categ){
  
  age_data_plot_categ <- age_data_plot[var==categ, .(age_group, Male=percent_m, Female=percent_f)]
  if(nrow(age_data_plot_categ)<1) return(plotly_empty())
  
  age_data_plot_categ <- melt(  age_data_plot_categ
                              , id.vars='age_group'
                              , variable.name='gender'
                              , value.name='percentage'
                              )
  age_data_plot_categ$abs <- abs(age_data_plot_categ$percentage)
  age_data_plot_categ[gender=='Male', percentage:=-percentage]
  
  m_ticks_max <- age_data_plot_categ[gender=='Male', min(percentage, na.rm=TRUE)]
  f_ticks_max <- age_data_plot_categ[gender=='Female', max(percentage, na.rm=TRUE)]
  m_bin <- f_bin <- 5
  if(m_ticks_max > -5) m_bin <- 2
  if(f_ticks_max < 5) f_bin <- 2
  if(m_ticks_max > -1) m_bin <- 1
  if(f_ticks_max < 1) f_bin <- 1
  if(m_ticks_max < -20) m_bin <- 10
  if(f_ticks_max > 20) f_bin <- 10
  
  m_ticks_max <- floor(m_ticks_max/m_bin)*m_bin
  f_ticks_max <- ceiling(f_ticks_max/f_bin)*f_bin
  x_ticks <- c(seq(m_ticks_max, 0, m_bin), seq(f_bin, f_ticks_max, f_bin))

  plot_ly(  age_data_plot_categ
          , x=~percentage
          , y=~age_group
          , split=~gender
          , type='bar'
          , orientation='h'
          , hoverinfo='text'
          , text =~paste0("<b>", age_group, '</b>: <em>', abs, ' %</em>')
          , color=~gender
          , opacity=1.0
          , legendgroup=~gender
          , showlegend=categ==unique(age_data_plot$var)[1]
          , colors=c('#2a3f54', '#1abb9c')) %>%
    layout(  bargap=0.3
           , barmode='overlay'
           , font=list(size=10)
           , xaxis=list(  tickmode='array'
                        , showline=TRUE
                        , title=''
                        , tickvals=x_ticks
                        , ticktext=paste0(abs(x_ticks),'%')
                        )
           , yaxis=list(title="", showline=FALSE, showgrid=TRUE)
           , legend=list(orientation='h')
           ) 
}



# rCharts
# for(categ in unique_var){
#   age_data_plot_categ <- age_data_plot[var==categ, .(age_group, Male=percent_m, Female=percent_f)]
#   age_data_plot_categ <- melt(age_data_plot_categ
#                               , id.vars = 'age_group'
#                               , variable.name = 'gender'
#                               , value.name = 'percentage'
#   )
#   age_data_plot_categ$abs <- abs(age_data_plot_categ$percentage)
#   age_data_plot_categ[gender=='Male', percentage:=-percentage]
#   n1 <- nPlot(
#     y = 'percentage', 
#     x = 'age_group', 
#     group = 'gender', 
#     type = 'multiBarHorizontalChart', 
#     data = age_data_plot_categ[order(-age_group)])
#   n1$chart(stacked = TRUE, color = c('#2a3f54', '#1abb9c'))
#   n1$chart(tooltipContent = "#! function(key, x, y, e){
#         var format = d3.format('0,000');
#         return '<h3>' + key + ', age ' + x + '</h3>' + 
#         '<p>' + 'Percentage: ' + format(e.point.abs) + '% </p>'
#         } !#")
#   
# }
