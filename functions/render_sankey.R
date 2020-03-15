



# un_data_graph <- un_migration_flow
# un_migration_flow <- fread('../data/un_migration_flow.csv')
# un_data_graph_to <- un_migration_flow[year==2019 & country_to_code==826 & value>0, .(country_to, country_from, value)][order(value)]
# un_data_graph_from <- un_migration_flow[year==2019 & country_from_code==826 & value>0, .(country_to, country_from, value)][order(value)]
# un_data_graph <- rbind(un_data_graph_to, un_data_graph_from)
# render_sankey(un_data_graph, top_n_in=10, top_n_out=13)

render_sankey <- function(un_data_graph, top_n_in=10, top_n_out=10, show_others_in=TRUE, show_others_out=TRUE){
  
  country <- un_data_graph[,.N,country_to][N>1]$country_to
  sankey_data <- rbind(tail(un_data_graph[country_to==country][order(value)], top_n_in),
                       tail(un_data_graph[country_from==country][order(value)], top_n_out))
  
  add_one_to_index <- 0
  
  if(show_others_in){
    in_countries <- sankey_data[country_to==country, country_from]
    others_in <- un_data_graph[country_to==country & !country_from %in% in_countries
                              , .(country_to, country_from='Others', value=sum(value, na.rm=TRUE))][1]
    if(others_in$value > 0){
      sankey_data <- rbind(sankey_data, others_in)
      add_one_to_index <- 1
    }
  }
  
  if(show_others_out){
    out_countries <- sankey_data[country_from==country, country_to]
    others_out <- un_data_graph[country_from==country & !country_to %in% out_countries
                                , .(country_to='Others', country_from, value=sum(value, na.rm=TRUE))][1]
    sankey_data <- rbind(sankey_data, others_out)
  }
    
  nodes <- unique(data.frame(name=c(  paste0(as.character(sankey_data$country_from),'_in')
                                    , paste0(as.character(sankey_data$country_to),'_out'))
                             ,group=c(  as.character(sankey_data$country_from)
                                      , as.character(sankey_data$country_to))
                               ))
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  sankey_data$IDcountry_from=match(paste0(sankey_data$country_from,'_in'), nodes$name)-1 
  sankey_data$IDcountry_to=match(paste0(sankey_data$country_to,'_out'), nodes$name)-1
  
  sankey_data[IDcountry_to==top_n_in+1+add_one_to_index, IDcountry_to:=top_n_in]
  
  nodes$name <- gsub(pattern='_in', replacement=' ', nodes$name)
  nodes$name <- gsub(pattern='_out', replacement='', nodes$name)
  nodes$name[top_n_in+2+add_one_to_index] <- ''

  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  
  # Make the Network
  sn <- sankeyNetwork(  Links=sankey_data
                      , Nodes=nodes
                      , Source="IDcountry_from"
                      , Target="IDcountry_to"
                      , Value="value"
                      , NodeID="name"
                      , NodeGroup="group"
                      , sinksRight=FALSE
                     # , colourScale=ColourScal
                      , nodeWidth=20 #nodePadding=20
                      , fontSize=13 #width=600, height=300,
                      , margin=list("left"=100, "right"=100)
                      )
  
  js_label_swap_side <- paste0('
    function(el,x){
      // select all our node text
      var node_text = d3.select(el)
      .selectAll(".node text")
      //and make them match
      .filter(function(d) { return (["', paste0(sankey_data$country_from,' ', collapse = '","'), '"].indexOf(d.name) > -1);})
      //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
      .attr("x", - x.options.nodeWidth/2)
      .attr("text-anchor", "end");
      }
  ')
  return(onRender(sn, js_label_swap_side))
}


