library(networkD3)
library(htmlwidgets)


render_sankey <- function(un_data_graph){
  
  nodes <- data.frame(name=c(as.character(un_data_graph$country_from), as.character(un_data_graph$country_to)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  un_data_graph$IDcountry_from=match(un_data_graph$country_from, nodes$name)-1 
  un_data_graph$IDcountry_to=match(un_data_graph$country_to, nodes$name)-1
  
  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  
  # Make the Network
  sn <- sankeyNetwork(Links = un_data_graph, Nodes = nodes,
                Source = "IDcountry_from", Target = "IDcountry_to",
                Value = "value", NodeID = "name", 
                sinksRight=FALSE, colourScale=ColourScal, 
                nodeWidth=20, #nodePadding=20, 
                fontSize=13, #width=600, height=300,
                margin = list("left"=100, "right"=100))
  
  js_label_swap_side <- paste0('
    function(el,x){
      // select all our node text
      var node_text = d3.select(el)
      .selectAll(".node text")
      //and make them match
      .filter(function(d) { return (["', paste0(un_data_graph$country_from, collapse = '","'), '"].indexOf(d.name) > -1);})
      //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
      .attr("x", - x.options.nodeWidth/2)
      .attr("text-anchor", "end");
      }
  ')
  return(onRender(sn, js_label_swap_side))
}

un_data_graph_to <- tail(un_data[year == '2017' & country_to_code == 250 & value > 0, .(country_to, country_from, value)][order(value)],20)
un_data_graph_from <- tail(un_data[year == '2017' & country_from_code == 250 & value > 0, .(country_to, country_from, value)][order(value)],20)

render_sankey(rbind(un_data_graph_from,un_data_graph_to))
