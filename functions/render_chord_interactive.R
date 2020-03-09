library(chorddiag)


# TO BUILD PLOT LOCALLY
# un_data <- load_migration_data()
# un_attr <- load_country_attributes()
# un_data <- un_data[year == 2019]
# render_chord(un_data, un_attr)


render_chord_interactive <- function(un_data, un_attr, variable = 'region'){
  # merge variable from attribute into migration data
  setkey(un_data, country_to_code)
  setkey(un_attr, code)
  un_data <- un_data[un_attr[,.(code, var_to=get(variable))]]
  setkey(un_data, country_from_code)
  un_data <- un_data[un_attr[,.(code, var_from=get(variable))]]
  
  plot_data <- un_data[year==year,.(value=sum(value, na.rm=TRUE)),.(var_to, var_from)][value >0]
  
  plot_data <- dcast(plot_data, formula = var_to ~ var_from)
  var_to <- plot_data$var_to
  
  plot_data[, var_to := NULL]
  var_from <- names(plot_data)
  
  plot_matrix <- as.matrix(plot_data)
  dimnames(plot_matrix) <- list(var_to = var_to, var_from = var_from)
  
  chorddiag(plot_matrix, groupnamePadding = 10, groupnameFontsize = 10, showTicks = FALSE)
  #groupColors=c('#8E44AD', '#27AE60', '#2980B9', '#F39C12', '#F1C40F', '#E74C3C')
}

