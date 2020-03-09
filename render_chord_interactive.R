library(chorddiag)

un_data <- load_migration_data()
un_attr <- load_country_attributes()

# TO BUILD PLOT LOCALLY
# un_data <- load_migration_data()
# un_attr <- load_country_attributes()
# un_data <- un_data[year == 2019]
# render_chord(un_data, un_attr)

render_chord <- function(un_data, un_attr, variable = 'region'){
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










subregion_data <- un_data[,.(value=sum(value,na.rm=TRUE)),.(subregion_to, subregion_from)][value >0]

mm <- dcast(subregion_data, formula = subregion_to ~ subregion_from)
subregion_to <- mm$subregion_to
mm[, subregion_to := NULL]
subregion_from <- names(mm)

mmm <- as.matrix(mm)
dimnames(mmm) <- list(subregion_to = subregion_to,
                      subrefion_from = subregion_from)

chorddiag(mmm, groupnamePadding = 20, showTicks = FALSE)


chorddiag(data, type = "directional", width = NULL, height = NULL,
          margin = 100, palette = "Dark2", palette2 = "Greys",
          showGroupnames = TRUE, groupNames = NULL, groupColors = NULL,
          groupThickness = 0.1, groupPadding = 2, groupnamePadding = 30,
          groupnameFontsize = 18, groupedgeColor = NULL,
          chordedgeColor = "#808080", categoryNames = NULL,
          categorynamePadding = 100, categorynameFontsize = 28, showTicks = TRUE,
          tickInterval = NULL, ticklabelFontsize = 10, fadeLevel = 0.1,
          showTooltips = TRUE, showZeroTooltips = TRUE, tooltipNames = NULL,
          tooltipUnit = NULL, tooltipFontsize = 12,
          tooltipGroupConnector = " &#x25B6; ", precision = NULL,
          clickAction = NULL, clickGroupAction = NULL)

