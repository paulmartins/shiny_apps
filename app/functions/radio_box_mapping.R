main_map_radio_box_values <- c('percent_migrant', 'percent_f_migrant', 'percent_refugees', 'net_migration')
world_radio_box_values <- c('development_index', 'income_index', 'region')

radio_box_mapping <- function(category_values){
  sapply(category_values, USE.NAMES=FALSE, function(categ){
    switch(categ
           , "development_index"="Development index"
           , "income_index"="Income index"
           , "region"="SDG Region"
           , "percent_migrant"="Proportion of migrants ⚥"
           , "percent_f_migrant"="Proportion of migrants ♀"
           , "percent_refugees"="Proportion of refugees ⚥"
           , "net_migration"="Net migration rate")
  })
}