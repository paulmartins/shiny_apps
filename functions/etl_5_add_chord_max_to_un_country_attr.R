#' @description Add the columns region_max_chord | income_max_chord | development_max_chord to un_country_attr

add_chord_max_to_un_country_attr <- function(){
  un_migration_flow <- fread('../data/un_migration_flow.csv')
  un_country_attr <- setDT(readRDS(file = '../data/un_country_attributes.rds'))
  # loop over each attributes
  for(var in c('development_index', 'income_index', 'region')){
    flog.info(paste('Calculating max over years for', var))
    un_flow_copy <- copy(un_migration_flow)
    setkey(un_flow_copy, country_to_code)
    setkey(un_country_attr, code)
    un_flow_copy <- un_flow_copy[un_country_attr[, .(code, var_to=get(var))]]
    setkey(un_flow_copy, country_from_code)
    un_flow_copy <- un_flow_copy[un_country_attr[, .(code, var_from=get(var))]]
    
    un_flow_copy[, max_out:=sum(value, na.rm=TRUE), .(year, var_from)]
    un_flow_copy[, max_in:=sum(value, na.rm=TRUE), .(year,var_to)]
    un_flow_copy[var_to == var_from, max_tot:=max_out+max_in]
    max_mat <- unique(un_flow_copy[, .(max_tot=max(max_tot, na.rm=TRUE)/1e06), var_from])[!is.na(var_from)]
    
    var2 <- gsub('_index', '', var)
    
    setnames(max_mat, old=c('var_from', 'max_tot'), new=c(var, paste0(var2, '_max_chord')))
    setkeyv(max_mat, var)
    setkeyv(un_country_attr, var)
    un_country_attr <- un_country_attr[max_mat]
  }
  saveRDS(un_country_attr, '../data/un_country_attributes.rds')
}  
