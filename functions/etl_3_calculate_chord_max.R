
# un_data <- load_migration_data()
# un_attr <- load_country_attributes()

calculate_chord_max <- function(un_country_attr, un_migration_flow){
  # loop over each attributes
  for(var in c('region', 'income_index', 'development_index')){
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
  return(un_country_attr)
}  
