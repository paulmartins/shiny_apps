
# un_data <- load_migration_data()
# un_attr <- load_country_attributes()

calculate_chord_max <- function(un_attr, un_data_original){
  # loop over each attributes
  for(var in c('region', 'income_index', 'development_index')){
    flog.info(paste('Calculating max over years for', var))
    un_data <- copy(un_data_original)
    setkey(un_data, country_to_code)
    setkey(un_attr, code)
    un_data <- un_data[un_attr[,.(code, var_to=get(var))]]
    setkey(un_data, country_from_code)
    un_data <- un_data[un_attr[,.(code, var_from=get(var))]]
    
    un_data[,max_out:=sum(value,na.rm=TRUE),.(year, var_from)]
    un_data[,max_in:=sum(value, na.rm=TRUE),.(year,var_to)]
    un_data[var_to == var_from, max_tot:=max_out+max_in]
    max_mat <- unique(un_data[,.(max_tot=max(max_tot, na.rm=TRUE)/1e06), var_from])[!is.na(var_from)]
    
    var2 <- gsub('_index', '', var)
    
    setnames(max_mat, old=c('var_from', 'max_tot'), new=c(var, paste0(var2,'_max_chord')))
    setkeyv(max_mat, var)
    setkeyv(un_attr, var)
    un_attr <- un_attr[max_mat]
  }
  return(un_attr)
}  
