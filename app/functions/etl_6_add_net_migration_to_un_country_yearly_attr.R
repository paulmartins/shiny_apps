add_net_migration_to_un_country_yearly_attr <- function(){
  un_migration_flow <- fread('../data/un_migration_flow.csv')
  # immigrant
  incoming_ppl <- un_migration_flow[,.(immigrants=sum(value, na.rm=TRUE)),.(year, country_to_code)]
  # emigrant
  outgoing_ppl <- un_migration_flow[,.(emigrants=sum(value, na.rm=TRUE)),.(year, country_from_code)]
  
  setkey(incoming_ppl, country_to_code, year)
  setkey(outgoing_ppl, country_from_code, year)
  net_migration_table <- incoming_ppl[outgoing_ppl]
  
  setnames(net_migration_table, old=c('country_to_code'), new=c('code'))
  
  un_country_yearly_attr <- fread('../data/un_country_yearly_attributes.csv')
  initial_col_order <- names(un_country_yearly_attr)
  
  setkey(un_country_yearly_attr, year, code)
  setkey(net_migration_table, year, code)
  un_country_yearly_attr <- net_migration_table[un_country_yearly_attr]
  un_country_yearly_attr[, net_migration:=round((immigrants-emigrants)/(total_pop*1e03),2)]
  setcolorder(un_country_yearly_attr, initial_col_order)
  
  fwrite(un_country_yearly_attr, '../data/un_country_yearly_attributes.csv')
}
