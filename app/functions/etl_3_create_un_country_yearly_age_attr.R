#' @description Creates a table un_country_yearly_age_attr that contains the columns:
#' country | code | 
#' The table is saved as RDS such that the columns are kept as factors upon calling readRDS
#' 
create_un_country_yearly_age_attr <- function(){
  
  
  # 1 Get table index -----------------------------------------------------------------------------
  flog.info('Get table index')
  
  migrant_stock_idx <- read_xlsx(  path='../data_raw/UN_MigrantStockByAgeAndSex_2019.xlsx'
                                 , sheet='Table 1'
                                 , range=c('A16:E1997'))
  migrant_stock_idx <- setDT(as.data.frame(migrant_stock_idx))
  setnames(migrant_stock_idx
            ,old=c('...1', '...5')
            ,new=c('year', 'code'))
  migrant_stock_idx[, c('...2', '...3', '...4'):= NULL]
  
  
  # 2 Get migrant male ----------------------------------------------------------------------------
  flog.info('Get migrant male')
  
  migrant_male <- read_xlsx(  path='../data_raw/UN_MigrantStockByAgeAndSex_2019.xlsx'
                            , sheet='Table 1'
                            , range=c('X16:AM1997'))
  migrant_male <- setDT(as.data.frame(migrant_male))
  migrant_male <- cbind(migrant_stock_idx, migrant_male)
  migrant_male <- melt(  migrant_male
                       , id.vars=c('year', 'code')
                       , variable.name='age_group'
                       , value.name='migrant_male'
                       , variable.factor=TRUE
                       )
  migrant_male[, migrant_male:=as.numeric(migrant_male)]
  
  
  # 3 Get migrant female --------------------------------------------------------------------------
  flog.info('Get migrant female')
  
  migrant_female <- read_xlsx(  path='../data_raw/UN_MigrantStockByAgeAndSex_2019.xlsx'
                              , sheet='Table 1'
                              , range=c('AO16:BD1997'))
  migrant_female <- setDT(as.data.frame(migrant_female))
  migrant_female <- cbind(migrant_stock_idx, migrant_female)
  migrant_female <- melt(  migrant_female
                         , id.vars=c('year', 'code')
                         , variable.name='age_group'
                         , value.name='migrant_female'
                         , variable.factor=TRUE
                         )
  migrant_female[, migrant_female:=as.numeric(migrant_female)]
  
  
  # 4 Get total pop male --------------------------------------------------------------------------
  flog.info('Get total population male')
  
  pop_male <- read_xlsx(  path='../data_raw/UN_MigrantStockByAgeAndSex_2019.xlsx'
                        , sheet='Table 2'
                        , range=c('X16:AM1997'))
  pop_male <- setDT(as.data.frame(pop_male))
  pop_male <- cbind(migrant_stock_idx, pop_male)
  pop_male <- melt(  pop_male
                   , id.vars=c('year', 'code')
                   , variable.name='age_group'
                   , value.name='pop_male'
                   , variable.factor=TRUE
                   )
  pop_male[, pop_male:=as.numeric(pop_male)*1000]
  
  
  # 5 Get total pop female ------------------------------------------------------------------------
  flog.info('Get total population female')
  
  pop_female <- read_xlsx(  path='../data_raw/UN_MigrantStockByAgeAndSex_2019.xlsx'
                          , sheet='Table 2'
                          , range=c('AO16:BD1997'))
  pop_female <- setDT(as.data.frame(pop_female))
  pop_female <- cbind(migrant_stock_idx, pop_female)
  pop_female <- melt(  pop_female
                     , id.vars=c('year', 'code')
                     , variable.name='age_group'
                     , value.name='pop_female'
                     , variable.factor=TRUE
                     )
  pop_female[, pop_female:=as.numeric(pop_female)*1000] 
  
  
  # 6 Merge everything together -------------------------------------------------------------------
  flog.info('Merge everything together ')
  
  setkey(migrant_male, year, code, age_group)
  setkey(migrant_female, year, code, age_group)
  setkey(pop_male, year, code, age_group)
  setkey(pop_female, year, code, age_group)
  
  un_country_yearly_age_attr <- migrant_male[migrant_female][pop_male][pop_female]
  un_country_yearly_age_attr[, country:=countrycode(code, origin='un', destination='country.name')]
  un_country_yearly_age_attr <- un_country_yearly_age_attr[!is.na(code)][code < 900][!is.na(country)]
  
  setcolorder(un_country_yearly_age_attr, c('year', 'code', 'country', 'age_group'))
  
  
  # 7 Writing formatted data  ---------------------------------------------------------------------
  flog.info('Saving un_country_yearly_age_attr in data/un_country_yearly_age_attributes.rds')
  
  # need to use RDS as data.table::fwrite will loose the factor columns
  saveRDS(un_country_yearly_age_attr, '../data/un_country_yearly_age_attributes.rds')
}


  