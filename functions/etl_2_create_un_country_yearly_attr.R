#' @description Creates a table un_country_yearly_attr that contains the columns:
#' country | code | 
#' The table is saved as RDS such that the columns are kept as factors upon calling readRDS
#' 
create_un_country_yearly_attr <- function(){
  
  
  # 1 Get percentage of migrant -------------------------------------------------------------------
  flog.info('Get percentage of migrant')
  
  percent_migrant <- read_xlsx(  path='../data_raw/UN_MigrantStockTotal_2019.xlsx'
                               , sheet='Table 3'
                               , range=c('B16:L299'))
  percent_migrant <- setDT(as.data.frame(percent_migrant))
  setnames( percent_migrant
           ,old=c('...1', '...3')
           ,new=c('country', 'code'))
  percent_migrant[, c('...2', '...4'):= NULL]
  percent_migrant <- percent_migrant[!is.na(code)][code < 900]
  percent_migrant <- melt(  percent_migrant
                          , id.vars=c('country', 'code')
                          , variable.name='year'
                          , value.name='percent_migrant')
  percent_migrant[,percent_migrant:=round(as.numeric(percent_migrant),1)]

  
  # 2 Get percentage of female migrant ------------------------------------------------------------
  flog.info('Get percentage of female migrant')

  percent_f_migrant <- read_xlsx(  path='../data_raw/UN_MigrantStockTotal_2019.xlsx'
                                 , sheet='Table 4'
                                 , range=c('B16:L299'))
  percent_f_migrant <- setDT(as.data.frame(percent_f_migrant))
  setnames(  percent_f_migrant
           , old=c('...1', '...3')
           , new=c('country', 'code'))
  percent_f_migrant[, c('...2', '...4'):= NULL]
  percent_f_migrant <- percent_f_migrant[!is.na(code)][code < 900]
  percent_f_migrant <- melt(  percent_f_migrant
                            , id.vars=c('country', 'code')
                            , variable.name='year'
                            , value.name='percent_f_migrant')
  percent_f_migrant[,percent_f_migrant:=round(as.numeric(percent_f_migrant),1)]
  
  # 3 Get number of refugees ----------------------------------------------------------------------
  flog.info('Get number of refugee')

  total_refugees <- read_xlsx(  path='../data_raw/UN_MigrantStockTotal_2019.xlsx'
                              , sheet='Table 6'
                              , range=c('B16:L299'))
  total_refugees <- setDT(as.data.frame(total_refugees))
  setnames(  total_refugees
           , old=c('...1', '...3')
           , new=c('country', 'code'))
  total_refugees[, c('...2', '...4'):= NULL]
  total_refugees <- total_refugees[!is.na(code)][code < 900]
  total_refugees <- melt(  total_refugees
                         , id.vars=c('country', 'code')
                         , variable.name='year'
                         , value.name='total_refugees')
  total_refugees[,total_refugees:=as.numeric(total_refugees)]
  
  # 4 Get total population ------------------------------------------------------------------------
  flog.info('Get total population') 
  
  total_pop <- read_xlsx(  path='../data_raw/UN_MigrantStockTotal_2019.xlsx'
                         , sheet='Table 2'
                         , range=c('B16:K299'))
  total_pop <- setDT(as.data.frame(total_pop))
  setnames(  total_pop
           , old=c('...1', '...3')
           , new=c('country', 'code'))
  total_pop[, c('...2'):= NULL]
  total_pop <- total_pop[!is.na(code)][code < 900]
  total_pop <- melt(  total_pop
                    , id.vars=c('country', 'code')
                    , variable.name='year'
                    , value.name='total_pop')
  total_pop[,total_pop:=as.numeric(total_pop)/1000] # convert it in million people
    
  # 5 Merge everything ----------------------------------------------------------------------------
  flog.info('Merge everything')
  
  setkey(percent_migrant, country, code, year)
  setkey(percent_f_migrant, country, code, year)
  setkey(total_refugees, country, code, year)
  setkey(total_pop, country, code, year)
    
  un_country_yearly_attr <- percent_migrant[percent_f_migrant][total_refugees][total_pop]
  un_country_yearly_attr[percent_migrant!=0, percent_refugees := round(100*total_refugees/(total_pop*1e06*percent_migrant/100),2)]
  un_country_yearly_attr[is.na(percent_migrant), percent_refugees:=0]
  un_country_yearly_attr[, country:=countrycode(code, origin='un', destination='country.name')]
    
  # 6 Writing formatted data ----------------------------------------------------------------------
  flog.info('Saving un_country_yearly_attr in data/un_country_yearly_attributes.csv')
    
  fwrite(un_country_yearly_attr, '../data/un_country_yearly_attributes.csv')
}

