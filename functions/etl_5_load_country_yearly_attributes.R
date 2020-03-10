
load_country_yearly_attributes <- function(force_download = FALSE, force_format = FALSE){
  
  if(force_download){
    
    # 1 Downloading raw migration data ------------------------------------------------------------
    flog.info('Downloading raw migration data') 
    
    url <- 'https://www.un.org/en/development/desa/population/migration/data/estimates2/data/UN_MigrantStockTotal_2019.xlsx'
    download.file(url, '../data/UN_MigrantStockTotal_2019.xlsx')
  }
  
  
  if(force_format | force_download){
    # 2 Formatting raw attributes data -------------------------------------------------------------
    
    # 2.1 Get percentage of migrant ----------------------------------------------------------------
    percent_migrant <- read_xlsx(  path='../data/UN_MigrantStockTotal_2019.xlsx'
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
    percent_migrant[,percent_migrant:=as.numeric(percent_migrant)]

    # 2.2 Get percentage of female migrant --------------------------------------------------------
    
    percent_f_migrant <- read_xlsx(  path='../data/UN_MigrantStockTotal_2019.xlsx'
                                   , sheet='Table 4'
                                   , range=c('B16:L299'))
    percent_f_migrant <- setDT(as.data.frame(percent_f_migrant))
    setnames( percent_f_migrant
              ,old=c('...1', '...3')
              ,new=c('country', 'code'))
    percent_f_migrant[, c('...2', '...4'):= NULL]
    percent_f_migrant <- percent_f_migrant[!is.na(code)][code < 900]
    percent_f_migrant <- melt(  percent_f_migrant
                              , id.vars=c('country', 'code')
                              , variable.name='year'
                              , value.name='percent_f_migrant')
    percent_f_migrant[,percent_f_migrant:=as.numeric(percent_f_migrant)]
    
    # 2.3 Get number of refugee -------------------------------------------------------------------
    
    total_refugees <- read_xlsx(  path='../data/UN_MigrantStockTotal_2019.xlsx'
                                  , sheet='Table 6'
                                  , range=c('B16:L299'))
    total_refugees <- setDT(as.data.frame(total_refugees))
    setnames( total_refugees
              ,old=c('...1', '...3')
              ,new=c('country', 'code'))
    total_refugees[, c('...2', '...4'):= NULL]
    total_refugees <- total_refugees[!is.na(code)][code < 900]
    total_refugees <- melt(  total_refugees
                           , id.vars=c('country', 'code')
                           , variable.name='year'
                           , value.name='total_refugees')
    total_refugees[,total_refugees:=as.numeric(total_refugees)]
    
    # 2.4 Get total population --------------------------------------------------------------------
    
    total_pop <- read_xlsx(  path='../data/UN_MigrantStockTotal_2019.xlsx'
                           , sheet='Table 2'
                           , range=c('B16:K299'))
    total_pop <- setDT(as.data.frame(total_pop))
    setnames( total_pop
              ,old=c('...1', '...3')
              ,new=c('country', 'code'))
    total_pop[, c('...2'):= NULL]
    total_pop <- total_pop[!is.na(code)][code < 900]
    total_pop <- melt(  total_pop
                      , id.vars=c('country', 'code')
                      , variable.name='year'
                      , value.name='total_pop')
    total_pop[,total_pop:=as.numeric(total_pop)]
    
    # 2.5 Merge everything ------------------------------------------------------------------------
    
    setkey(percent_migrant, country, code, year)
    setkey(percent_f_migrant, country, code, year)
    setkey(total_refugees, country, code, year)
    setkey(total_pop, country, code, year)
    
    final_table <- percent_migrant[percent_f_migrant][total_refugees][total_pop]
    final_table[, total_pop := total_pop/1000]
    final_table[, percent_refugees := total_refugees/(total_pop*1e06)]
    
    # 2.6 Writing formatted data ------------------------------------------------------------------
    flog.info('Writing formatted data')
    
    fwrite(final_table, '../data/un_country_yearly_attributes.csv')
  }
  
  # 3 Read formatted data -----------------------------------------------------------------------
  flog.info('Read formatted data')
  
  return(fread('../data/un_country_yearly_attributes.csv'))
}

