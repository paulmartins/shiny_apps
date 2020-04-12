# International Migration Dashboard

R Shiny application to navigate through the data distributed by the United Nations Department of Economic and Social Affairs located in folder `un_migration`.

## ETL

To run in your local R console in order to get the data from the online repositories and transform it into a format that is more convenient to use inside the app.

```{r}
setwd('app')
all_functions <- list.files('functions/')
sapply(file.path('functions', all_functions), source)
```
### Extract

Save all the raw data files into a `/data_raw` folder

```{r}
download_raw_data()
```
### Transform

Create better shaped data to be used by the shiny app in the `/data` folder

```{r}
create_un_country_attr()
create_un_country_yearly_attr()
create_un_country_yearly_age_attr()
create_un_migration_flow()
add_chord_max_to_un_country_attr()
add_net_migration_to_un_country_yearly_attr()

un_attr <- setDT(readRDS('data/un_country_attributes.rds'))
create_countries_polygons(un_attr)
```
### Load

The data is loaded at the very start of the `server.R`

```{r}
un_country_attr <- setDT(readRDS(file='data/un_country_attributes.rds'))
un_country_yearly_attr <- fread('data/un_country_yearly_attributes.csv')
un_country_yearly_age_attr <- fread('data/un_country_yearly_age_attributes.csv')
un_migration_flow <- fread('data/un_migration_flow.csv')
countries_poly <- readOGR(dsn=path.expand("data/polygons/"), layer='countries')
```
