# International Migration Dashboard

R Shiny application to navigate through the data distributed by the United Nations Department of Economic and Social Affairs

## ETL

Here is what you need to run in your local R console in order to get the data from the online repositories and transform it into a format that is more convenient to use inside the app.

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

un_attr <- setDT(readRDS('../data/un_country_attributes.rds'))
create_countries_polygons(un_attr)


```

### Load

At the very start of the `app/server.R` file, load all the data

```{r}
un_country_attr <- setDT(readRDS(file='../data/un_country_attributes.rds'))
un_country_yearly_attr <- fread('../data/un_country_yearly_attributes.csv')
un_country_yearly_age_attr <- fread('../data/un_country_yearly_age_attributes.csv')
un_migration_flow <- fread('../data/un_migration_flow.csv')

countries_poly <- readOGR(dsn=path.expand("../data/polygons/"), layer='countries')
```


## Application

The shiny app is composed of 2 files located in the `app/` folder:
* `ui.R` handles all the UI related code and mostly depends on [gentelellaShiny](http://code.markedmondson.me/gentelellaShiny/) and [shinyWidgets](https://dreamrs.github.io/shinyWidgets/index.html).
* `server.R` deals with backend changes to update the UI.

## Docker
