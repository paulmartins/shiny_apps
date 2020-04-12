# International Migration Dashboard

R Shiny application to navigate through the data distributed by the United Nations Department of Economic and Social Affairs

## ETL

To run in your local R console in order to get the data from the online repositories and transform it into a format that is more convenient to use inside the app.

```{r}
setwd('app')
all_functions <- list.files('../functions/')
sapply(file.path('../functions', all_functions), source)
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

un_attr <- setDT(readRDS('../data/un_country_attributes.rds'))
create_countries_polygons(un_attr)
```

### Load

The data is loaded at the very start of the `app/server.R`

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

The shiny app can be run inside a docker container where the code and data have been copied into.

* Build the docker image: `docker build -t un_migration_img .`
* Run the app from docker: `docker run -p 80:3838 un_migration_img`
* Check your browser at the address http://localhost:80/

It is possible to run the app on the docker image without copying the code and data with docker-compose. 
For that comment out the lines `COPY app/ /srv/shiny-server/` and `RUN chmod 777 /usr/bin/shiny-server.sh` in the Dockerfile, rebuilt the image and finally run `docker-compose up`.
