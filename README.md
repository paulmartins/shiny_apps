# Docker for R Shiny Apps

Each folder in this repo contains a shiny app:

* `un_migration`: Navigate through the data distributed by the United Nations Department of Economic and Social Affairs

## Applications

The shiny apps are composed of 2 files:
* `ui.R` handles all the UI related code.
* `server.R` deals with backend changes to update the UI.

## Docker

The shiny apps can be run inside a docker container where the code and data have been copied into.

* Build the docker image: `docker build -t shiny_apps_img .`
* Run the app from docker: `docker run -p 80:3838 shiny_apps_img`
* Check your browser at the address http://localhost:80/

It is possible to run the app on the docker image without copying the code and data with docker-compose. 
For that comment out the lines `COPY app/ /srv/shiny-server/` and `RUN chmod 777 /usr/bin/shiny-server.sh` in the Dockerfile, rebuilt the image and finally run `docker-compose up`.
