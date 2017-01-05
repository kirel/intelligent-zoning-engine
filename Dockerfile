FROM rocker/shiny

RUN apt-get update; apt-get install -y libgdal-dev libxml2-dev sqlite3

COPY app/deps.R /deps.R
RUN Rscript --vanilla /deps.R && rm /deps.R

RUN usermod -a -G docker shiny
COPY app /srv/shiny-server

