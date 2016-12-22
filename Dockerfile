FROM rocker/shiny

RUN apt-get update; apt-get install -y libgdal-dev libxml2-dev

COPY app/deps.R /deps.R
RUN Rscript --vanilla /deps.R && rm /deps.R

COPY app /srv/shiny-server
