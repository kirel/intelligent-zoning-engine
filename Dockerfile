FROM rocker/shiny

RUN apt-get update; apt-get install -y libgdal-dev libxml2-dev sqlite3

COPY app/deps.R /deps.R
RUN Rscript --vanilla /deps.R && rm /deps.R

RUN sed -i.bak 's/run_as shiny;/run_as docker;/' /etc/shiny-server/shiny-server.conf
RUN chown docker.docker /var/log/shiny-server

COPY app /srv/shiny-server

