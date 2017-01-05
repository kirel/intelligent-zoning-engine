FROM rocker/shiny

RUN apt-get update; apt-get install -y libgdal-dev libgeos-dev libxml2-dev texlive texlive-latex-extra

COPY app/deps.R /deps.R
RUN Rscript --vanilla /deps.R && rm /deps.R

RUN sed -i.bak 's/run_as shiny;/run_as docker;/' /etc/shiny-server/shiny-server.conf
COPY app /srv/shiny-server
