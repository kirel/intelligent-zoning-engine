FROM rocker/shiny

RUN apt-get update; apt-get install -y libgdal-dev libgeos-dev libxml2-dev texlive texlive-latex-extra

COPY app/deps.R /deps.R
RUN Rscript --vanilla /deps.R && rm /deps.R

COPY app /srv/shiny-server
