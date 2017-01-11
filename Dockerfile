FROM rocker/shiny

RUN apt-get update; apt-get install -y libgdal-dev libgeos-dev libxml2-dev libgit2-dev libssl-dev libv8-dev texlive texlive-latex-extra

ENV APP_DIR /srv/shiny-server

RUN rm -rf ${APP_DIR}/*
COPY packrat/packrat.lock ${APP_DIR}/packrat/
RUN cd ${APP_DIR} && R -e 'install.packages("packrat"); packrat::restore(prompt=FALSE)'
# creates .Rprofile etc.
RUN cd ${APP_DIR} && R -e 'packrat::packify()'
# creates packrat.opts etc. otherwise shiny server crashes
RUN cd ${APP_DIR} && R -e '1'

# TODO this might be unneccesary
RUN sed -i.bak 's/run_as shiny;/run_as docker;/' /etc/shiny-server/shiny-server.conf
RUN chown docker.docker /var/log/shiny-server
RUN chown docker.docker /srv/shiny-server

COPY app ${APP_DIR}