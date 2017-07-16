FROM r-base:3.4.0

RUN apt-get update && apt-get upgrade -y && apt-get install -y texlive texlive-latex-extra
RUN apt-get update && apt-get upgrade -y && apt-get install -y libtiff-dev libgdal-dev libgeos-dev libproj-dev libxml2-dev libgit2-dev libssl-dev libv8-dev

WORKDIR /app

COPY packrat /app/packrat
RUN R -e 'install.packages("packrat" , repos="http://cran.us.r-project.org")'
RUN R -e "0" --args --bootstrap-packrat
RUN R -e 'packrat::restore(restart = FALSE, overwrite.dirty = TRUE)'

COPY app /app/shiny

CMD R -e 'source("packrat/init.R");options(shiny.autoreload=T,shiny.port=3838,shiny.host="0.0.0.0");shiny::runApp("shiny")'
