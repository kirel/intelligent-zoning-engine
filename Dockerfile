FROM rocker/shiny

RUN apt-get update && apt-get install -y libgdal-dev
RUN R -e "install.packages(c('readr', 'dplyr', 'tidyr', 'leaflet', 'formattable', 'htmltools', 'shiny'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('Cairo'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('purrr'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('DT'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinydashboard'), repos='https://cran.rstudio.com/')"

copy tool /srv/shiny-server