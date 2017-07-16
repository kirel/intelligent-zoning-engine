requiredPackages = c('igraph', 'rgdal', 'rgeos', 'readr', 'dplyr', 'tidyr', 'leaflet', 'htmltools',
                     'purrr', 'colorspace', 'RColorBrewer', 'ggplot2', 'ggmap', 'ggrepel', 'broom', 'maptools', 'xtable',
                     'knitr', 'rmarkdown', 'shiny', 'DT', 'memoise', 'future', 'futile.logger', 'openxlsx')
for(p in requiredPackages){
  # if(!require(p, character.only = TRUE)) install.packages(p, repos='http://cran.r-project.org')
  library(p, character.only = TRUE)
}
