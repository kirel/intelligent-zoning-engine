requiredPackages = c('rgdal', 'readr', 'dplyr', 'tidyr', 'leaflet', 'htmltools',
                     'purrr', 'colorspace', 'RColorBrewer', 'ggplot2', 'ggmap',
                     'knitr', 'rmarkdown', 'shiny', 'DT', 'memoise', 'future', 'futile.logger')
for(p in requiredPackages){
  if(!require(p, character.only = TRUE)) install.packages(p, repos='http://cran.r-project.org')
  library(p, character.only = TRUE)
}
