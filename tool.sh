#!/usr/bin/sh
R -e "options(shiny.autoreload=T);shiny::runApp('tool', port = 3838)"
