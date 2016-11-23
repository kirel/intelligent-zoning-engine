#!/usr/bin/sh
R -e "options(shiny.autoreload=T);shiny::runApp('app', port = 3838)"
