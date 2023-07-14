library(shiny)
# list all files in data/fcs directory

options(shiny.autoreload = TRUE)
shiny::runApp(".", launch.browser = FALSE, port = 1337)
