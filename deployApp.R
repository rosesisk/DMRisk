####################################
## tiny script to deploy/update Shiny app
####################################
library(rsconnect)
library(here)


rsconnect::deployApp(here())
