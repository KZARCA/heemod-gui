source("functions.R")
source("interface.R")

library(dplyr)

REGION <- NULL
COUNTRY <- NULL

try({
  REGION <- rgho::get_gho_codes(dimension = "REGION")
  COUNTRY <- rgho::get_gho_codes(dimension="COUNTRY")
})

MODULES <- c(
  "Simple equation" = "equation",
  "WHO mortality rate" = "rgho",
  "Survival modeling" = "survival",
  "Time-dependant variable" = "timedep"
)

enableBookmarking(store = "server")
observe_show_module_timedep <- list()
observe_show_module_survival <- list()
