#
# global.R
#
# Copyright (C) 2016-17 Kevin Zarca
#
# This program is licensed to you under the terms of version 3 of the
# GNU Affero General Public License. This program is distributed WITHOUT
# ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
#


source("functions.R")
source("interface.R")
options(shiny.port=3456)

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
