#
# ui.R
#
# Copyright (C) 2016-17 Kevin Zarca
#
# This program is licensed to you under the terms of version 3 of the
# GNU Affero General Public License. This program is distributed WITHOUT
# ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
#


jscode2 <- "
shinyjs.init = function() {$(document).on({
'shiny:idle':function(event) {
      $('#lastIdle').val($.now());
    }
  })
}
shinyjs.getLastUpdate = function(){
  $('#lastUpdate').val($.now());
}
shinyjs.restoreTabs = function(){
  while ($('#lastIdle').val() + 500 < $.now()){
    
  }
}
"

jscode <- "
shinyjs.init = function(){
  var maintenant = $.now();
  //Shiny.onInputChange('main', 'tab_transition_matrix');
}
"
function(request) {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "heemod"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "main",
        shinydashboard::menuItem(
          "States", tabName = "tab_states"),
        shinydashboard::menuItem(
          "Transition Matrix", tabName = "tab_transition_matrix"),
        shinydashboard::menuItem(
          "Global Parameters", tabName = "tab_global_parameters"),
        shinydashboard::menuItem(
          "States Parameters", tabName = "tab_states_parameters"),
        shinydashboard::menuItem(
          "Deterministic Sensitivity Analysis", tabName = "tab_dsa"),
        shinydashboard::menuItem(
          "Probabilistic Sensitivity Analysis", tabName = "tab_psa"),
        shinydashboard::menuItem(
          "Results",
          shinydashboard::menuSubItem(
            "Main results",
            tabName = "tab_main_results"
          ),
          shinydashboard::menuSubItem(
            HTML("Probabilistic Sensitivity Analysis"),
            tabName = "tab_psa_results"
          ),
          shinydashboard::menuSubItem(
            HTML("Deterministic Sensitivity Analysis"),
            tabName = "tab_dsa_results"
          )
          )
    )),
    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jscode),
      lang = "en-US",
      shinyjs::inlineCSS(list(
        ".row-eq-height"  = c(
          "display: -webkit-box",
          "display: -webkit-flex",
          "display: -ms-flexbox",
          "display: flex"
        ),
        ".rotateIcon" = c(
          "-webkit-transition: 0.5s ease-in-out",
          "-moz-transition: 0.5s ease-in-out",
          "-o-transition: 0.5s ease-in-out",
          "transition: 0.5s ease-in-out"
        ),
        ".rotateIcon:hover" = c(
          "-webkit-transform: rotate(45deg)",
          "-moz-transform: rotate(45deg)",
          "-o-transform: rotate(45deg)",
          "-ms-transform: rotate(45deg)",
          "transform: rotate(45deg)"
        ),
        ".centerdiv" = c(
          "display: -webkit-box",
          "display: -moz-box",
          "display: -ms-flexbox",
          "display: -webkit-flex",
          "display: flex",
          "-ms-justify-content:center",
          "-webkit-justify-content:center",
          "justify-content:center"
        ),
        ".active > .restoring" = c(
          "color: #b8c7ce !important",
          "border-left-color:#222d32 !important",
          "background:#222d32 !important"
        )
      )),
      uiOutput("masker"),
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "tab_states",
          fluidRow(
            shinydashboard::box(
              numericInput(
                "nbStates",
                label = "Number of States",
                value = "",
                min = 1
              )
            ),
            shinydashboard::box(
              numericInput(
                "nbStrategies",
                label = "Number of Strategies",
                value="",
                min = 1
              )
            )
          ),
          fluidRow(
            uiOutput("nameStates"),
            uiOutput("nameStrategies")
          ),
          conditionalPanel(
            condition = "input.checkShowHelp == 1",
            fluidRow(
              shinydashboard::box(
                style = "background-color: #ffffff;",
                em("Number of distinct states in the model, and their names.")
                
              ),
              shinydashboard::box(
                style = "background-color: #ffffff;",
                em("Number of strategies to compare.")
              )
            )
          )),
        shinydashboard::tabItem(
          tabName = "tab_transition_matrix",    
          fluidRow(
            column(
              12,
              uiOutput("transMatrix1"),
              conditionalPanel(
                condition = "input.nbStrategies > 1",
                column(
                  3,
                  offset=3,
                  actionButton(
                    "copyValuesParametersTM",
                    "Copy values for other strategies"
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              12,
              uiOutput("transMatrix2")
            )
          ),
          conditionalPanel(
            condition = "input.checkShowHelp == 1",
            fluidRow(
              shinydashboard::box(
                style = "background-color: #ffffff;",
                em("Matrix of transition probabilities between states.
               References can be made to parameters computed in the previous tab.
               The sum of probabilities per row must equal 1. The alias "),
                strong("C"),
                em(" (meaning probability complement) means 1 minus the row sum of other probabilities.")
              )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_global_parameters",
          uiOutput("globalParameters")
          
          #uiOutput("allModules")
        ), 
        shinydashboard::tabItem(
          tabName = "tab_states_parameters",
          sidebarLayout(
            sidebarPanel(
              numericInput(
                "nbStateVariables",
                label = "Number of State Variables",
                value = 2,
                min = 2
              ),
              uiOutput("nameStateVariables"),
              wellPanel(
                style = "background-color: #ffffff;",
                em("Number of values attached to states (such as cost, utility...), and their names.
               Names should not contain spaces, start with '.', or special characters.")
              )
            ),
            mainPanel(
              fluidRow(
                column(
                  12,
                  uiOutput("stateParameters1"),
                  conditionalPanel(
                    condition = "input.nbStrategies > 1",
                    column(
                      3,
                      offset=3,
                      actionButton(
                        "copyValuesParametersSP",
                        "Copy values for other strategies"
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  uiOutput("stateParameters2")
                )
              ),
              conditionalPanel(
                condition = "input.checkShowHelp == 1",
                fluidRow(
                  column(
                    4,
                    wellPanel(
                      style = "background-color: #ffffff;",
                      em("Values associated with each state for each strategy.")
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.nbStates > 0 & input.nbStateVariables > 1",
                fluidRow(
                  column(
                    6,
                    uiOutput(
                      "costVariable"
                    )
                  ),
                  column(
                    6,
                    uiOutput(
                      "effectVariable"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.checkShowHelp == 1 & input.nbStates > 0 & input.nbStateVariables > 1",
                fluidRow(
                  column(
                    6,
                    wellPanel(
                      style = "background-color: #ffffff;",
                      em("State variable name representing "),
                      strong("cost"),
                      em(" in the model (or mathematical expression using variable names, such as (var1+var2)/2).")
                    )
                  ),
                  column(
                    6,
                    wellPanel(
                      style = "background-color: #ffffff;",
                      em("State variable name representing "),
                      strong("effect"),
                      em(" in the model (or mathematical expression using variable names, such as (var1+var2)/2).")
                    )
                  )
                )
              )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_dsa",
          fluidRow(
            uiOutput("DSA")
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_psa",
          fluidRow(
            uiOutput("PSA") 
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_main_results",
          sidebarLayout(
            sidebarPanel(
              h3("Model parameters"),
              selectInput(
                "countMethod",
                "Counting method",
                c("beginning", "end", "cycle-tree",
                  "half-cycle", "life-table", "spread-half-cycle"),
                selected = "life-table"
              ),
              numericInput("startAge", value = NA, label = "Age at the beginning"),
              numericInput("cycles", value = 10, label = "Number of cycles"),
              numericInput("cycleDuration", value = NA, label = "Duration of one cycle (years)"),
              conditionalPanel(
                condition = "input.checkShowHelp == 1",
                wellPanel(
                  style = "background-color: #ffffff;",
                  em("Counting method for state membership."),
                  strong("beginning"),
                  em(" assume transitions occur at the beginning of each cycle, "),
                  strong("end"),
                  em(" at the end of each cycle, "),
                  strong("half-cycle"),
                  em(" tries to corrects counts (but actually fails...) by adding 
                 half of the initial count and "),
                  strong("life-table"),
                  em(" assume transitions occur at the middle of each cycle. "),
                  strong("life-table"),
                  em(" is probably the less incorrect method in most situations.")
                )
              ),
              uiOutput("outInit"),
              conditionalPanel(
                condition = "input.checkShowHelp == 1 & input.nbStates > 0",
                wellPanel(
                  style = "background-color: #ffffff;",
                  em("Initial counts per state applied to all models. 
                 Should be a positive number.")
                )
              )
            ),
            mainPanel(
              uiOutput("outModel"),
              DT::dataTableOutput("tableResults"),
              uiOutput("titleICER"),
              DT::dataTableOutput("tableICER"),
              tags$br(),
              plotOutput("plotCE"),
              uiOutput("outCounts"),
              plotOutput("plotCounts"),
              uiOutput("outValues"),
              plotOutput("plotValues")
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_psa_results",
          fluidRow(
            shinydashboard::box(
              width = 4,
              numericInput("nIterations", "Number of iterations", 100, max = 10000, min = 10),
              actionButton("startPSA", "Start", class="btn-success")
              ),
            shinydashboard::box(
              width = 8,
              dataTableOutput("tablePSA")
            )
          ),
          fluidRow(
            column(12, uiOutput("plotCEPlane"))
          ),
          fluidRow(
            column(12, uiOutput("plotACEVPI"))
          ),
          fluidRow(
            column(12, uiOutput("plotCov"))
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_dsa_results",
          uiOutput("outDSA"),
          uiOutput("plotDSA")
        )
      ),
      wellPanel(
        fluidRow(
          column(
            3,
            em("heemod by KZ & AFP"),
            br(),
            tags$a(
              href = "https://github.com/KZARCA/heemod-gui",
              "More info",
              target="_blank"
            )
          ),
          column(
            3,
            checkboxInput(
              "checkShowHelp",
              "Show help",
              value = TRUE
            )
          ),
          column(
            3,
            offset = 3,
            bookmarkButton(label = "Save model")
          )
        )
      )
    )
  )
}



