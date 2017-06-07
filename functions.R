#
# functions.R
#
# Copyright (C) 2016-17 Kevin Zarca
#
# This program is licensed to you under the terms of version 3 of the
# GNU Affero General Public License. This program is distributed WITHOUT
# ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
#

find_unauthorized <- function(list_object, ...){
  dots <- list(...)
  x <- dots$x
  y <- dots$y
  
  list_obj_name <- substitute(list_object) %>%
    paste_(x, y)
  
  unauthorized <- try({
    purrr::compact(
      filter_fun_lazydots(
        lazyeval::as.lazy_dots(
          list_object
        )
      )
    )})
    if ("try-error" %in% class(unauthorized)){
      showNotification(id = paste_("input_error", list_obj_name), type = "error", "Input error")
      return(TRUE)
    } else if(length(unauthorized)){
      purrr::map2_chr(unauthorized, names(unauthorized), function(x, name){
        if (length(x) == 1){
          sprintf("Function %s is not authorized (%s)", x, name)
        } else {
          sprintf("Functions %s are not authorized (%s)", paste(x, collapse = ", "), name)
        } 
      }) %>%
        paste(collapse = "\n") %>%
        showNotification(id = paste_("unauthorized_fun", list_obj_name), ., type = "error", duration = NULL)
      removeNotification(paste_("input_error", list_obj_name))
      return(TRUE)
    } else {
      removeNotification(paste_("unauthorized_fun", list_obj_name))
      removeNotification(paste_("input_error", list_obj_name))
      return(FALSE)
    }
}

filter_fun_lazydots <- function(dots){
  purrr::map(dots, function(x){
    filter_fun(x$expr)
  })
}

filter_fun <- function(expr){
  all_funs <- pryr::fun_calls(expr)
  authorized <- c("+", "-", "/", "*", "^", "log", "log10", "exp", 
                  "sqrt", "median", "max", "min", "ifelse",
                  "==", "!=", "(", ">", "<", ">=", "<=", "::",
                  "heemod", "discount", "get_who_mr")
  all_funs[!all_funs %in% authorized]
}

paste_ <- function(...){
  paste(..., sep = "_")
}

get_names_SA <- function(input, values){
  compact <- purrr::compact
  flatten_chr <- purrr::flatten_chr
  
  equation <- purrr::map(seq_len(values$nEquation)-1, function(i){
    input[[paste0("equationName", i)]]
  }) 
  rgho <- purrr::map(seq_len(values$nRgho)-1, function(i){
    input[[paste0("rghoName", i)]]
  }) 
  survival <- purrr::map(seq_len(values$nSurvival)-1, function(i){
    c(
      if(!is.null(input[[paste0("survivalName", i)]])) paste(input[[paste0("survivalName", i)]], "lambda"),
      if (!is.null(input[[paste0("survivalDistribution", i)]]) && input[[paste0("survivalDistribution", i)]] == "Weibull") paste(input[[paste0("survivalName", i)]], "k")
    )
  })
  timedep <- purrr::map(seq_len(values$nTimedep)-1, function(i){
    if (!is.null(input[[paste0("timedepType", i)]])){
      if (input[[paste0("timedepType", i)]] == "constant") {
        input[[paste0("timedepName", i)]]
      }
      else {
        if(!is.null(values[[paste0("nTimedepNC", i)]])){
          purrr::map(seq.int(0, values[[paste0("nTimedepNC", i)]]), function(j){
            sprintf("%s (%s-%s)", input[[paste0("timedepName", i)]], input[[paste_("timedepStart", i, j)]], input[[paste_("timedepEnd", i, j)]])
          }) %>% compact %>% flatten_chr
        }
      }
    }
      
  })
  return(
    c(equation, rgho, survival, timedep) %>% 
      compact %>%
      flatten_chr %>% 
      sort
    )
}


showStateParam <- function(nbStrat, input, values, click) {
  nbStates <- input$nbStates
  nbStateVariables <- input$nbStateVariables
  
  req(nbStates)
  req(nbStateVariables)
  
  stateName <- ""
  variableStateName <- ""
  
  for (i in seq_len(nbStates)) {
    stateName[i] <- input[[paste0("stateName", i)]]
  }
  
  for (i in seq_len(nbStateVariables)) {
    variableStateName[i] <- input[[paste0("variableStateName", i)]]
  }
  
  if (input$nbStates > 0) {
    start <- ifelse(nbStrat > 1, 2, 1)
    
    lapply(
      seq(from = start, to = nbStrat),
      function(x) {
        tagList(
          h3(paste("State Parameters for", input[[paste0("strategyName",x)]])),
          tags$table(
            class='stateVariables',
            tagList(
              tags$th(),
              lapply(
                seq_len(nbStates),
                function(i) {
                  tags$th(style='text-align:center', stateName[i])
                }),
              tags$th(style='text-align:center', "Discounting Rate"),
              lapply(
                seq_len(nbStateVariables),
                function(i) {
                  tags$tr(
                    tags$td(variableStateName[i]),
                    lapply(
                      seq_len(nbStates),
                      function (j) {
                        isolate({
                          tags$td(textInput(
                            paste_("stateVariable",x,i,j),
                            value = ifelse(click == TRUE, 
                                           ifelse(!is.null(input[[paste_("stateVariable",1,i,j)]]), input[[paste_("stateVariable",1,i,j)]], 0),
                                           ifelse (!is.null(input[[paste_("stateVariable",x,i,j)]]), input[[paste_("stateVariable",x,i,j)]], 
                                                   ifelse(!is.null(input[[paste_("stateVariable",1,i,j)]]),input[[paste_("stateVariable",1,i,j)]],0)
                                           )),
                            label = NULL,
                            width="100%"))
                        })
                      }),
                    isolate({
                      tags$td(numericInput(
                        paste_("discountingRate",x,i),
                        label = NULL,
                        step=1,
                        value = ifelse(click == TRUE, 
                                       ifelse(!is.null(input[[paste_("discountingRate",1,i)]]), input[[paste_("discountingRate",1,i)]], 0),
                                       ifelse(!is.null(input[[paste_("discountingRate",x,i)]]), input[[paste_("discountingRate",x,i)]], 
                                              ifelse(!is.null(input[[paste_("discountingRate",1,i)]]), input[[paste_("discountingRate",1,i)]],0)
                                       )),
                        width="100%"))
                    })
                  )
                })
            )
          )
        )
      })
  }
}

showTransMatrix <- function(nbStrat, input, values, click) {
  nbStates <- input$nbStates
  
  req(nbStates)
  
  stateName <- ""
  start <- ifelse(nbStrat > 1, 2, 1)
  
  for (i in seq_len(nbStates)) {
    stateName[i] <- input[[paste0("stateName", i)]]
  }
  
  if (input$nbStates > 0) {
    tagList(
      lapply(
        seq(from = start, to = nbStrat),
        function(x) {
          tagList(
            h3(paste("Transition Matrix for", input[[paste0("strategyName",x)]])),
            tags$div(
              renderPlot({
                tm <- ux_matrix(input, x)
                if (is.null(tm)) {
                  plot.new()
                  text(.5, .5, "Incorrect\ninput")
                } else {
                  plot(tm)
                }
              },
              width = 200,
              height = 200
              ), style="text-align: center"
            ),
            tags$table(
              style="margin:0 auto;",
              class='transmatrix',
              tagList(
                tags$th(),
                lapply(
                  seq_len(nbStates),
                  function(i) {
                    tags$th(style='text-align:center', stateName[i])
                  }),
                lapply(
                  seq_len(nbStates),
                  function(i) {
                    tags$tr(
                      tags$td(stateName[i]),
                      lapply(
                        seq_len(nbStates),
                        function(j) {
                          isolate(
                            tags$td(textInput(
                              paste_("transmatrix",x,i,j),
                              value = ifelse(click == TRUE, 
                                             ifelse(!is.null(input[[paste_("transmatrix",1,i,j)]]), input[[paste_("transmatrix",1,i,j)]], "0"),
                                             ifelse(!is.null(input[[paste_("transmatrix",x,i,j)]]),input[[paste_("transmatrix",x,i,j)]],
                                                    ifelse(!is.null(input[[paste_("transmatrix",1,i,j)]]), input[[paste_("transmatrix",1,i,j)]], "0")
                                             )),
                              label=NULL,
                              width="100%")))
                        })
                    )
                  })
              )
            ),
            hr()
          )
        })
    )
  }
}

searchRegion <- function(n, input){
  regionNames <- REGION %>%
    attr("labels")
  
  regionNames <- ifelse(
    regionNames == "NA" | grepl("^Not ", regionNames),
    "------",
    regionNames
  )
  
  vRegionCodes <- as.vector(REGION)
  names(vRegionCodes) <- regionNames
  selectizeInput(
    paste0("rghoRegion", n),
    NULL,
    selected = ifelse(!is.null(input[[paste0("rghoRegion", n)]]), input[[paste0("rghoRegion", n)]], "GLOBAL"),
    choices = vRegionCodes
  )
}


searchCountry <- function(n, input){
  req(input[[paste0("rghoRegion", n)]])
  countryCodes <- rgho::filter_gho(
    COUNTRY,
    WHO_REGION_CODE == input[[paste0("rghoRegion", n)]]
  )
  countryNames <- countryCodes %>%
    attr("labels")

  vCountryCodes <- as.vector(c("Global", countryCodes))
  names(vCountryCodes) <- c("Global", countryNames)
  selectizeInput(
    paste0("rghoCountry", n),
    NULL,
    choices = vCountryCodes,
    selected = isolate({ifelse(!is.null(input[[paste0("rghoCountry", n)]]), input[[paste0("rghoCountry", n)]], "GLOBAL")})
  )
}

show_module <- function(module, edit, n, table_title, table_body)  {
  if (edit){
    wellPanel(id = paste0("editing", Hmisc::upFirst(module)),
              fluidRow(id = paste0("editing", Hmisc::upFirst(module), "Title", n), class = "row-eq-height",
                       table_title
              ),
              fluidRow(id = paste0("editing", Hmisc::upFirst(module), "Body", n), class = "row-eq-height",
                       table_body
              ),
              actionButton(paste0(module, "OK"), "OK")
    )
  } else {
    tagList(
      if (n == 0){
        h4(id = paste0(module, "H4"), names(which(MODULES == module)))
      },
      if (n == 0 | !module %in% c("equation", "rgho"))
        fluidRow(id = paste0(module, "Title", n), class = "row-eq-height",
                 table_title
        ),
      fluidRow(id = paste0(module, "Body", n), class = "row-eq-height",
               table_body#, 
               #actionLink(paste0(module,"Delete", n), icon("trash-o", "fa-2x"))
      )
    )
  }
}

show_first <- function(val, FUN, input){
  req(input$nbStates, input$nbStrategies)
  if (val == "SP1")
    req(input$nbStateVariables)
  for (i in seq_len(input$nbStates)){
    req(input[[paste0("stateName", i)]])
  }
  req(input$strategyName1)
  FUN(1, input, values, click = FALSE)
}

copyValues <- function(trigger, input, values, FUN) {
  a <- eventReactive(input[[trigger]],{
    FUN(input$nbStrategies, input, values, TRUE)
  })
  
  return(a())
}

show_next <- function(val, trigger, input, values, FUN){
  req(input$nbStates, input$nbStrategies > 1)
  if (val == "SP2")
    req(input$nbStateVariables)
  
  for (i in seq_len(input$nbStates)){
    req(input[[paste0("stateName", i)]])
  }
  for (i in seq_len(input$nbStrategies)){
    req(input[[paste0("strategyName", i)]])
  }
  input[[trigger]]
  if (input[[trigger]]){
    copyValues(trigger, input = input, values = values, FUN)
  } else {
    FUN(input$nbStrategies, input, values, click = FALSE)
  }
}

### prepare_timedep has a problem with reactivity : there are 2 renderUI imbricated inside 1 renderUI. 
### When one of the renderUI is updated, all the others are invalidated. 

prepare_timedep <- function(n, edit, input, values) {
  table_body <- tagList(
    column(2, textInput(paste0("timedepName", n), NULL, ifelse(!is.null(input[[paste0("timedepName", n)]]), input[[paste0("timedepName", n)]], ""))),
    column(2, selectInput(paste0("timedepType", n), NULL, 
                          choices = c("Constant variation with the number of cycles" = "constant", 
                                                                      "Non-constant variation with the number of cycles since model start" = "nonConstantModelTime", 
                                                                      "Non-constant variation with the number of cycles since entering a state" = "nonConstantStateTime"), 
                          selected = ifelse(!is.null(input[[paste0("timedepType", n)]]), input[[paste0("timedepType", n)]], character(0)))),
    column(8, renderUI({
      fluidRow(
        if (!is.null(input[[paste0("timedepType", n)]])){
          if (input[[paste0("timedepType", n)]] == "constant"){
            column(9,
                   renderUI({
                     fluidRow(
                       column(4, textInput(paste0("timedepValueC", n), NULL,  isolate(ifelse(!is.null(input[[paste0("timedepValueC", n)]]), input[[paste0("timedepValueC", n)]],""))))
                     )
                   })
            )
          } else {
            if (is.null(values[[paste0("nTimedepNC", n)]])) {
              values[[paste0("nTimedepNC", n)]] <- 0
            }
            tagList(
              column(9, 
                     renderUI({
                       lapply(seq.int(0, values[[paste0("nTimedepNC", n)]]), function(i){
                         fluidRow(
                           column(4, textInput(paste_("timedepValueNC", n, i), NULL, isolate(ifelse(!is.null(input[[paste_("timedepValueNC", n, i)]]), input[[paste_("timedepValueNC", n, i)]], "")))),
                           column(4, numericInput(paste_("timedepStart", n, i), NULL, isolate(ifelse(!is.null(input[[paste_("timedepStart", n, i)]]), input[[paste_("timedepStart", n, i)]],"")))),
                           column(4, numericInput(paste_("timedepEnd", n, i), NULL, isolate(ifelse(!is.null(input[[paste_("timedepEnd", n, i)]]), input[[paste_("timedepEnd", n, i)]], ""))))
                         )
                       })
                     })
                     
              ),
              column(2, actionButton(paste0("timedepNew", n), icon("plus")))
            )
          }
        }
      )
    })
    )
  ) 
  table_title <- tagList(
    column(2, strong("Name")),
    column(2, strong("Type of time-dependent variable")),
    column(8, renderUI({
      fluidRow(
        if (!is.null(input[[paste0("timedepType", n)]])){
          if (input[[paste0("timedepType", n)]] == "constant"){
            column(9,
                   fluidRow(
                     column(4, strong("Value"))
                   )
            )
          } else {
            tagList(
              column(9, 
                     fluidRow(
                       column(4, strong("Value")),
                       column(4, strong("Cycle Start")),
                       column(4, strong("Cycle End"))
                     )
              )
            )
          }
          
        }
      )
    }))
  )
  return(list(title = table_title, body = table_body))
}
prepare_rgho <- function(n, edit, input, values) {
  table_body <- tagList(
    isolate(column(3, textInput(paste0("rghoName", n), NULL, ifelse(!is.null(input[[paste0("rghoName", n)]]), input[[paste0("rghoName", n)]], "")))),
    isolate(column(3, selectInput(paste0("rghoGender", n), NULL, choices = c(Female = "FMLE", Male = "MLE", "Both"), selected = input[[paste0("rghoGender", n)]]))),
    column(3, searchRegion(n, input)),
    column(3, renderUI({
      searchCountry(n, input)
    }))
  )
  table_title <- tagList(
    column(3, strong("Name")),
    column(3, strong("Gender")),
    column(3, strong("Region")),
    column(3, strong("Country"))
  )
  return(list(title = table_title, body = table_body))
}

prepare_equation <- function(n, edit, input, values) {
  table_title <- tagList(
    column(2, strong("Name")),
    column(7, strong("Value"))
  )
  table_body <- tagList(
    column(2, textInput(paste0("equationName", n), NULL, ifelse(!is.null(input[[paste0("equationName", n)]]), input[[paste0("equationName", n)]], ""))),
    column(7, textInput(paste0("equationValue", n), NULL, ifelse(!is.null(input[[paste0("equationValue", n)]]), input[[paste0("equationValue", n)]], "")))
  )
  return(list(title = table_title, body = table_body))
}

prepare_survival <- function(n, edit, input, values) {
  table_body <- tagList(
    column(12,
           renderUI({
             fluidRow(
               column(2, textInput(paste0("survivalName", n), NULL, ifelse(!is.null(input[[paste0("survivalName", n)]]), input[[paste0("survivalName", n)]], ""))),
               column(2, selectInput(paste0("survivalDistribution", n), NULL, choices = c("Exponential", "Weibull"), selected = ifelse (!is.null(input[[paste0("survivalDistribution", n)]]), input[[paste0("survivalDistribution", n)]], ""))),
               column(3, textInput(paste0("survivalLambda", n), NULL, 
                                      isolate(ifelse(!is.null(input[[paste0("survivalLambda", n)]]), input[[paste0("survivalLambda", n)]], "")))),
               column(3, 
                      if (!is.null(input[[paste0("survivalDistribution", n)]]) && input[[paste0("survivalDistribution", n)]] == "Weibull"){
                        isolate(textInput(paste0("survivalK", n), NULL, ifelse(!is.null(input[[paste0("survivalK", n)]]), input[[paste0("survivalK", n)]],"")))
                      }
               ))
           })
    )
  )
  table_title <- tagList(
    column(12, 
           fluidRow(
             column(2, strong("Name")), 
             column(2, strong("Distribution")), 
             column(3, strong("Lambda")),
             column(3, renderUI({
               if (!is.null(input[[paste0("survivalDistribution", n)]]) && input[[paste0("survivalDistribution", n)]] == "Weibull"){
                 strong("k")
               }
             }))
           )
    )
  )
  return(list(title = table_title, body = table_body))
}