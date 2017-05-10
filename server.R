#
# server.R
#
# Copyright (C) 2016-17 Kevin Zarca
#
# This program is licensed to you under the terms of version 3 of the
# GNU Affero General Public License. This program is distributed WITHOUT
# ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
#


shinyServer(function(input, output, session) {
  values <- reactiveValues(nGlobalParameters = 1, nEquation = 0, nRgho = 0, nSurvival = 0, 
                           nTimedep = 0, nDeterministic = 0, nProbabilistic = 0, moduleEdit = FALSE,
                           removeDSA = FALSE, removePSA = FALSE)
  local_values <- reactiveValues(restoring = 0, restoring_time = Sys.time(), show_masker = FALSE, all_updated = FALSE)

  onBookmark(function(state) {
    nameValues <- names(reactiveValuesToList(values))
    purrr::walk(nameValues, function(x){
      state$values[[x]] <- values[[x]]
    })
  })
  
  onRestore(function(state) {
    nameValues <- ls(state$values)
    purrr::walk(nameValues, function(x){
      values[[x]] <- state$values[[x]]
    })
    local_values$last_tab <- input$main
    
  })
  
  onRestored(function(state) {
    output_names <- names(outputOptions(output))
    output_names <- output_names[-grep("debug", output_names)]
    except <- c(MODULES, "DSA", "DSAtable", "PSA", "PSAtable")
    output_names <- output_names[!output_names %in% except]
    for (out in output_names){
      outputOptions(output, out, suspendWhenHidden = FALSE)
    }
    local_values$restoring <- 1
    local_values$restoring_time <- Sys.time()
  })
  
  
  observe({
    req(local_values$restoring > 0)
    local_values$order_restore <- c("tab_states", "tab_transition_matrix", "tab_global_parameters", "tab_states_parameters", "tab_dsa", "tab_psa", local_values$last_tab)
    local_values$final_restore <- length(local_values$order_restore) + 1
    purrr::walk2(seq_len(local_values$final_restore - 1), local_values$order_restore, function(x, y){
      observe({
        if (local_values$restoring == x){
          invalidateLater(100)
          if (Sys.time() - local_values$restoring_time > 1){
            shinydashboard::updateTabItems(session, "main", y )
            local_values$restoring <- x + 1
            local_values$restoring_time <- Sys.time()
          }
        }
      })
    })
  })
  
  observe({
    req(local_values$restoring > 0)
    if(local_values$restoring == local_values$final_restore) {
      local_values$show_masker <- FALSE
    } else {
      local_values$show_masker <- TRUE
    }
  })
  
  output$masker <- renderUI({
    if(local_values$show_masker){
      tagList(
        div(class = "centerdiv", style = "width:100%; position:relative; background-color:#ecf0f5; z-index:2",
            div("Restoring...", icon("refresh", class = "fa-spin fa-2x"))
        ),
        div(style = "width:100%; height:1000px;position:absolute; background-color:#ecf0f5; z-index:2000"
        )
      )
    }
  })
  
  observe({
    if(local_values$show_masker){
      shinyjs::addClass(class = "restoring", selector = "#main li > a")
    } else {
      shinyjs::removeClass(class = "restoring", selector = "#main li > a")
    }
  })
  
  setBookmarkExclude(
    c(
      paste0(MODULES, "OK"),
      "newParam",
      unname(MODULES),
      "addDeterministic",
      "addProbabilistic"
    )
  )
  
  observe({ ##?WHY??
    output$searchCountry <- renderUI({
      n <- values$nRho
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
        selected = ifelse(!is.null(input[[paste0("rghoCountry", n)]]), input[[paste0("rghoCountry", n)]], "GLOBAL")
      )
    })
  })
  
  observe_timedepNew <- list()
  
  observe_nTimedep <- observe({
    lapply(seq.int(0, values$nTimedep), function(n){
      isolate({
        if (purrr::map_lgl(observe_timedepNew[n + 1], is.null)){ #I would have prefered : if (is.null(observe_timedepNew[[n+1]]))
          observe_timedepNew[[n + 1]] <<- observeEvent(input[[paste0("timedepNew", n)]], {
            if (is.null(values[[paste0("nTimedepNC", n)]]))
              values[[paste0("nTimedepNC", n)]] <- 1
            else 
              values[[paste0("nTimedepNC", n)]] <- values[[paste0("nTimedepNC", n)]] + 1
          })
        }
      })
    })
  }) 
  
  
  output$nameStates <- renderUI({
    req(input$nbStates)
    lapply(
      seq_len(input$nbStates),
      function(i) {
        isolate({
          textInput(
            paste0("stateName", i),
            paste("State Name", i),
            value = ifelse(
              !is.null(input[[paste0("stateName",i)]]),
              input[[paste0("stateName",i)]],
              LETTERS[i]))
        })
      }) %>% shinydashboard::box()
  })
  
  output$nameStateVariables <- renderUI({
    req(input$nbStateVariables)
    
    lapply(
      seq_len(input$nbStateVariables),
      function(i) {
        isolate({
          textInput(
            paste0("variableStateName", i),
            paste("Variable Name", i),
            value = ifelse(
              !is.null(input[[paste0("variableStateName",i)]]),
              input[[paste0("variableStateName",i)]],
              if (i == 1)
                "cost"
              else if (i==2)
                "outcome"
              else
                paste0("variable_",i)))
        })
      }) 
  })
  
  output$nameStrategies <- renderUI({
    req(input$nbStrategies)
    
    lapply(
      seq_len(input$nbStrategies),
      function(i) {
        isolate({
          textInput(
            paste0("strategyName", i),
            paste("Strategy Name", i),
            value = ifelse(
              !is.null(input[[paste0("strategyName",i)]]),
              input[[paste0("strategyName",i)]],
              as.character(as.roman(i))))
        })
      }) %>% shinydashboard::box()
  })
  
  
  output$transMatrix1 <- renderUI({
    show_first(val = "TM1", FUN = showTransMatrix, input)    
  })
  
  output$transMatrix2 <- renderUI({
    show_next(val = "TM2", trigger = "copyValuesParametersTM", input, values, showTransMatrix)
    
  })  
  
  output$stateParameters1 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_first(val = "SP1", FUN = showStateParam, input)
  })
  
  output$stateParameters2 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_next(val = "SP2", trigger = "copyValuesParametersSP", input, values, showStateParam)
  })
  
  
  
  output$costVariable <- renderUI({
    textInput(
      "costVariable",
      label = "Cost Variable",
      value = ifelse(!is.null(input$costVariable), input$costVariable, ifelse(!is.null(input$variableStateName1), input$variableStateName1, NA))
    )
  })
  output$effectVariable <- renderUI({
    textInput(
      "effectVariable",
      label = "Effect Variable",
      value = ifelse(!is.null(input$effectVariable), input$effectVariable, ifelse(!is.null(input$variableStateName2), input$variableStateName2, NA))
    )
  })
  
  observeEvent(input$addParametersGP, {
    isolate(values$nGlobalParameters <- values$nGlobalParameters + 1)
  })

  
  output$DSA <- renderUI({
    req(sum(c(values$nEquation, values$nRgho,
              values$nSurvival, values$nTimedep)) > 0)
    # req(length(choices) > 0)
    i <- 0
    tagList(
      column(
        12,
        uiOutput("DSAtable")
      )
      ,
        column(
          12,
          div(
            class="centerdiv",
            actionButton(
              "addDeterministic", "Add a DSA value")
          ))
    )
  })
  
  observe({
    n <- values$nDeterministic
    shinyjs::toggleState("addDeterministic",
                         condition = n == 0 | (n < length(get_names_SA(input, values)) &
                           !is.null(input[[paste0("minDSAValue", n)]]) && !is.na(input[[paste0("minDSAValue", n)]]) &
                           !is.null(input[[paste0("maxDSAValue", n)]]) && !is.na(input[[paste0("maxDSAValue", n)]])))
  })
  
  observeEvent(input$addDeterministic, {
    values$nDeterministic <- values$nDeterministic + 1
  })
  
  output$DSAtable <- renderUI({
    req(sum(c(values$nEquation, values$nRgho,
              values$nSurvival, values$nTimedep)) > 0, values$nDeterministic > 0)
    choices <- get_names_SA(input, values)
    lapply(seq.int(1, values$nDeterministic), function(i){
      if (i > 1){
        old_choices <- purrr::map(paste0("DSAGlobalParamName", seq.int(1, i - 1)), ~ input[[.]])
        rem_choices <- choices[-which(choices %in% old_choices)]
      } else  rem_choices <- choices
      var_name <- if (i == 1) "Variable name" else NULL
      max_val <- if (i == 1) "Minimum value" else NULL
      min_val <- if (i == 1) "Maximum value" else NULL
      style_trash <- if(i == 1) "margin-top:25px" else NULL
      isolate({
        div(id = paste0("DSA_div", i), class="centerdiv",
            selectizeInput(paste0("DSAGlobalParamName", i), var_name, choices = rem_choices, selected = ifelse(!is.null(input[[paste0("DSAGlobalParamName", i)]]), input[[paste0("DSAGlobalParamName", i)]], "")),
            numericInput(paste0("minDSAValue", i), max_val, ifelse(!is.null(input[[paste0("minDSAValue", i)]]), input[[paste0("minDSAValue", i)]], "")),
            numericInput(paste0("maxDSAValue", i), min_val, ifelse(!is.null(input[[paste0("maxDSAValue", i)]]), input[[paste0("maxDSAValue", i)]], "")),
            actionLink(paste0("deleteDSA", i), label = NULL, icon = icon("trash-o", class = "fa-2x"), style = style_trash)
        )
      })
    })
  })
  
  observe({
    lapply(seq.int(1, values$nDeterministic), function(n){
      observeEvent(input[[paste0("deleteDSA", n)]], {
        choices <- get_names_SA(input, values)
        if (n < values$nDeterministic){
          for(i in seq.int(n, values$nDeterministic)){
            var_name <- if (i == 1) "Variable name" else NULL
            max_val <- if (i == 1) "Minimum value" else NULL
            min_val <- if (i == 1) "Maximum value" else NULL
            updateSelectizeInput(session, paste0("DSAGlobalParamName", i), var_name, choices = choices, selected = input[[paste0("DSAGlobalParamName", i + 1)]])
            updateNumericInput(session, paste0("minDSAValue", i), max_val, input[[paste0("minDSAValue", i + 1)]])
            updateNumericInput(session, paste0("maxDSAValue", i), min_val, input[[paste0("maxDSAValue", i + 1)]])
          }
        } else{
          var_name <- if (n == 1) "Variable name" else NULL
          max_val <- if (n == 1) "Minimum value" else NULL
          min_val <- if (n == 1) "Maximum value" else NULL
        }
        updateNumericInput(session, paste0("minDSAValue", values$nDeterministic), label = max_val, value = NA)
        updateNumericInput(session, paste0("maxDSAValue", values$nDeterministic ), label = min_val, value = NA)
        values$removeDSA <- TRUE
        
      }, once = TRUE, ignoreInit = TRUE)
    })
  })
  
  
  observe({ ### Workaround to allow enough time for updateNumericInput to reset
    if (values$removeDSA == TRUE){
      req(is.na(input[[paste0("minDSAValue", values$nDeterministic)]]), is.na(input[[paste0("maxDSAValue", values$nDeterministic)]]))
      values$removeDSA <- FALSE
      isolate(
        values$nDeterministic <- ifelse(values$nDeterministic > 0, values$nDeterministic - 1, 0)
      )
    }
  })
  
  
  output$PSA <- renderUI({
    req(sum(c(values$nEquation, values$nRgho, values$nSurvival, values$nTimedep)) > 0)
    # isolate(choices <- get_names_SA(input, values))
    # req(length(choices) > 0)
    
    i = 0
    tagList(
      column(
        12,
        uiOutput("PSAtable")
      ),
      column(
        12,
        div(class="centerdiv",
            actionButton(
              "addProbabilistic", "Add a PSA value")
        ))
    )
  })
  
  output$PSAtable <- renderUI({
    req(sum(c(values$nEquation, values$nRgho,
              values$nSurvival, values$nTimedep)) > 0, values$nProbabilistic > 0)
    choices <- get_names_SA(input, values)
    req(length(choices) > 0)
    
    #req(values$nProbabilistic > 0)
    lapply(seq.int(1, values$nProbabilistic), function(i){
      if (i > 1){
        old_choices <- purrr::map(paste0("PSAGlobalParamName", seq.int(1, i - 1)), ~ input[[.]])
        rem_choices <- choices[-which(choices %in% old_choices)]
      } else rem_choices <- choices
      tagList(
        column(2, selectizeInput(paste0("PSAGlobalParamName", i), "Variable name" , choices = rem_choices, selected = ifelse(!is.null(input[[paste0("PSAGlobalParamName", i)]]), input[[paste0("PSAGlobalParamName", i)]], ""))),
        column(2, selectizeInput(paste0("PSADistrib", i), "Distribution", choices = c("", "Normal", "Lognormal", "Binomial", "Gamma", "Logitnormal", "Multinomial"), selected = ifelse(!is.null(input[[paste0("PSADistrib", i)]]), input[[paste0("PSADistrib", i)]], character(0)))),
        renderUI({
          req(stringr::str_length(input[[paste0("PSADistrib", i)]]) > 1)
            psa_param1 <- switch (input[[paste0("PSADistrib", i)]],
                                  "Normal" = "Mean",
                                  "Lognormal" = "Mean",
                                  "Binomial" = "Prop",
                                  "Gamma" = "Mean",
                                  "Logitnormal" = "Mu",
                                  "Multinomial" = "Nb parameters"
            ) 
            psa_param2 <- switch (input[[paste0("PSADistrib", i)]],
                                  "Normal" = "SD",
                                  "Lognormal" = "SD",
                                  "Binomial" = "Size",
                                  "Gamma" = "SD",
                                  "Logitnormal" = "Sigma"
            )
            tagList(
              renderUI({
                n_i <- i
                tagList(
                  isolate(column(2, numericInput(paste0("PSAParam1", i), psa_param1, ifelse(!is.null(input[[paste0("PSAParam1", i)]]), input[[paste0("PSAParam1", i)]], NA)))),
                  if (isolate(input[[paste0("PSADistrib", i)]] != "Multinomial")){
                    tagList(
                    isolate(column(2, numericInput(paste0("PSAParam2", i), psa_param2, ifelse(!is.null(input[[paste0("PSAParam2", i)]]), input[[paste0("PSAParam2", i)]], NA)))),
                    if (isolate(input[[paste0("PSADistrib", i)]] == "Lognormal")){
                      isolate(column(2, style = "margin-top:20px", checkboxInput(paste0("PSALogscale", i), "Check if mean and sd are on the log scale", value = ifelse(!is.null(input[[paste0("PSALogscale", i)]]), input[[paste0("PSALogscale", i)]], FALSE))))
                    }
                    )
                  } else {
                    if(!is.null(input[[paste0("PSAParam1", i)]]) && !is.na(input[[paste0("PSAParam1", i)]]) && input[[paste0("PSAParam1", i)]] > 0){
                    isolate({
                      if (!is.null(local_values$other_to_multinom) && i == local_values$other_to_multinom){
                        n_i <- local_values$other_to_multinom + 1
                        local_values$other_to_multinom <- NULL
                      } else n_i <- i
                    })
                      
                      #isolate({
                      column(4,
                             lapply(seq_len(isolate(input[[paste0("PSAParam1", i)]])), function(j){
                               isolate({
                               fluidRow(
                                 column(
                                   6,
                                   if (j == 1) {
                                     textInput(
                                       paste_("PSAMultinomName", i, j),
                                       label = "Parameter",
                                       value = input[[paste0("PSAGlobalParamName", i)]]) %>%
                                       shinyjs::disabled()
                                   }
                                   else {
                                     selectInput(
                                       paste_("PSAMultinomName", i, j),
                                       label = "Parameter",
                                       choices = choices,
                                       selected = ifelse(
                                         !is.null(input[[paste_("PSAMultinomName", n_i, j)]]),
                                         input[[paste_("PSAMultinomName", n_i, j)]],
                                         "")
                                       )
                                   }
                                 ),
                                 column(
                                   6,
                                   numericInput(
                                     paste_("PSAMultinomValue", i, j),
                                     label = "Value",
                                     value = ifelse(
                                       !is.null(input[[paste_("PSAMultinomValue", n_i, j)]]),
                                       input[[paste_("PSAMultinomValue", n_i, j)]],
                                       NA
                                     )))
                               )
                               })
                             })
                      )
                      #})
                    }
                  }
                )
              })
            )
        }),
        column(1, actionLink(paste0("deletePSA", i), label = NULL, icon = icon("trash-o", class = "fa-2x")), style = "margin-top:25px")
      ) %>% fluidRow

    })
  })
  
  observe({
    n <- values$nProbabilistic
    shinyjs::toggleState("addProbabilistic", 
                         condition =  n == 0 | (n < length(get_names_SA(input, values)) & 
                           !is.null(input[[paste0("PSAParam1", n)]]) && !is.na(input[[paste0("PSAParam1", n)]])))
  })
  
  observeEvent(input$addProbabilistic, {
    values$nProbabilistic <- values$nProbabilistic + 1
  })
  
  
  observe({
    lapply(seq.int(1, values$nProbabilistic), function(n){
      observeEvent(input[[paste0("deletePSA", n)]], {
        choices <- get_names_SA(input, values)
        if (n < values$nProbabilistic){
          local_values$old_param1 <- input[[paste0("PSAGlobalParamName", n + 1)]]
          for(i in seq.int(n, values$nProbabilistic - 1)){
            psa_param1 <- switch (input[[paste0("PSADistrib", i)]],
                                  "Normal" = "Mean",
                                  "Lognormal" = "Mean",
                                  "Binomial" = "Prop",
                                  "Gamma" = "Mean",
                                  "Logitnormal" = "Mu",
                                  "Multinomial" = "Nb parameters"
            ) 
            psa_param2 <- switch (input[[paste0("PSADistrib", i)]],
                                  "Normal" = "SD",
                                  "Lognormal" = "SD",
                                  "Binomial" = "Size",
                                  "Gamma" = "SD",
                                  "Logitnormal" = "Sigma"
            )
            updateSelectizeInput(session, paste0("PSAGlobalParamName", i), "Variable name" , choices = choices, selected = ifelse(!is.null(input[[paste0("PSAGlobalParamName", i + 1)]]), input[[paste0("PSAGlobalParamName", i + 1)]], ""))
            updateSelectizeInput(session, paste0("PSADistrib", i), "Distribution", choices = c("Normal", "Lognormal", "Binomial", "Gamma", "Logitnormal", "Multinomial"), selected = ifelse(!is.null(input[[paste0("PSADistrib", i + 1)]]), input[[paste0("PSADistrib", i + 1)]], character(0)))
            updateNumericInput(session, paste0("PSAParam1", i), psa_param1, ifelse(!is.null(input[[paste0("PSAParam1", i + 1)]]), input[[paste0("PSAParam1", i + 1)]], NA))
            if (input[[paste0("PSADistrib", i)]] != "Multinomial" & input[[paste0("PSADistrib", i + 1)]] != "Multinomial"){
              updateNumericInput(session, paste0("PSAParam2", i), psa_param2, ifelse(!is.null(input[[paste0("PSAParam2", i + 1)]]), input[[paste0("PSAParam2", i + 1)]], NA))
              local_values$removePSA <- n
            } else if (input[[paste0("PSADistrib", i + 1)]] == "Multinomial") {
              local_values$other_to_multinom <- i
              local_values$removePSA <- n
            } 
          }
        } else {
          local_values$removePSA <- values$nProbabilistic
        }
      }, once = TRUE, ignoreInit = TRUE)
    })
  })
  
  observe({
    req(local_values$removePSA)
    if (is.null(local_values$other_to_multinom)){
      psa_param1 <- switch (input[[paste0("PSADistrib", values$nProbabilistic)]],
                            "Normal" = "Mean",
                            "Lognormal" = "Mean",
                            "Binomial" = "Prop",
                            "Gamma" = "Mean",
                            "Logitnormal" = "Mu",
                            "Multinomial" = "Nb parameters"
      )
      psa_param2 <- switch (input[[paste0("PSADistrib", values$nProbabilistic)]],
                            "Normal" = "SD",
                            "Lognormal" = "SD",
                            "Binomial" = "Size",
                            "Gamma" = "SD",
                            "Logitnormal" = "Sigma"
      )
      choices <- get_names_SA(input, values)
      updateSelectizeInput(session, paste0("PSAGlobalParamName", values$nProbabilistic), "Variable name" , choices = choices, selected = character(0))
      updateSelectizeInput(session, paste0("PSADistrib", values$nProbabilistic), "Distribution", choices = c("Normal", "Lognormal", "Binomial", "Gamma", "Logitnormal", "Multinomial"), selected = character(0))
      updateNumericInput(session, paste0("PSAParam1", values$nProbabilistic), psa_param1, NA)
      
      
      if (input[[paste0("PSADistrib", values$nProbabilistic)]] != "Multinomial"){
        updateNumericInput(session, paste0("PSAParam2", values$nProbabilistic), psa_param2, NA)
      } else {
        for (i in seq_len(input[[paste0("PSAParam1", values$nProbabilistic)]])){
          updateTextInput(session, paste_("PSAMultinomName", values$nProbabilistic,i), psa_param1, "")
          updateNumericInput(session, paste_("PSAMultinomValue", values$nProbabilistic,i), psa_param1, NA)
        }
      }
      local_values$all_updated <- TRUE
    }
  })
  
  observe({
    req(local_values$removePSA, local_values$all_updated)
    if (stringr::str_length(input[[paste0("PSAGlobalParamName", values$nProbabilistic)]]) == 0 & 
        (input[[paste0("PSADistrib", values$nProbabilistic)]] != "Multinomial" || is.na(input[[paste_("PSAMultinomValue", values$nProbabilistic, 1)]]))){
      local_values$old_param1 <- NULL
      local_values$removePSA <- NULL
      local_values$all_updated <- FALSE
      isolate(
        values$nProbabilistic <- ifelse(values$nProbabilistic > 0, values$nProbabilistic - 1, 0)
      )
    }
  })

  output$outInit <- renderUI({
    #####
    req(
      nbState <- ux_nb_states(input),
      stateNames <- ux_state_names(input)
    )
    tagList(
      tags$h3("Initial counts per state"),
      tags$table(
        tagList(
          list(
            tags$th(""),
            tags$th(style='text-align:center', "Count")
          ),
          lapply(
            seq_len(nbState),
            function(i) {
              tags$tr(
                list(
                  tags$td(
                    strong(stateNames[i])),
                  tags$td(
                    if (i == 1) {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = 1000,
                        width="100%"
                      )
                    } else {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = 0,
                        width="100%"
                      )})))}))))
  })
  
  output$outModel <- renderUI({
    #####
    req(input$init1)
    input$reload_results
    isolate(values$model <- ux_run_models(input = input, values = values))
    values$summary_model <- summary(values$model)
    if (is.null(values$model)) {
      tagList(tags$h3("Model specification incomplete"))
    } else {
      tagList(
        tags$h1("Model results"),
        tags$h3("Total values")
      )
    }
  })
  
  output$tableResults <- DT::renderDataTable({
    #####
    req(values$model)
    req(values$summary_model$res_values)
    
    DT::datatable(
      values$summary_model$res_values,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  output$titleICER <- renderUI({
    #####
    req(values$model)
    req(values$summary_model$res_comp)
    
    tagList(
      tags$h3("Efficiency frontier"),
      tags$p(paste(values$summary_model$frontier, collapse = " -> ")),
      tags$h3("Model comparison")
    )
  })
  # 
  output$plotCE <- renderPlot({
    req(values$model)
    req(values$summary_model$res_comp)
    plot(values$model, type = "ce")
  },
  width = 600)
  
  output$tableICER <- DT::renderDataTable({
    #####
    req(values$model)
    req(values$summary_model$res_comp)
    
    DT::datatable(
      values$summary_model$res_comp,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  output$outCounts <- renderUI({
    #####
    
    req(values$model)
    
    tagList(
      tags$h3("State membership count"),
      selectInput(
        inputId = "modelPlotCounts",
        label = "Panneling",
        choices = c("By strategy" = "by_strategy",
                    "By state" = "by_state")
      ),
      checkboxInput("checkPlotCounts", "Free-Y")
    )
  })
  
  output$plotCounts <- renderPlot({
    #####
    req(values$model)
    panels <- input$modelPlotCounts
    req(panels)
    plot(
      values$model,
      type = "counts",
      panels = panels,
      free_y = input$checkPlotCounts
    ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_colour_brewer(
        name = "Count",
        palette = "Set1"
      )
  },
  width = 600)
  
  output$outValues <- renderUI({
    #####
    
    req(values$model)
    
    tagList(
      tags$h3("State values"),
      selectInput(
        inputId = "modelPlotValues",
        label = "Panneling",
        choices = c("By strategy" = "by_strategy",
                    "By value" = "by_value")
      ),
      checkboxInput("checkPlotValues", "Free-Y")
    )
  })
  
  output$plotValues <- renderPlot({
    #####
    req(values$model)
    panels <- input$modelPlotValues
    req(panels)
    plot(
      values$model,
      type = "values",
      panels = panels,
      free_y = input$checkPlotValues
    ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_colour_brewer(
        name = "Value",
        palette = "Set1"
      )
  },
  width = 600)
  
  output$outDSA <- renderUI({
    req(values$nDeterministic > 0)
    req(values$model)
    
    
    tagList(
      selectInput(
        inputId = "dsaPlotResult",
        label = "Result to plot",
        choices = c(Cost = "cost", Effect = "effect", ICER = "icer")
      ),
      selectInput(
        inputId = "dsaPlotType",
        label = "Type of result",
        choices = c(Simple = "simple", Difference = "difference")
      )
    )
  })
  
  output$plotDSA <- renderUI({
    req(values$nDeterministic > 0)
    req(values$model)
    tagList(
      renderPlot({
        values$dsa <- ux_run_dsa(input, values)
        plot(values$dsa,
             result = input$dsaPlotResult,
             type = input$dsaPlotType)
      })
    )
  })
  
  for (module in MODULES){
    edit <- FALSE
    output[[module]] <- renderUI({ # I can't figure out how to use the variable "module" inside the renderUI. I tried parent.env() with no success
      substitute({
        nb_lines <- values[[paste0("n", Hmisc::upFirst(module))]]
        out <- lapply(seq_len(nb_lines) - 1, function(n){
          isolate({
            title_body <- do.call(paste0("prepare_", module), list(n, edit, input, values))
            show_module(module, edit, n, title_body$title, title_body$body)
          })
        })
        out
      })
    }, quoted = TRUE)
  }

  output$globalParameters <- renderUI({
    tagList(
      fluidRow(
        class="dropdown",
        column(
          12,
          actionLink(
            "newParam", "",
            icon(
              "plus-circle", class="fa-3x rotateIcon"),
            style="margin-bottom:40px") %>% shiny::div(),
          shinyjs::hidden(
            fluidRow(
              id = "tabnewParam",
              tags$ul(
                class = "dropdown-menu",
                style ="position:absolute; top:0; left:60px; display:block; background-color:rgba(255, 255, 255, 0.8)",
                lapply(
                  seq_along(MODULES), function(i){
                    tags$li(
                      actionLink(
                        MODULES[i],
                        names(MODULES)[i],
                        class = "btn btn-link")
                    )
                  })
              )
            )
          )
        )
      ),
      lapply(MODULES, function(module){
        fluidRow(
          column(12,
                 uiOutput(module)
          )
        )})
    )
  })
  
  shinyjs::onevent(
    "mouseenter",
    "newParam",
    shinyjs::show(
      "tabnewParam",
      anim = FALSE,
      animType = "fade"
    ))
  shinyjs::onevent(
    "mouseleave",
    "globalParameters",
    shinyjs::hide(
      "tabnewParam",
      anim = FALSE,
      animType = "fade"
    ))
  
  lapply(MODULES, function(module) {
    observeEvent(input[[module]], {
      edit = TRUE
      for (mod in MODULES){
        removeUI(paste0("#editing", Hmisc::upFirst(mod)))
      }
      if (module ==  "rgho" && (is.null(REGION) | is.null(COUNTRY))){
        try({
          REGION <<- get_gho_codes(dimension = "REGION")
          COUNTRY <<- get_gho_codes(dimension="COUNTRY")
        })
        if (is.null(REGION) | is.null(COUNTRY)){
          showNotification("GHO server is not reacheable for the moment. Please try again later.", duration = 5, type = "warning")
        }
      }
      else {
        n <- values[[paste0("n", Hmisc::upFirst(module))]]
        title_body <- do.call(paste0("prepare_", module), list(n, edit, input, values))
        insertUI("#addModule", ui=
                   show_module(module, edit, n, title_body$title, title_body$body)
        )
      }
    })
  })
  
  observeEvent(input$equationOK, {
    removeUI("#editingEquation")
    insertUI("#addModule", ui = NULL) # workaround to get reactivity back inside #addModule (which was lost probably because of the same ids like equationNameX)
    values$nEquation <- values$nEquation + 1
  })
  
  observeEvent(input$rghoOK, {
    removeUI("#editingRgho")
    insertUI("#addModule", ui = NULL) # workaround to get reactivity back inside #addModule (which was lost probably because of the same ids)
    values$nRgho <- values$nRgho + 1
  })
  
  observeEvent(input$survivalOK, {
    removeUI("#editingSurvival")
    insertUI("#addModule", ui = NULL) # workaround to get reactivity back inside #addModule (which was lost probably because of the same ids)
    values$nSurvival <- values$nSurvival + 1
  })
  
  
  observeEvent(input$timedepOK, {
    removeUI("#editingTimedep")
    insertUI("#addModule", ui = NULL) # workaround to get reactivity back inside #addModule (which was lost probably because of the same ids)
    n <- values$nTimedep
    values$nTimedep <- n + 1
  }) 
})

