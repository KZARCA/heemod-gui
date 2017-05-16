#
# interface.R
#
# Copyright (C) 2016-17 Kevin Zarca
#
# This program is licensed to you under the terms of version 3 of the
# GNU Affero General Public License. This program is distributed WITHOUT
# ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
#


shiny_subset <- function(x, elem_names) {
  res <- list()
  for (n in elem_names) {
    res <- c(res, list(x[[n]]))
  }
  names(res) <- elem_names
  res
}

ux_nb_models <- function(input) {
  input$nbStrategies
}

ux_model_names <- function(input) {
  unlist(
    shiny_subset(
      input,
      paste0("strategyName", seq_len(ux_nb_models(input)))
    )
  )
}

ux_nb_parameters <- function(values) {
  list(
    nRgho = values$nRgho,
    nEquation = values$nEquation,
    nSurvival = values$nSurvival,
    nTimedep = values$nTimedep
  )
}

ux_nb_states <- function(input) {
  input$nbStates
}

ux_nb_state_values <- function(input) {
  input$nbStateVariables
}

ux_state_value_names <- function(input) {
  unlist(
    shiny_subset(
      input,
      paste0("variableStateName", seq_len(ux_nb_state_values(input)))
    )
  )
}

ux_state_names <- function(input) {
  unlist(
    shiny_subset(
      input,
      paste0("stateName", seq_len(ux_nb_states(input)))
    )
  )
}

ux_parse_equation <- function(x, input) {
  stats::setNames(
    shiny_subset(input, paste0("equationValue", seq_len(x)-1)),
    unlist(shiny_subset(input, paste0("equationName", seq_len(x)-1)))
  )
}

ux_nb_timedepNC <- function(x, values){
  purrr::map(seq_len(x)-1, function(i){
    if (!is.null(values[[paste0("nTimedepNC", i)]])){
      stats::setNames(values[[paste0("nTimedepNC", i)]], paste0("nTimedepNC", i))
    } 
  })
}

ux_parse_timedep <- function(x, nTimedepNC, input){
  purrr::map(seq_len(x)-1, function(i){
    if(!is.null(nTimedepNC[[i+1]])){
      timedepType <- ifelse(input[[paste0("timedepType", i)]] == "nonConstantModelTime", "model_time", "state_time")
      tmp <- dplyr::data_frame(
        value = shiny_subset(input, paste_("timedepValueNC", i, seq(0, nTimedepNC[[i+1]]))) %>% unlist,
        start = shiny_subset(input, paste_("timedepStart", i, seq(0, nTimedepNC[[i+1]]))) %>% unlist,
        end = shiny_subset(input, paste_("timedepEnd", i, seq(0, nTimedepNC[[i+1]]))) %>% unlist
      ) %>%
        #mutate(exprif = sprintf("%s >= %s & %s <= %s", "model_time", start, "model_time", end))
        mutate(expr = sprintf("ifelse(%s >= %s & %s <= %s, %s, ", timedepType, start, timedepType, end, value))
      l <- nrow(tmp)
      expr <- c(tmp$expr, tmp$value[l])
      sprintf("%s%s", paste0(expr, collapse = ""), paste0(rep(")" , l), collapse = ""))

      #sprintf("dplyr::case_when(%s)", paste0(tmp$exprif, " ~ ", tmp$value, collapse = ", ")) 
    } else {
      shiny_subset(input, paste0("timedepValueC", i)) %>% unlist %>% unname
    }
  }) %>% setNames(
    unlist(shiny_subset(input, paste0("timedepName", seq_len(x)-1)))
  )
}

ux_parse_survival <- function(x, input){
  purrr::map(seq_len(x)-1, function(i){
    lambda <- input[[paste0("survivalLambda", i)]]
    if(input[[paste0("survivalDistribution", i)]] == "Weibull"){
      lambda <- input[[paste0("survivalLambda", i)]]
      k <- input[[paste0("survivalK", i)]]
      sprintf("1 - exp(%s * ((model_time - 1)^%s - model_time^%s))", lambda, k, k)
    } 
  }) %>% setNames(
    unlist(shiny_subset(input, paste0("survivalName", seq_len(x)-1)))
  )
}

ux_parse_rgho <- function(x, input){
  purrr::map(seq_len(x)-1, function(i){
    sprintf('heemod::get_who_mr(age, sex = "%s", region = "%s", country = "%s")', input[[paste0("rghoGender", i)]], input[[paste0("rghoRegion", i)]], input[[paste0("rghoCountry", i)]])
  }) %>% setNames(
    unlist(shiny_subset(input, paste0("rghoName", seq_len(x)-1)))
  )
}

ux_sex <- function(input){
  shQuote(shiny_subset(input, "rghoGender0"),type = "cmd") %>%
  setNames("sex")
}


ux_parameters <- function(input, values, eval) {
  compact <- purrr::compact
  
  info_param <- ux_nb_parameters(values)
  if (info_param$nEquation > 0) {
    list_equation <- ux_parse_equation(info_param$nEquation, input)
  } else {
    list_equation <- NULL
  }
  if (info_param$nRgho > 0) {
    list_rgho <- ux_parse_rgho(info_param$nRgho, input)
  } else {
    list_rgho <- NULL
  }
  if (info_param$nSurvival > 0) {
    list_survival <- ux_parse_survival(info_param$nSurvival, input)
  } else {
    list_survival <- NULL
  }
  if (info_param$nTimedep > 0) {
    nTimedepNC <- ux_nb_timedepNC(info_param$nTimedep, values) 
    list_timedep <- ux_parse_timedep(info_param$nTimedep, nTimedepNC, input)
  } else {
    list_timedep <- NULL
  }
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  
  age <- ux_age(input)
  sex <- ux_sex(input)
  list_param <- c(
    age,
    sex,
    list_equation,
    if(eval)list_rgho,
    list_timedep,
    list_survival
  )
  
  unauthorized <- find_unauthorized(list_param)
  
  if(eval == TRUE & !unauthorized){
    if (length(list_param)) {
      names(list_param) <- trim(names(list_param))
      heemod::define_parameters_(
        lazyeval::as.lazy_dots(list_param)
      )
    } else {
      heemod::define_parameters()
    }
  }
}

ux_use_morta <- function(input) {
  input$use_morta
}


ux_age <- function(input){
  if(!is.na(input$startAge) & !is.na(input$cycleDuration)){
    list(
      input$startAge,
      input$cycleDuration,
      "age_init + model_time * cycle_duration"
    ) %>%
      setNames(c("age_init", "cycle_duration", "age"))
  }
}

ux_morta_country <- function(input) {
  input$countryChoice
}

ux_matrix <- function(input, model_number) {
  res <- try({
    nb_states <- ux_nb_states(input)
    
    mat_values <- shiny_subset(
      input,
      paste_(
        "transmatrix",
        model_number,
        rep(seq_len(nb_states), each = nb_states),
        rep(seq_len(nb_states), nb_states)
      )
    )
    
    unauthorized <- find_unauthorized(mat_values)

    if (!unauthorized){
      heemod:::define_transition_(
        .dots = lazyeval::as.lazy_dots(mat_values),
        state_names = ux_state_names(input)
      )
    } else {
      stop("Forbidden functions found")
    }
  })
  
  if ("try-error" %in% class(res)) {
    NULL
  } else {
    res
  }
}

ux_state <- function(input, model_number, state_number) {
  state_value_names <- ux_state_value_names(input)
  state_values <- sprintf(
    "heemod::discount(%s, %e, %s)",
    unlist(
      shiny_subset(
        input,
        paste_(
          "stateVariable",
          model_number,
          seq_len(ux_nb_state_values(input)),
          state_number
        )
      )
    ),
    unlist(
      shiny_subset(
        input,
        paste_(
          "discountingRate",
          model_number,
          seq_len(ux_nb_state_values(input))
        )
      )
    ),
    ifelse(ux_method(input) == "beginning", TRUE, FALSE)
  )
  names(state_values) <- state_value_names
  unauthorized <- find_unauthorized(state_values, x = model_number, y = state_number)
  if (!unauthorized){
    heemod::define_state_(
      lazyeval::as.lazy_dots(state_values)
    )
  }
}

ux_state_list <- function(input, model_number) {
  nb_states <- ux_nb_states(input)
  list_states <- lapply(
    seq_len(nb_states),
    function(x)
      ux_state(
        input = input,
        model_number = model_number,
        state_number = x
      )
  )
  names(list_states) <- ux_state_names(input)
  if (!any(purrr::map_lgl(list_states, is.null))){
    heemod:::define_state_list_(list_states)
  }
}

ux_model <- function(input, values, model_number) {
  ux_state_list(
    input = input,
    model_number = model_number
  )
  heemod::define_strategy_(
    transition = ux_matrix(
      input = input,
      model_number = model_number
    ),
    states = ux_state_list(
      input = input,
      model_number = model_number
    )
  ) 
}

ux_init <- function(input) {
  init <- unlist(
      shiny_subset(
        input,
        paste0("init", seq_len(ux_nb_states(input)))
      )
    )
  n <- names(init)
  init %>% 
    as.vector %>%
    as.character %>%
    setNames(n)
}

ux_cycles <- function(input) {
  input$cycles
}

ux_method <- function(input) {
  input$countMethod
}

ux_cost <- function(input) {
  req(input$costVariable)
  cost_variable <- setNames(input$costVariable, "cost variable")
  unauthorized <- find_unauthorized(cost_variable)
  if (!unauthorized) lazyeval::as.lazy(cost_variable)
}

ux_effect <- function(input) {
  req(input$effectVariable)
  effect_variable <- setNames(input$effectVariable, "cost variable")
  unauthorized <- find_unauthorized(effect_variable)
  if (!unauthorized) lazyeval::as.lazy(effect_variable)
}

ux_base_model <- function(input) {
  ux_model_names(input)[1]
}

ux_run_models_raw <- function(input, values) {
  list_models <- lapply(
    seq_len(ux_nb_models(input)),
    function(x)
      ux_model(
        input = input,
        values = values,
        model_number = x
      )
  )
  names(list_models) <- ux_model_names(input)

  heemod::run_model_(
    parameters = ux_parameters(
      input = input,
      values = values,
      eval = TRUE
    ),
    uneval_strategy_list = list_models,
    init = ux_init(input),
    cycles = ux_cycles(input),
    method = ux_method(input),
    cost = ux_cost(input),
    effect = ux_effect(input),
    state_time_limit = NULL,
    central_strategy = NULL,
    inflow = rep("0", length(ux_init(input))) %>%
      setNames(nm = paste0("inflow",seq_along(ux_init(input))))
  ) 
}

ux_run_models <- function(input, values) {
  res <-# try({
    ux_run_models_raw(input, values)
  #}, 
  #silent = FALSE)
  
  if ("try-error" %in% class(res)) {
    NULL
  } else {
    res
  }
}

ux_run_dsa <- function(input, values) {
  
  n <- unlist(shiny_subset(
    input,
    paste0("DSAGlobalParamName", seq_len(values$nDeterministic)-1)
  ))
  
  low <- stats::setNames(
    shiny_subset(
      input,
      paste0("minDSAValue", seq_len(values$nDeterministic)-1)
    ),
    n)
  
  high <- stats::setNames(
    shiny_subset(
      input,
      paste0("maxDSAValue", seq_len(values$nDeterministic)-1)
    ),
    n)
  
  heemod::run_dsa(
    model = values$model,
    dsa = heemod::define_dsa_(
      par_names = n,
      low_dots = lazyeval::as.lazy_dots(low),
      high_dots = lazyeval::as.lazy_dots(high)
    ))
}
