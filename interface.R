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

ux_parameters <- function(input, values) {
  
  info_param <- ux_nb_parameters(values)
  
  if (info_param$nEquation > 0) {
    list_equation <- ux_parse_equation(info_param$nEquation, input)
  } else {
    list_equation <- NULL
  }
  
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  
  list_param <- c(
    list_equation
  )
  
  if (length(list_param)) {
    names(list_param) <- trim(names(list_param))
    
    heemod::define_parameters_(
      lazyeval::as.lazy_dots(list_param)
    )
  } else {
    heemod::define_parameters()
  }
}

ux_use_morta <- function(input) {
  input$use_morta
}

ux_morta_age <- function(input) {
  input$startAge
}

ux_morta_sex <- function(input) {
  input$gender
}

ux_morta_country <- function(input) {
  input$countryChoice
}

ux_matrix <- function(input, model_number) {
  res <- try({
    nb_states <- ux_nb_states(input)
    
    mat_values <- shiny_subset(
      input,
      paste0(
        "transmatrix",
        model_number,
        rep(seq_len(nb_states), each = nb_states),
        rep(seq_len(nb_states), nb_states)
      )
    )
    
    heemod::define_transition_(
      .dots = lazyeval::as.lazy_dots(mat_values),
      state_names = ux_state_names(input)
    )
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
    "heemod::discount(%s, %e)",
    unlist(
      shiny_subset(
        input,
        paste0(
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
        paste0(
          "discountingRate",
          model_number,
          seq_len(ux_nb_state_values(input))
        )
      )
    ) / 100
  )
  
  names(state_values) <- state_value_names
  
  heemod::define_state_(
    lazyeval::as.lazy_dots(state_values)
  )
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
  heemod:::define_state_list_(list_states)
}

ux_model <- function(input, values, model_number) {
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
  as.vector(
    unlist(
      shiny_subset(
        input,
        paste0("init", seq_len(ux_nb_states(input)))
      )
    )
  )
}

ux_cycles <- function(input) {
  input$cycles
}

ux_method <- function(input) {
  input$countMethod
}

ux_cost <- function(input) {
  lazyeval::as.lazy(input$costVariable)
}

ux_effect <- function(input) {
  lazyeval::as.lazy(input$effectVariable)
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
      values = values
    ),
    uneval_strategy_list = list_models,
    init = ux_init(input),
    cycles = ux_cycles(input),
    method = ux_method(input),
    cost = ux_cost(input),
    effect = ux_effect(input),
    state_cycle_limit = NULL,
    central_strategy = NULL,
    inflow = rep(0, length(ux_init(input)))
  )
}

ux_run_models <- function(input, values) {
  res <- try({
    ux_run_models_raw(input, values)
  }, 
  silent = TRUE)
  
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
