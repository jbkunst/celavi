# library(tidyverse)
#


feature_selection <- function(fit_function = NULL,
                              data = NULL,
                              test = data,
                              # variables = NULL,
                              response = NULL,
                              metric = NULL,
                              stat = median,
                              iterations = 1,
                              sample_size = NULL,
                              sample_frac = NULL,
                              predict_function = NULL,
                              parallel = FALSE,
                              ...
                              ){

  # fit_function ------------------------------------------------------------
  assertthat::assert_that(is.function(fit_function))
  assertthat::assert_that(assertthat::has_args(fit_function, "formula"))
  assertthat::assert_that(assertthat::has_args(fit_function, "data"))

  # data --------------------------------------------------------------------
  assertthat::assert_that(is.data.frame(data))
  if(is.null(test)){
    cli::cli_alert_info("Using data as test data.")
    test <- data
  }

  # response ----------------------------------------------------------------
  assertthat::assert_that(is.character(response))
  assertthat::assert_that(length(response) == 1)
  assertthat::assert_that(response %in% names(data))

  # metric ------------------------------------------------------------------
  if (is.null(metric)) metric <- get_metric(data[, response, drop = TRUE])
  # sampler <- get_sampler(data, sample_size, sample_frac)

  # stat --------------------------------------------------------------------
  assertthat::assert_that(is.function(stat))


  # fit start model ---------------------------------------------------------
  data_iter   <- data

  cli::cli_alert_info("Fitting 1st model using { ncol(data_iter) - 1} predictor variables.")

  object_iter <- fit_function(
    as.formula(stringr::str_glue("{ response } ~ .")),
    data_iter,
    ...
  )

  round     <- 1
  vars      <- list(keep = setdiff(names(data), response))
  vis       <- list()
  decisions <- list(vars)

  # predict_function(object, head(data)) == head(data[, response, drop = TRUE])

  # process -----------------------------------------------------------------
  while(TRUE) {

    cli::cli_h2(stringr::str_glue("Round #{ round }"))

    vi <- variable_importance(
      object_iter,
      data = test,
      variables = vars$keep,
      response = response,
      metric = metric,
      iterations = iterations,
      sample_size = sample_size,
      sample_frac = sample_frac,
      predict_function = predict_function,
      parallel = parallel
      )

    plot(vi)

    vars <- variable_decision(vi, stat = stat)

    vis       <- append(vis, list(vi))

    if(length(vars$rm) > 0){

      cli::cli_alert_info(
        stringr::str_glue(
          "Removing { length(vars$rm) } variables. Fitting new model with { length(vars$keep) } variables."
          )
      )

      data_iter <- data[, c(response, vars$keep)]

      object_iter <- fit_function(
        as.formula(stringr::str_glue("{ response } ~ .")),
        data_iter,
        ...
      )


    } else {

      break

    }

    decisions <- append(decisions, list(vars))

    round <- round + 1

  }

  # output ------------------------------------------------------------------
  # object
  #
  # do.call(plot, vis)
  #
  # library(patchwork)
  #
  # vis |>
  #   purrr::map(plot) |>
  #   purrr::reduce(`+`)

  dout <- vis |>
    purrr::map_df(identity, .id = "round") |>
    dplyr::mutate(round = as.numeric(.data$round)) |>
    dplyr::filter(variable == "_full_model_") |>
    dplyr::group_by(.data$round) |>
    dplyr::summarise(value = mean(.data$value))

  vars_keep <- decisions |>
    purrr::map(purrr::pluck, "keep")

  dout <- dout |>
    dplyr::mutate(
      n_variables = purrr::map_int(vars_keep, length),
      variables = vars_keep
    )

  attr(dout, "variable_importance") <- vis

  dout

}


variable_decision <- function(vi, stat = mean){

  # ggplot(vi) +
  #   geom_boxplot(aes(variable, value)) +
  #   theme(axis.text.x = element_text(angle = 90))
  #
  # plot(vi)

  # agreggation variables using stat
  dimp_var_agg <- vi |>
    dplyr::filter(stringr::str_detect(.data$variable, "_full_model_|_base_line_", negate = TRUE)) |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(value = stat(.data$value), .groups = "drop") |>
    dplyr::arrange(desc(value))

  # here, stats = mean
  full_model_metric_value <- vi |>
    dplyr::filter(stringr::str_detect(.data$variable, "_full_model_")) |>
    dplyr::summarise(value = mean(.data$value)) |>
    dplyr::pull(.data$value)

  # 1 - full_model_metric_value

  var_rm   <- dimp_var_agg |>
    dplyr::filter(.data$value <  full_model_metric_value) |>
    dplyr::pull(.data$variable)

  var_keep <- dimp_var_agg |>
    dplyr::filter(.data$value >= full_model_metric_value) |>
    dplyr::pull(.data$variable)

  list(rm = var_rm, keep = var_keep)

}
