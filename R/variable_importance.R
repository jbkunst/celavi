#' Variable importance via variable permutations
#'
#' @param object The model object.
#' @param data A data to calculate the loss_function.
#' @param variables Variables to use.
#' @param response Name of the variable response.
#' @param loss_function The loss function to evaluate,  Must be a function with 2
#'   arguments: actual and predicted values. Loss function gives a smaller
#'   value if the model have better performance of the model.
#' @param iterations Number of iterations.
#' @param sample_size Sample size.
#' @param sample_frac Proportion to sample in each iteration.
#' @param predict_function Predict function, usually is a function(model, newdata)
#'   which returns a vector (no data frame).
#' @param parallel A logical value indicating if the process should be using `furrr::future_pmap_dbl`
#'   or `purrr::pmap_dbl`.
#' @param verbose A logical value indicating to show progress bars.
#'
#' @examples
#'
#' lm_model <- lm(mpg ~ ., data = mtcars)
#'
#' vi <- variable_importance(
#'   lm_model,
#'   response = "mpg",
#'   data = mtcars
#' )
#'
#' plot(vi)
#'
#' @importFrom utils getS3method
#' @importFrom rlang .data
#' @export
variable_importance <- function(object,
                                data = NULL,
                                variables = NULL,
                                response = NULL,
                                loss_function = NULL,
                                iterations = 1,
                                sample_size = NULL,
                                sample_frac = NULL,
                                predict_function = NULL,
                                parallel = FALSE,
                                verbose = TRUE) {


  # data --------------------------------------------------------------------
  if(is.null(data)) {

    cli::cli_alert_info("Trying extract data from object.")

    data <- object[["data"]]

    cli::cli_alert_info(stringr::str_glue("Using `object[[\"data\"]]` as data."))

  }

  assertthat::assert_that(is.data.frame(data))

  # variables ---------------------------------------------------------------
  if (is.null(variables)) {
    variables <- names(data)
    cli::cli_alert_info("Using all variables in data.")
  }

  # response ----------------------------------------------------------------
  # response <- "y"
  if(is.null(response)) {

    response <- get_response(object)

  }
  assertthat::assert_that(is.character(response))
  assertthat::assert_that(length(response) == 1)

  # dont work for factors?
  assertthat::assert_that(length(data[, response, drop = TRUE]) == nrow(data))

  # loss_function ------------------------------------------------------------------
  if (is.null(loss_function)) {
    loss_function <- get_loss_function(data[, response, drop = TRUE])
  }

  assertthat::assert_that(is.function(loss_function))

  # iterations --------------------------------------------------------------
  assertthat::assert_that(is.numeric(iterations))
  assertthat::assert_that(iterations > 0)

  # sampler -----------------------------------------------------------------
  sampler <- get_sampler(data, sample_size, sample_frac)

  # predict function --------------------------------------------------------
  if (is.null(predict_function)) {

    cls <- get_class(object)

    predict_function <- utils::getS3method("predict", class = cls)

    cli::cli_alert_info(stringr::str_glue("Using `predict.{cls}` as predict function."))

  }

  # function ----------------------------------------------------------------
  loss_function_after_permutation <- function(variable, iteration) {

    if(is.na(variable)) variable <- character(0)

    daux <- sampler(data)

    daux[, variable] <- daux[sample(nrow(daux)), variable]

    loss_function(daux[, response, drop = TRUE], predict_function(object, daux))

  }

  # process -----------------------------------------------------------------
  dout <- tidyr::crossing(
    variable = c(variables, NA),
    iteration = seq(iterations)
    )

  if (parallel) {

    future::plan(future::multisession())

    if (verbose) {

      fun_loss_function_p <- function(variable, iteration, p) {
        p()
        loss_function_after_permutation(variable, iteration)
      }

      progressr::with_progress({
        p <- progressr::progressor(steps = nrow(dout))
        results <- furrr::future_pmap_dbl(
          dout,
          fun_loss_function_p,
          p = p,
          .options = furrr::furrr_options(seed = TRUE)
        )
      },
      handlers = progressr::handler_progress(
        format = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
        show_after = 0,
        intrusiveness = getOption("progressr.intrusiveness.terminal", 1),
        target = "terminal"
        )
      )

    } else {
      results <- furrr::future_pmap_dbl(
        dout,
        loss_function_after_permutation,
        .options = furrr::furrr_options(seed = TRUE)
        )

    }


  } else {
    fun_loss_function_np <- loss_function_after_permutation

    if (verbose) {
      pb <- progress::progress_bar$new(
        total = nrow(dout),
        format = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta"
      )

      fun_loss_function_np <- function(variable, iteration) {
        pb$tick(tokens = list(variable = variable, iteration = iteration))
        loss_function_after_permutation(variable, iteration)
      }

    }

    results <- purrr::pmap_dbl(dout, fun_loss_function_np)

  }

  dout <- dplyr::mutate(dout, value = results)

  # fix categories
  dout <- dout |>
    dplyr::mutate(
      variable = dplyr::case_when(
        is.na(variable)      ~ "_full_model_",
        variable == response ~ "_base_line_",
        TRUE                 ~ variable
      )
    )

  # output ------------------------------------------------------------------
  attr(dout, "class_object") <- get_class(object)

  attr(dout, "loss_function") <- attr(loss_function, "text")

  class(dout) <- c("celavi_loss_function_permutations_raw", class(dout))

  dout

}

get_response <- function(object){

  cli::cli_alert_info("Trying extract response name using `formula`.")

  gr <- function(object) as.character(stats::formula(object)[[2L]])
  gr <- purrr::safely(gr)

  response <- gr(object)


  if(is.null(response$result)) {

    cli::cli_alert_danger(as.character(response$error))

    return(NULL)

  } else {

    cli::cli_alert_info(stringr::str_glue("Using `{ response$result }` as response."))

    return(response$result)

  }

}

get_class <- function(object){

  cls <- class(object)

  if(length(cls) == 1) return(cls)

  funs <- purrr::map(
    cls,
    purrr::safely(utils::getS3method),
    f = "predict"
  )

  lgls <- funs |>
    purrr::map(purrr::pluck, "error") |>
    purrr::map_lgl(is.null)

  cls <- cls[lgls]

  cls

}

get_loss_function <- function(response_vector) {

  if(length(unique(response_vector)) == 2) {

    loss_function <- one_minus_auc

    loss_function_txt <- "1 - AUCROC"

  } else if(is.character(response_vector) | is.factor(response_vector)) {

    loss_function <- one_minus_accuracy

    loss_function_txt <- "1 - accuracy"

  } else if(is.numeric(response_vector)){

    loss_function <- rmse

    loss_function_txt <- "root mean square error"

  }

  cli::cli_alert_info(stringr::str_glue("Using { loss_function_txt } as loss function."))

  attr(loss_function, "text") <- loss_function_txt

  loss_function

}

get_sampler <- function(data, sample_size = NULL, sample_frac = NULL){

  if(is.null(sample_size) & is.null(sample_frac)) {

    sampler <- base::identity

    cli::cli_alert_info("Using `base::identity` as sampler.")

  } else if(!is.null(sample_size)){

    assertthat::assert_that(is.numeric(sample_size))
    assertthat::assert_that(sample_size == as.integer(sample_size))
    assertthat::assert_that(sample_size > 1)

    replace_arg <- sample_size > nrow(data)

    sampler <- purrr::partial(
      dplyr::sample_n,
      size = sample_size,
      replace = replace_arg
    )

    cli::cli_alert_info("Using `dplyr::sample_n` as sampler.")

  } else if(!is.null(sample_frac)){

    assertthat::assert_that(is.numeric(sample_frac))
    assertthat::assert_that(sample_frac > 0)
    assertthat::assert_that(sample_frac <= 1)

    sampler <- purrr::partial(dplyr::sample_frac, size = sample_frac)

    cli::cli_alert_info("Using `dplyr::sample_frac` as sampler.")

  } else {

    cli::cli_abort("Some problem defining the sampler.")

  }

  sampler

}
