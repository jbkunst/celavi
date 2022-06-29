#' @export
plot.celavi_metric_permutations_raw <- function(x, ...){

  extra_xs <- list(...)

  objects <- append(list(x), extra_xs)

  # give id if some models have same class (to group correctly)
  objects <- purrr::map2(objects, seq_along(objects), ~ dplyr::mutate(.x, id = .y))

  # adding the class
  dimp <- purrr::map_df(objects, ~ dplyr::mutate(.x, class = attr(.x, "class_object")[1]))

  # separating variables from references values
  # this will be used for boxplot
  dimp_var <- dimp |>
    dplyr::filter(stringr::str_detect(.data$variable, "_full_model_|_base_line_", negate = TRUE))

  # obtaining mean of drop out loss for bar/colum/rect geom
  dimp_var_aggregated <- dimp_var |>
    dplyr::group_by(.data$id, .data$class, .data$variable) |>
    dplyr::summarise(value = mean(.data$value), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$value)) |>
    dplyr::mutate(variable = forcats::fct_inorder(.data$variable)) |>
    dplyr::mutate(variable = forcats::fct_rev(.data$variable))

  lvls <- levels(dplyr::pull(dimp_var_aggregated, .data$variable))

  # adding levels to dimp_var from dimp_var_aggregated
  dimp_var <- dimp_var |>
    dplyr::mutate(variable = factor(.data$variable, levels = lvls))

  # values for reference, (1) separate (2) anda aggregate
  dimp_reference <- dimp |>
    dplyr::filter(stringr::str_detect(.data$variable, "_full_model_|_base_line_")) |>
    dplyr::group_by(.data$id, .data$class, .data$variable) |>
    dplyr::summarise(value = mean(.data$value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "variable", values_from = "value")

  dimp_var_aggregated <- dimp_var_aggregated |>
    dplyr::left_join(dimp_reference, by = c("id", "class"))

  gg <- ggplot2::ggplot() +

    # ggplot2::facet_grid(ggplot2::vars(.data$id, .data$class)) +
    ggplot2::facet_wrap(ggplot2::vars(stringr::str_c(.data$id, .data$class, sep = ": ")), ncol = 1) +

    ggplot2::geom_boxplot(
      data = dimp_var,
      ggplot2::aes(.data$variable, .data$value),
      color = "transparent",
      fill = "transparent",
    )  +

    ggplot2::geom_hline(
      data = dimp_reference,
      ggplot2::aes(yintercept = .data$`_full_model_`),
      color = "gray60"
    ) +

    ggplot2::geom_rect(
      data = dimp_var_aggregated,
      ggplot2::aes(
        xmin = as.numeric(.data$variable) - 1/3,
        xmax = as.numeric(.data$variable) + 1/3,
        ymin = .data$`_full_model_`,
        ymax = .data$value),
      fill = "gray90"
    ) +

    ggplot2::geom_boxplot(
      data = dimp_var,
      ggplot2::aes(.data$variable, .data$value),
      width = 0.25,
      color = "gray60",
      fill = "gray70",
    )  +

    ggplot2::scale_y_continuous(name = attr(x, "metric")) +

    ggplot2::coord_flip() +

    ggplot2::theme_minimal()

  # remove facet if there are no extra VIs
  if(length(extra_xs) == 0){

    gg <- gg +
      ggplot2::facet_null()

  }

  # ROC?
  if(FALSE){
    gg <- gg +
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ 1 - .x))
  }

  gg

}

#' @export
plot.celavi_feature_selection <- function(x, loss_function_transform = ~ .x, ...){

  # x <- dout

  x2 <- x |>
    dplyr::select(
      .data$round,
      loss_function = .data$mean_value,
      variables = .data$n_variables) |>
    tidyr::pivot_longer(cols = c(2:3))

  x3 <- x |>
    dplyr::select(.data$round, value = .data$values) |>
    tidyr::unnest(cols = 2) |>
    dplyr::mutate(name = "loss_function")

  ggplot2::ggplot(mapping = ggplot2::aes(.data$round, .data$value)) +

    ggplot2::geom_line(data = dplyr::filter(x2, .data$name == "loss_function")) +

    ggplot2::geom_boxplot(data = x3) +

    ggplot2::geom_col(data = dplyr::filter(x2, .data$name == "variables")) +

    ggplot2::ylim(0, NA) +

    ggplot2::facet_wrap(ggplot2::vars(.data$name), scales = "free")


}
