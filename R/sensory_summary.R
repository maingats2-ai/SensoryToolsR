#' Summarise sensory attributes by product
#'
#' Calculates descriptive statistics for sensory attributes,
#' including sample size, mean, standard deviation, standard error,
#' coefficient of variation, confidence interval, minimum, maximum,
#' and median.
#'
#' @param data A data frame or tibble containing sensory data.
#' @param product Character. Name of the product/sample column.
#' @param attributes Character vector containing the sensory attribute
#' columns. If NULL, numeric columns other than common design variables
#' are automatically selected.
#' @param conf_level Numeric. Confidence level used for confidence
#' intervals. Default is 0.95.
#'
#' @return A tibble containing descriptive statistics for each product
#' and sensory attribute.
#'
#' @examples
#' \dontrun{
#' summary <- sensory_summary(
#'   data,
#'   product = "product",
#'   attributes = c("sweetness", "bitterness")
#' )
#' }
#'
#' @export
sensory_summary <- function(
    data,
    product = "product",
    attributes = NULL,
    conf_level = 0.95
) {

  # ---------------------------
  # Validate inputs
  # ---------------------------

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame or tibble.",
      call. = FALSE
    )
  }

  if (!product %in% names(data)) {
    stop(
      "Product column not found: ", product,
      call. = FALSE
    )
  }

  if (
    !is.numeric(conf_level) ||
    length(conf_level) != 1 ||
    conf_level <= 0 ||
    conf_level >= 1
  ) {
    stop(
      "`conf_level` must be a single number between 0 and 1.",
      call. = FALSE
    )
  }

  # ---------------------------
  # Detect attributes
  # ---------------------------

  if (is.null(attributes)) {

    numeric_cols <- names(data)[
      vapply(data, is.numeric, logical(1))
    ]

    common_design_cols <- c(
      "session",
      "replicate",
      "rep",
      "assessor_id",
      "product_id",
      "sample_id"
    )

    attributes <- setdiff(
      numeric_cols,
      c(product, common_design_cols)
    )
  }

  if (length(attributes) == 0) {
    stop(
      "No numeric sensory attributes were identified.",
      call. = FALSE
    )
  }

  missing_attributes <- setdiff(
    attributes,
    names(data)
  )

  if (length(missing_attributes) > 0) {
    stop(
      "Unknown attribute column(s): ",
      paste(missing_attributes, collapse = ", "),
      call. = FALSE
    )
  }

  non_numeric <- attributes[
    !vapply(
      data[attributes],
      is.numeric,
      logical(1)
    )
  ]

  if (length(non_numeric) > 0) {
    stop(
      "Sensory attributes must be numeric: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------
  # Convert to long format
  # ---------------------------

  long_data <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(attributes),
    names_to = "attribute",
    values_to = "score"
  )

  # ---------------------------
  # Calculate statistics
  # ---------------------------

  alpha <- 1 - conf_level

  summary_table <- long_data |>
    dplyr::group_by(
      .data[[product]],
      .data$attribute
    ) |>
    dplyr::summarise(

      n = sum(!is.na(.data$score)),

      mean = mean(
        .data$score,
        na.rm = TRUE
      ),

      sd = stats::sd(
        .data$score,
        na.rm = TRUE
      ),

      median = stats::median(
        .data$score,
        na.rm = TRUE
      ),

      min = min(
        .data$score,
        na.rm = TRUE
      ),

      max = max(
        .data$score,
        na.rm = TRUE
      ),

      .groups = "drop"
    ) |>

    dplyr::mutate(

      se = dplyr::if_else(
        .data$n > 1,
        .data$sd / sqrt(.data$n),
        NA_real_
      ),

      cv_percent = dplyr::if_else(
        !is.na(.data$mean) &
          .data$mean != 0,
        (.data$sd / .data$mean) * 100,
        NA_real_
      ),

      t_critical = stats::qt(
        1 - alpha / 2,
        df = dplyr::if_else(
          .data$n > 1,
          as.numeric(.data$n - 1),
          NA_real_
        )
      ),

      ci_lower = .data$mean -
        .data$t_critical * .data$se,

      ci_upper = .data$mean +
        .data$t_critical * .data$se
    ) |>

    dplyr::select(
      dplyr::all_of(product),
      "attribute",
      "n",
      "mean",
      "sd",
      "se",
      "median",
      "cv_percent",
      "ci_lower",
      "ci_upper",
      "min",
      "max"
    )

  cli::cli_alert_success(
    "Sensory summary completed."
  )

  cli::cli_inform(
    "Products: {dplyr::n_distinct(data[[product]])}"
  )

  cli::cli_inform(
    "Attributes: {length(attributes)}"
  )

  summary_table
}
