#' Multi-attribute sensory panel analysis
#'
#' Runs sensory panel ANOVA and assessor performance analysis across
#' multiple sensory attributes.
#'
#' @param data A data frame or tibble containing replicated sensory data.
#' @param attributes Character vector containing names of numeric sensory
#' attributes to analyse.
#' @param product Character. Name of the product column.
#' @param assessor Character. Name of the assessor column.
#' @param session Character. Name of the session or replicate column.
#' @param alpha Numeric. Significance level. Default is 0.05.
#' @param agreement_threshold Numeric. Agreement-correlation screening
#' threshold. Default is 0.70.
#' @param repeatability_multiplier Numeric. Multiplier used for
#' repeatability screening. Default is 1.5.
#'
#' @return An object of class `sensory_panel_multi` containing:
#' \itemize{
#'   \item `attribute_summary`: one-row summary for each sensory attribute.
#'   \item `anova_tables`: named list of ANOVA tables.
#'   \item `assessor_tables`: named list of assessor-performance tables.
#'   \item `anova_results`: complete `sensory_panel_anova` objects.
#'   \item `performance_results`: complete
#'   `sensory_panel_performance` objects.
#'   \item `attributes`: attributes analysed.
#' }
#'
#' @details
#' This function provides a convenient wrapper around
#' `sensory_panel_anova()` and `sensory_panel_performance()`.
#'
#' Review counts are screening indicators and should not be interpreted
#' as automatic assessor exclusion criteria.
#'
#' @examples
#' \dontrun{
#' result <- sensory_panel_multi(
#'   data,
#'   attributes = c(
#'     "sweetness",
#'     "bitterness"
#'   )
#' )
#'
#' result$attribute_summary
#' }
#'
#' @export
sensory_panel_multi <- function(
    data,
    attributes,
    product = "product",
    assessor = "assessor",
    session = "session",
    alpha = 0.05,
    agreement_threshold = 0.70,
    repeatability_multiplier = 1.5
) {

  # --------------------------------------------------
  # Validate inputs
  # --------------------------------------------------

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame or tibble.",
      call. = FALSE
    )
  }

  if (
    missing(attributes) ||
    !is.character(attributes) ||
    length(attributes) < 1
  ) {
    stop(
      "`attributes` must contain at least one sensory attribute name.",
      call. = FALSE
    )
  }

  if (anyDuplicated(attributes)) {
    stop(
      "`attributes` must not contain duplicate names.",
      call. = FALSE
    )
  }

  required_design_columns <- c(
    product,
    assessor,
    session
  )

  missing_design_columns <- setdiff(
    required_design_columns,
    names(data)
  )

  if (length(missing_design_columns) > 0) {
    stop(
      "Missing required design column(s): ",
      paste(
        missing_design_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  missing_attributes <- setdiff(
    attributes,
    names(data)
  )

  if (length(missing_attributes) > 0) {
    stop(
      "Missing sensory attribute column(s): ",
      paste(
        missing_attributes,
        collapse = ", "
      ),
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
      "Sensory attribute(s) must be numeric: ",
      paste(
        non_numeric,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(alpha) ||
    length(alpha) != 1 ||
    is.na(alpha) ||
    alpha <= 0 ||
    alpha >= 1
  ) {
    stop(
      "`alpha` must be a single number between 0 and 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(agreement_threshold) ||
    length(agreement_threshold) != 1 ||
    is.na(agreement_threshold) ||
    !is.finite(agreement_threshold) ||
    agreement_threshold < -1 ||
    agreement_threshold > 1
  ) {
    stop(
      "`agreement_threshold` must be between -1 and 1.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Containers
  # --------------------------------------------------

  anova_results <- vector(
    "list",
    length(attributes)
  )

  names(anova_results) <- attributes

  performance_results <- vector(
    "list",
    length(attributes)
  )

  names(performance_results) <- attributes

  attribute_summary_list <- vector(
    "list",
    length(attributes)
  )

  # --------------------------------------------------
  # Analyse each sensory attribute
  # --------------------------------------------------

  for (i in seq_along(attributes)) {

    current_attribute <- attributes[i]

    anova_result <- sensory_panel_anova(
      data = data,
      attribute = current_attribute,
      product = product,
      assessor = assessor,
      session = session
    )

    performance_result <-
      sensory_panel_performance(
        data = data,
        attribute = current_attribute,
        product = product,
        assessor = assessor,
        session = session,
        alpha = alpha,
        agreement_threshold =
          agreement_threshold,
        repeatability_multiplier =
          repeatability_multiplier
      )

    anova_results[[current_attribute]] <-
      anova_result

    performance_results[[current_attribute]] <-
      performance_result

    attribute_summary_list[[i]] <-
      tibble::tibble(
        attribute = current_attribute,

        product_p_value =
          anova_result$product_p_value,

        assessor_p_value =
          anova_result$assessor_p_value,

        session_p_value =
          anova_result$session_p_value,

        interaction_p_value =
          anova_result$interaction_p_value,

        product_significant =
          !is.na(
            anova_result$product_p_value
          ) &&
          anova_result$product_p_value <
          alpha,

        interaction_significant =
          !is.na(
            anova_result$interaction_p_value
          ) &&
          anova_result$interaction_p_value <
          alpha,

        median_repeatability_rmse =
          performance_result$
          panel_summary$
          median_repeatability_rmse,

        median_agreement_correlation =
          performance_result$
          panel_summary$
          median_agreement_correlation,

        n_assessors_review =
          performance_result$
          panel_summary$
          n_assessors_review
      )
  }

  # --------------------------------------------------
  # Combine summaries
  # --------------------------------------------------

  attribute_summary <- do.call(
    rbind,
    attribute_summary_list
  )

  attribute_summary <-
    tibble::as_tibble(
      attribute_summary
    )

  anova_tables <- lapply(
    anova_results,
    function(x) {
      x$anova_table
    }
  )

  assessor_tables <- lapply(
    performance_results,
    function(x) {
      x$assessor_table
    }
  )

  # --------------------------------------------------
  # Return object
  # --------------------------------------------------

  result <- list(
    attributes = attributes,
    attribute_summary =
      attribute_summary,
    anova_tables =
      anova_tables,
    assessor_tables =
      assessor_tables,
    anova_results =
      anova_results,
    performance_results =
      performance_results,
    settings = list(
      alpha = alpha,
      agreement_threshold =
        agreement_threshold,
      repeatability_multiplier =
        repeatability_multiplier
    )
  )

  class(result) <- "sensory_panel_multi"

  cli::cli_alert_success(
    "Multi-attribute sensory panel analysis completed."
  )

  cli::cli_inform(
    "Attributes analysed: {length(attributes)}"
  )

  cli::cli_inform(
    "Attributes with significant product effect: {sum(attribute_summary$product_significant)}"
  )

  cli::cli_inform(
    "Attributes with significant Product x Assessor interaction: {sum(attribute_summary$interaction_significant)}"
  )

  result
}
