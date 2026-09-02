#' Perform simple additive ANOVA for a sensory attribute
#'
#' Fits a simple additive ANOVA model for one sensory attribute using
#' product and assessor effects.
#'
#' @param data A data frame or tibble containing sensory data.
#' @param attribute Character. Name of the sensory attribute column.
#' @param product Character. Name of the product/sample column.
#' @param assessor Character. Name of the assessor column.
#'
#' @return An object of class `sensory_anova` containing the fitted model,
#' ANOVA table, product p-value, and model metadata.
#'
#' @details
#' The fitted model is:
#'
#' `attribute ~ product + assessor`
#'
#' Product and assessor are included as additive effects, and the ordinary
#' residual mean square from this model is used as the error term for the
#' Product F test.
#'
#' This function is intended as a simple analysis for sensory data with
#' one observation per Assessor x Product combination, or when an additive
#' Product + Assessor model is specifically desired.
#'
#' If replicated Assessor x Product observations are detected, the function
#' issues a warning because session effects and Product x Assessor variation
#' are not represented explicitly in this model.
#'
#' For replicated trained-panel or QDA data with session or replicate
#' information, use [sensory_panel_anova()] instead. That function explicitly
#' includes session and Product x Assessor effects and uses the
#' Product x Assessor mean square as the error term for the Product test.
#'
#' Post-hoc comparisons produced by [sensory_posthoc()] use the fitted
#' `sensory_anova` model and therefore inherit the same additive-model
#' assumptions.
#'
#' @examples
#' \dontrun{
#' result <- sensory_anova(
#'   data,
#'   attribute = "sweetness",
#'   product = "product",
#'   assessor = "assessor"
#' )
#' }
#'
#' @export
sensory_anova <- function(
    data,
    attribute,
    product = "product",
    assessor = "assessor"
) {

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame or tibble.",
      call. = FALSE
    )
  }

  if (missing(attribute) ||
      !is.character(attribute) ||
      length(attribute) != 1) {
    stop(
      "`attribute` must be the name of one sensory attribute.",
      call. = FALSE
    )
  }

  required_cols <- c(
    attribute,
    product,
    assessor
  )

  missing_cols <- setdiff(
    required_cols,
    names(data)
  )

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(data[[attribute]])) {
    stop(
      "Sensory attribute must be numeric: ",
      attribute,
      call. = FALSE
    )
  }

  analysis_data <- data[
    !is.na(data[[attribute]]) &
      !is.na(data[[product]]) &
      !is.na(data[[assessor]]),
    ,
    drop = FALSE
  ]

  if (nrow(analysis_data) == 0) {
    stop(
      "No complete observations available for ANOVA.",
      call. = FALSE
    )
  }

  if (length(unique(analysis_data[[product]])) < 2) {
    stop(
      "At least two products are required for ANOVA.",
      call. = FALSE
    )
  }

  if (length(unique(analysis_data[[assessor]])) < 2) {
    stop(
      "At least two assessors are required for this ANOVA model.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Detect replicated assessor-product observations
  # --------------------------------------------------

  assessor_product_counts <- table(
    analysis_data[[assessor]],
    analysis_data[[product]]
  )

  if (any(assessor_product_counts > 1L)) {
    warning(
      paste0(
        "Replicated Assessor x Product observations were detected. ",
        "`sensory_anova()` fits a simple additive Product + Assessor model. ",
        "For replicated trained-panel or QDA data, use ",
        "`sensory_panel_anova()` so session and Product x Assessor ",
        "variation are modelled explicitly."
      ),
      call. = FALSE
    )
  }

  analysis_data[[product]] <- factor(
    analysis_data[[product]]
  )

  analysis_data[[assessor]] <- factor(
    analysis_data[[assessor]]
  )

  model_formula <- stats::reformulate(
    termlabels = c(product, assessor),
    response = attribute
  )

  model <- stats::aov(
    formula = model_formula,
    data = analysis_data
  )

  anova_table <- stats::anova(model)

  anova_df <- data.frame(
    term = rownames(anova_table),
    df = anova_table[["Df"]],
    sum_sq = anova_table[["Sum Sq"]],
    mean_sq = anova_table[["Mean Sq"]],
    f_value = anova_table[["F value"]],
    p_value = anova_table[["Pr(>F)"]],
    row.names = NULL,
    check.names = FALSE
  )

  product_row <- anova_df[
    anova_df$term == product,
    ,
    drop = FALSE
  ]

  product_p <- if (nrow(product_row) == 1) {
    product_row$p_value
  } else {
    NA_real_
  }

  result <- list(
    attribute = attribute,
    product = product,
    assessor = assessor,
    formula = model_formula,
    model = model,
    anova_table = tibble::as_tibble(anova_df),
    product_p_value = product_p,
    n_observations = nrow(analysis_data),
    n_products = length(
      unique(analysis_data[[product]])
    ),
    n_assessors = length(
      unique(analysis_data[[assessor]])
    )
  )

  class(result) <- "sensory_anova"

  cli::cli_alert_success(
    "Sensory ANOVA completed."
  )

  cli::cli_inform(
    "Attribute: {attribute}"
  )

  cli::cli_inform(
    "Products: {result$n_products}"
  )

  cli::cli_inform(
    "Assessors: {result$n_assessors}"
  )

  if (!is.na(product_p)) {

    cli::cli_inform(
      "Product p-value: {format.pval(product_p, digits = 4)}"
    )
  }

  result
}
