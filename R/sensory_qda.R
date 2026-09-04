#' Integrated QDA sensory analysis workflow
#'
#' Runs an integrated Quantitative Descriptive Analysis (QDA) workflow
#' across multiple sensory attributes.
#'
#' The workflow combines data validation, descriptive statistics,
#' multi-attribute panel ANOVA, assessor performance analysis, PCA,
#' and PCA diagnostics.
#'
#' @param data A data frame or tibble containing replicated sensory data.
#' @param attributes Character vector containing names of numeric sensory
#' attributes to analyse.
#' @param product Character. Name of the product/sample column.
#' Default is `"product"`.
#' @param assessor Character. Name of the assessor column.
#' Default is `"assessor"`.
#' @param session Character. Name of the session or replicate column.
#' Default is `"session"`.
#' @param alpha Numeric. Significance level used for ANOVA and panel
#' screening. Default is 0.05.
#' @param agreement_threshold Numeric. Agreement correlation threshold
#' used in assessor-performance screening. Default is 0.70.
#' @param repeatability_multiplier Numeric. Multiplier used for
#' repeatability-error screening. Default is 1.5.
#' @param pca_center Logical. Should sensory attributes be centered before
#' PCA? Default is TRUE.
#' @param pca_scale Logical. Should sensory attributes be standardized
#' before PCA? Default is FALSE.
#' @param pca_components Integer vector identifying PCA components for
#' diagnostics. Default is `c(1, 2)`.
#' @param pca_top_n Integer. Number of top PCA contributors retained for
#' each selected component. Default is 5.
#'
#' @return An object of class `sensory_qda` containing:
#' \itemize{
#'   \item `validation`: output from `sensory_validate()`.
#'   \item `summary`: descriptive sensory statistics.
#'   \item `panel`: output from `sensory_panel_multi()`.
#'   \item `pca`: output from `sensory_pca()`.
#'   \item `pca_diagnostics`: output from `sensory_pca_diagnostics()`.
#'   \item `attributes`: sensory attributes analysed.
#'   \item `design`: names of product, assessor, and session variables.
#'   \item `settings`: analysis settings used.
#' }
#'
#' @details
#' `sensory_qda()` is an orchestration function. It does not introduce
#' a new statistical model. Instead, it combines previously defined
#' SensoryToolsR functions into one reproducible QDA workflow.
#'
#' The function does not automatically exclude assessors, choose PCA
#' scaling, or alter the significance level.
#'
#' PCA is performed on product mean sensory profiles.
#'
#' @examples
#' \dontrun{
#' result <- sensory_qda(
#'   data,
#'   attributes = c(
#'     "sweetness",
#'     "bitterness",
#'     "firmness",
#'     "juiciness"
#'   )
#' )
#'
#' result$summary
#' result$panel$attribute_summary
#' result$pca$variance_table
#' result$pca_diagnostics$top_attributes
#' }
#'
#' @export
sensory_qda <- function(
    data,
    attributes,
    product = "product",
    assessor = "assessor",
    session = "session",
    alpha = 0.05,
    agreement_threshold = 0.70,
    repeatability_multiplier = 1.5,
    pca_center = TRUE,
    pca_scale = FALSE,
    pca_components = c(1, 2),
    pca_top_n = 5
) {

  # --------------------------------------------------
  # Basic input validation
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
    length(attributes) < 2
  ) {
    stop(
      "`attributes` must contain at least two sensory attribute names.",
      call. = FALSE
    )
  }

  if (anyDuplicated(attributes)) {
    stop(
      "`attributes` must not contain duplicate names.",
      call. = FALSE
    )
  }

  design_columns <- c(
    product,
    assessor,
    session
  )

  invalid_design_names <- vapply(
    design_columns,
    function(x) {
      !is.character(x) ||
        length(x) != 1 ||
        is.na(x) ||
        x == ""
    },
    logical(1)
  )

  if (any(invalid_design_names)) {
    stop(
      "`product`, `assessor`, and `session` must each be one column name.",
      call. = FALSE
    )
  }

  missing_design_columns <- setdiff(
    design_columns,
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

  non_numeric_attributes <- attributes[
    !vapply(
      data[attributes],
      is.numeric,
      logical(1)
    )
  ]

  if (length(non_numeric_attributes) > 0) {
    stop(
      "Sensory attribute(s) must be numeric: ",
      paste(
        non_numeric_attributes,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate analysis settings
  # --------------------------------------------------

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
    agreement_threshold < -1 ||
    agreement_threshold > 1
  ) {
    stop(
      "`agreement_threshold` must be between -1 and 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(repeatability_multiplier) ||
    length(repeatability_multiplier) != 1 ||
    is.na(repeatability_multiplier) ||
    !is.finite(repeatability_multiplier) ||
    repeatability_multiplier <= 0
  ) {
    stop(
      "`repeatability_multiplier` must be a finite number greater than 0.",
      call. = FALSE
    )
  }

  if (
    !is.logical(pca_center) ||
    length(pca_center) != 1 ||
    is.na(pca_center)
  ) {
    stop(
      "`pca_center` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(pca_scale) ||
    length(pca_scale) != 1 ||
    is.na(pca_scale)
  ) {
    stop(
      "`pca_scale` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(pca_components) ||
    length(pca_components) < 1 ||
    any(is.na(pca_components)) ||
    any(pca_components %% 1 != 0) ||
    any(pca_components < 1)
  ) {
    stop(
      "`pca_components` must contain positive integer component numbers.",
      call. = FALSE
    )
  }

  if (anyDuplicated(pca_components)) {
    stop(
      "`pca_components` must not contain duplicate values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(pca_top_n) ||
    length(pca_top_n) != 1 ||
    is.na(pca_top_n) ||
    !is.finite(pca_top_n) ||
    pca_top_n %% 1 != 0 ||
    pca_top_n < 1
  ) {
    stop(
      "`pca_top_n` must be a positive integer.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Experimental-design check
  # --------------------------------------------------

  n_products <- length(
    unique(
      data[[product]][
        !is.na(data[[product]])
      ]
    )
  )

  n_assessors <- length(
    unique(
      data[[assessor]][
        !is.na(data[[assessor]])
      ]
    )
  )

  n_sessions <- length(
    unique(
      data[[session]][
        !is.na(data[[session]])
      ]
    )
  )

  if (n_products < 3) {
    stop(
      "Integrated QDA with PCA requires at least three products.",
      call. = FALSE
    )
  }

  if (n_assessors < 2) {
    stop(
      "Integrated QDA requires at least two assessors.",
      call. = FALSE
    )
  }

  if (n_sessions < 2) {
    stop(
      "Integrated QDA requires at least two sessions or replicates.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # 1. Data validation
  # --------------------------------------------------

  validation <- sensory_validate(
    data,
    assessor = assessor,
    product = product,
    session = session,
    attributes = attributes
  )

  # --------------------------------------------------
  # 2. Descriptive sensory statistics
  # --------------------------------------------------

  summary_result <- sensory_summary(
    data,
    product = product,
    attributes = attributes
  )

  # --------------------------------------------------
  # 3. Multi-attribute panel analysis
  # --------------------------------------------------

  panel_result <- sensory_panel_multi(
    data = data,
    attributes = attributes,
    product = product,
    assessor = assessor,
    session = session,
    alpha = alpha,
    agreement_threshold =
      agreement_threshold,
    repeatability_multiplier =
      repeatability_multiplier
  )

  # --------------------------------------------------
  # 4. PCA
  # --------------------------------------------------

  pca_result <- sensory_pca(
    data = data,
    attributes = attributes,
    product = product,
    center = pca_center,
    scale = pca_scale
  )

  # --------------------------------------------------
  # Validate requested PCA components now that the
  # number of available components is known
  # --------------------------------------------------

  available_components <- length(
    pca_result$variance_explained
  )

  if (
    any(
      pca_components >
      available_components
    )
  ) {
    stop(
      "`pca_components` contains unavailable principal component numbers.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # 5. PCA diagnostics
  # --------------------------------------------------

  pca_diagnostics <- sensory_pca_diagnostics(
    pca_result,
    components = pca_components,
    top_n = pca_top_n
  )

  # --------------------------------------------------
  # Integrated overview
  # --------------------------------------------------

  overview <- tibble::tibble(
    n_observations = nrow(data),
    n_products = n_products,
    n_assessors = n_assessors,
    n_sessions = n_sessions,
    n_attributes = length(attributes),
    n_significant_product_attributes =
      sum(
        panel_result$
          attribute_summary$
          product_significant,
        na.rm = TRUE
      ),
    n_significant_interactions =
      sum(
        panel_result$
          attribute_summary$
          interaction_significant,
        na.rm = TRUE
      ),
    pca_pc1_percent =
      pca_result$
      variance_explained[1],
    pca_pc2_percent =
      if (
        length(
          pca_result$
          variance_explained
        ) >= 2
      ) {
        pca_result$
          variance_explained[2]
      } else {
        NA_real_
      },
    pca_pc1_pc2_percent =
      if (
        length(
          pca_result$
          variance_explained
        ) >= 2
      ) {
        sum(
          pca_result$
            variance_explained[1:2]
        )
      } else {
        pca_result$
          variance_explained[1]
      }
  )

  # --------------------------------------------------
  # Build result object
  # --------------------------------------------------

  result <- list(
    overview = overview,
    validation = validation,
    summary = summary_result,
    panel = panel_result,
    pca = pca_result,
    pca_diagnostics =
      pca_diagnostics,
    attributes = attributes,
    design = list(
      product = product,
      assessor = assessor,
      session = session
    ),
    settings = list(
      alpha = alpha,
      agreement_threshold =
        agreement_threshold,
      repeatability_multiplier =
        repeatability_multiplier,
      pca_center = pca_center,
      pca_scale = pca_scale,
      pca_components =
        pca_components,
      pca_top_n = pca_top_n
    )
  )

  class(result) <- "sensory_qda"

  # --------------------------------------------------
  # Console report
  # --------------------------------------------------

  cli::cli_alert_success(
    "Integrated sensory QDA analysis completed."
  )

  cli::cli_inform(
    "Products: {n_products}"
  )

  cli::cli_inform(
    "Assessors: {n_assessors}"
  )

  cli::cli_inform(
    "Sessions: {n_sessions}"
  )

  cli::cli_inform(
    "Attributes: {length(attributes)}"
  )

  cli::cli_inform(
    "Attributes with significant product effect: {overview$n_significant_product_attributes}"
  )

  cli::cli_inform(
    "PC1 + PC2 variance: {format(round(overview$pca_pc1_pc2_percent, 2), nsmall = 2)}%"
  )

  result
}
