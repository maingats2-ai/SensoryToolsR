#' Perform trained-panel ANOVA for a sensory attribute
#'
#' Fits an ANOVA model for replicated descriptive sensory data including
#' product, assessor, session, and product-by-assessor interaction effects.
#'
#' @param data A data frame or tibble containing sensory data.
#' @param attribute Character. Name of one numeric sensory attribute.
#' @param product Character. Name of the product/sample column.
#' @param assessor Character. Name of the assessor column.
#' @param session Character. Name of the session or replicate column.
#'
#' @return An object of class `sensory_panel_anova` containing the fitted
#' model, ANOVA table, key p-values, and experimental-design metadata.
#'
#' @details
#' The model fitted is:
#'
#' `attribute ~ product + assessor + session + product:assessor`
#'
#' The terms are interpreted as:
#'
#' * Product: ability of the panel to discriminate among products.
#' * Assessor: differences in scale use among assessors.
#' * Session: systematic differences between replicate sessions.
#' * Product x Assessor: differences among assessors in how they
#'   discriminate the products.
#'
#' This function is intended for replicated descriptive sensory panel data.
#'
#' @examples
#' \dontrun{
#' result <- sensory_panel_anova(
#'   data,
#'   attribute = "sweetness",
#'   product = "product",
#'   assessor = "assessor",
#'   session = "session"
#' )
#' }
#'
#' @export
sensory_panel_anova <- function(
    data,
    attribute,
    product = "product",
    assessor = "assessor",
    session = "session"
) {

  # --------------------------------------------------
  # Input validation
  # --------------------------------------------------

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame or tibble.",
      call. = FALSE
    )
  }

  if (
    missing(attribute) ||
    !is.character(attribute) ||
    length(attribute) != 1
  ) {
    stop(
      "`attribute` must be the name of one sensory attribute.",
      call. = FALSE
    )
  }

  arguments_to_check <- list(
    product = product,
    assessor = assessor,
    session = session
  )

  invalid_arguments <- vapply(
    arguments_to_check,
    function(x) {
      !is.character(x) ||
        length(x) != 1 ||
        is.na(x) ||
        x == ""
    },
    logical(1)
  )

  if (any(invalid_arguments)) {
    stop(
      "`product`, `assessor`, and `session` must each be one column name.",
      call. = FALSE
    )
  }

  required_cols <- c(
    attribute,
    product,
    assessor,
    session
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

  # --------------------------------------------------
  # Remove incomplete observations
  # --------------------------------------------------

  complete_rows <- !is.na(data[[attribute]]) &
    !is.na(data[[product]]) &
    !is.na(data[[assessor]]) &
    !is.na(data[[session]])

  analysis_data <- data[
    complete_rows,
    ,
    drop = FALSE
  ]

  if (nrow(analysis_data) == 0) {
    stop(
      "No complete observations available for panel ANOVA.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate experimental design
  # --------------------------------------------------

  n_products <- length(
    unique(analysis_data[[product]])
  )

  n_assessors <- length(
    unique(analysis_data[[assessor]])
  )

  n_sessions <- length(
    unique(analysis_data[[session]])
  )

  if (n_products < 2) {
    stop(
      "At least two products are required.",
      call. = FALSE
    )
  }

  if (n_assessors < 2) {
    stop(
      "At least two assessors are required.",
      call. = FALSE
    )
  }

  if (n_sessions < 2) {
    stop(
      "At least two sessions or replicates are required for panel ANOVA.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Convert experimental factors
  # --------------------------------------------------

  analysis_data[[product]] <- factor(
    analysis_data[[product]]
  )

  analysis_data[[assessor]] <- factor(
    analysis_data[[assessor]]
  )

  analysis_data[[session]] <- factor(
    analysis_data[[session]]
  )

  # --------------------------------------------------
  # Construct model
  # --------------------------------------------------

  interaction_term <- paste0(
    product,
    ":",
    assessor
  )

  model_formula <- stats::reformulate(
    termlabels = c(
      product,
      assessor,
      session,
      interaction_term
    ),
    response = attribute
  )

  model <- stats::aov(
    formula = model_formula,
    data = analysis_data
  )

  anova_table <- stats::anova(model)

  # --------------------------------------------------
  # Product test using Product x Assessor error term
  # --------------------------------------------------

  product_row <- which(
    rownames(anova_table) == product
  )

  interaction_row <- which(
    rownames(anova_table) == interaction_term
  )

  if (
    length(product_row) != 1 ||
    length(interaction_row) != 1
  ) {
    stop(
      "Unable to identify Product or Product x Assessor terms in the ANOVA table.",
      call. = FALSE
    )
  }

  product_ms <- anova_table[
    product_row,
    "Mean Sq"
  ]

  product_df <- anova_table[
    product_row,
    "Df"
  ]

  interaction_ms <- anova_table[
    interaction_row,
    "Mean Sq"
  ]

  interaction_df <- anova_table[
    interaction_row,
    "Df"
  ]

  product_f <- product_ms /
    interaction_ms

  product_p <- stats::pf(
    product_f,
    df1 = product_df,
    df2 = interaction_df,
    lower.tail = FALSE
  )

  anova_table[
    product_row,
    "F value"
  ] <- product_f

  anova_table[
    product_row,
    "Pr(>F)"
  ] <- product_p

  # --------------------------------------------------
  # Convert ANOVA table to tibble
  # --------------------------------------------------

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

  anova_tbl <- tibble::as_tibble(
    anova_df
  )

  # --------------------------------------------------
  # Helper for extracting p-values
  # --------------------------------------------------

  extract_p <- function(term_name) {

    row <- anova_df[
      anova_df$term == term_name,
      ,
      drop = FALSE
    ]

    if (nrow(row) == 1) {
      row$p_value
    } else {
      NA_real_
    }
  }

  product_p <- extract_p(product)
  assessor_p <- extract_p(assessor)
  session_p <- extract_p(session)
  interaction_p <- extract_p(interaction_term)

  # --------------------------------------------------
  # Build result object
  # --------------------------------------------------

  result <- list(
    attribute = attribute,
    product = product,
    assessor = assessor,
    session = session,
    interaction = interaction_term,
    formula = model_formula,
    model = model,
    anova_table = anova_tbl,
    product_p_value = product_p,
    assessor_p_value = assessor_p,
    session_p_value = session_p,
    interaction_p_value = interaction_p,
    n_observations = nrow(analysis_data),
    n_products = n_products,
    n_assessors = n_assessors,
    n_sessions = n_sessions
  )

  class(result) <- "sensory_panel_anova"

  # --------------------------------------------------
  # Console summary
  # --------------------------------------------------

  cli::cli_alert_success(
    "Sensory panel ANOVA completed."
  )

  cli::cli_inform(
    "Attribute: {attribute}"
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
    "Product p-value: {format.pval(product_p, digits = 4)}"
  )

  cli::cli_inform(
    "Product x Assessor p-value: {format.pval(interaction_p, digits = 4)}"
  )

  result
}
