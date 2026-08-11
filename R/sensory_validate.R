#' Validate the structure of a sensory dataset
#'
#' Checks a sensory dataset for common structural issues before analysis,
#' including missing values, duplicate observations, experimental structure,
#' candidate sensory attributes, and completeness of the
#' assessor-product-session design.
#'
#' @param data A data frame or tibble containing sensory data.
#' @param assessor Character. Name of the assessor column.
#' @param product Character. Name of the product/sample column.
#' @param session Character or NULL. Name of the session column.
#' @param attributes Character vector of sensory attribute columns. If NULL,
#' numeric columns not used as design variables are treated as candidate
#' sensory attributes.
#'
#' @return An object of class `sensory_validation` containing validation
#' results and a printed diagnostic summary.
#'
#' @examples
#' \dontrun{
#' result <- sensory_validate(
#'   data,
#'   assessor = "assessor",
#'   product = "product",
#'   session = "session"
#' )
#' }
#'
#' @export
sensory_validate <- function(
    data,
    assessor = "assessor",
    product = "product",
    session = "session",
    attributes = NULL
) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  required_cols <- c(assessor, product)

  if (!is.null(session)) {
    required_cols <- c(required_cols, session)
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  design_cols <- c(assessor, product)

  if (!is.null(session)) {
    design_cols <- c(design_cols, session)
  }

  if (is.null(attributes)) {

    numeric_cols <- names(data)[
      vapply(data, is.numeric, logical(1))
    ]

    attributes <- setdiff(
      numeric_cols,
      design_cols
    )
  }

  missing_attributes <- setdiff(attributes, names(data))

  if (length(missing_attributes) > 0) {
    stop(
      "Unknown attribute column(s): ",
      paste(missing_attributes, collapse = ", "),
      call. = FALSE
    )
  }

  n_assessors <- length(unique(data[[assessor]]))
  n_products <- length(unique(data[[product]]))

  if (!is.null(session)) {
    n_sessions <- length(unique(data[[session]]))
  } else {
    n_sessions <- 1L
  }

  missing_by_column <- vapply(
    data,
    function(x) sum(is.na(x)),
    numeric(1)
  )

  total_missing <- sum(missing_by_column)

  duplicate_keys <- data[
    duplicated(data[design_cols]) |
      duplicated(data[design_cols], fromLast = TRUE),
    design_cols,
    drop = FALSE
  ]

  n_duplicate_rows <- nrow(duplicate_keys)

  # Build the expected experimental design
  design_levels <- lapply(
    design_cols,
    function(x) sort(unique(data[[x]]))
  )

  names(design_levels) <- design_cols

  expected_design <- expand.grid(
    design_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  observed_design <- unique(
    data[design_cols]
  )

  # Identify missing assessor-product-session combinations
  design_key <- function(df) {
    do.call(
      paste,
      c(df, sep = "|||")
    )
  }

  expected_key <- design_key(expected_design)
  observed_key <- design_key(observed_design)

  missing_design <- expected_design[
    !(expected_key %in% observed_key),
    ,
    drop = FALSE
  ]

  n_expected_design <- nrow(expected_design)
  n_observed_design <- nrow(observed_design)
  n_missing_design <- nrow(missing_design)

  panel_complete <- n_missing_design == 0

  result <- list(
    n_rows = nrow(data),
    n_columns = ncol(data),
    n_assessors = n_assessors,
    n_products = n_products,
    n_sessions = n_sessions,
    attributes = attributes,
    missing_by_column = missing_by_column,
    total_missing = total_missing,
    duplicate_keys = duplicate_keys,
    n_duplicate_rows = n_duplicate_rows,
    design_columns = design_cols,
    n_expected_design = n_expected_design,
    n_observed_design = n_observed_design,
    n_missing_design = n_missing_design,
    missing_design = missing_design,
    panel_complete = panel_complete
  )

  class(result) <- "sensory_validation"

  cat("\n")
  cat("========================================\n")
  cat(" SensoryToolsR - DATA VALIDATION\n")
  cat("========================================\n\n")

  cat("Dataset\n")
  cat("  Rows:                    ",
      result$n_rows, "\n", sep = "")
  cat("  Columns:                 ",
      result$n_columns, "\n\n", sep = "")

  cat("Experimental structure\n")
  cat("  Assessors:               ",
      result$n_assessors, "\n", sep = "")
  cat("  Products:                ",
      result$n_products, "\n", sep = "")
  cat("  Sessions:                ",
      result$n_sessions, "\n\n", sep = "")

  cat("Sensory attributes\n")

  if (length(attributes) == 0) {
    cat("  None detected\n")
  } else {
    for (x in attributes) {
      cat("  - ", x, "\n", sep = "")
    }
  }

  cat("\nMissing values\n")
  cat("  Total:                   ",
      result$total_missing, "\n", sep = "")

  cat("\nDuplicate design records\n")
  cat("  Rows:                    ",
      result$n_duplicate_rows, "\n", sep = "")

  cat("\nPanel completeness\n")
  cat("  Expected design records: ",
      result$n_expected_design, "\n", sep = "")
  cat("  Observed design records: ",
      result$n_observed_design, "\n", sep = "")
  cat("  Missing design records:  ",
      result$n_missing_design, "\n", sep = "")

  if (result$panel_complete) {
    cat("  Panel completeness:      COMPLETE\n")
  } else {
    cat("  Panel completeness:      INCOMPLETE\n")
  }

  if (result$n_missing_design > 0) {

    cat("\nMissing assessor-product-session combinations\n")

    print(
      result$missing_design,
      row.names = FALSE
    )
  }

  cat("\n----------------------------------------\n")

  if (
    result$total_missing == 0 &&
    result$n_duplicate_rows == 0 &&
    result$n_missing_design == 0 &&
    length(attributes) > 0
  ) {

    cat(" STATUS: STRUCTURE COMPLETE\n")

  } else {

    cat(" STATUS: REVIEW DATA BEFORE ANALYSIS\n")
  }

  cat("========================================\n")

  invisible(result)
}
