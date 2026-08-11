#' Clean a sensory dataset
#'
#' Performs conservative cleaning of a sensory dataset by trimming
#' whitespace, standardizing character variables, removing empty rows
#' and columns, and optionally removing exact duplicate rows.
#'
#' @param data A data frame or tibble containing sensory data.
#' @param trim_whitespace Logical. If TRUE, leading and trailing whitespace
#' is removed from character variables.
#' @param empty_strings_to_na Logical. If TRUE, empty character strings are
#' converted to NA.
#' @param remove_empty Logical. If TRUE, completely empty rows and columns
#' are removed.
#' @param remove_duplicates Logical. If TRUE, exact duplicate rows are removed.
#'
#' @return A cleaned tibble.
#'
#' @examples
#' \dontrun{
#' clean_data <- sensory_clean(data)
#' }
#'
#' @export
sensory_clean <- function(
    data,
    trim_whitespace = TRUE,
    empty_strings_to_na = TRUE,
    remove_empty = TRUE,
    remove_duplicates = FALSE
) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  out <- tibble::as_tibble(data)

  original_rows <- nrow(out)
  original_columns <- ncol(out)

  # Trim whitespace in character columns
  if (trim_whitespace) {
    out[] <- lapply(
      out,
      function(x) {
        if (is.character(x)) {
          trimws(x)
        } else {
          x
        }
      }
    )
  }

  # Convert empty strings to NA
  if (empty_strings_to_na) {
    out[] <- lapply(
      out,
      function(x) {
        if (is.character(x)) {
          x[x == ""] <- NA_character_
        }
        x
      }
    )
  }

  # Remove completely empty rows and columns
  if (remove_empty) {
    out <- janitor::remove_empty(out, which = "rows")
    out <- janitor::remove_empty(out, which = "cols")
  }

  # Remove exact duplicate rows only if requested
  duplicates_removed <- 0L

  if (remove_duplicates) {
    before <- nrow(out)
    out <- dplyr::distinct(out)
    duplicates_removed <- before - nrow(out)
  }

  cli::cli_alert_success("Data cleaning completed.")
  cli::cli_inform("Rows before: {original_rows}")
  cli::cli_inform("Rows after:  {nrow(out)}")
  cli::cli_inform("Columns before: {original_columns}")
  cli::cli_inform("Columns after:  {ncol(out)}")

  if (remove_duplicates) {
    cli::cli_inform("Exact duplicate rows removed: {duplicates_removed}")
  }

  out
}
