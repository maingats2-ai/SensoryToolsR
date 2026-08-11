#' Import a sensory dataset
#'
#' Imports a sensory dataset from an Excel or CSV file and returns
#' a tibble. Column names are cleaned to a consistent format.
#'
#' @param file Character string giving the path to the data file.
#' Supported formats are `.xlsx`, `.xls`, and `.csv`.
#'
#' @param sheet Sheet name or sheet number to read when importing
#' an Excel file. Default is 1.
#'
#' @param clean_names Logical. If TRUE, column names are cleaned
#' using `janitor::clean_names()`. Default is TRUE.
#'
#' @param na_values Character vector containing values that should
#' be treated as missing. Default values are "", "NA", "N/A",
#' "na", and "n/a".
#'
#' @return A tibble containing the imported sensory dataset.
#'
#' @details
#' The function supports Excel and CSV files. It performs basic
#' validation of the file extension and reports the number of
#' rows and columns imported.
#'
#' This function is designed as the first step in a reproducible
#' sensory-data analysis workflow.
#'
#' @examples
#' \dontrun{
#' coffee <- sensory_import("Coffee_QDA.xlsx")
#'
#' sensory_data <- sensory_import(
#'   "sensory_data.csv"
#' )
#' }
#'
#' @export

sensory_import <- function(
    file,
    sheet = 1,
    clean_names = TRUE,
    na_values = c("", "NA", "N/A", "na", "n/a")
) {

  # Check that file argument is supplied
  if (missing(file) || !is.character(file) || length(file) != 1) {
    stop(
      "Please provide a single file path.",
      call. = FALSE
    )
  }

  # Check that file exists
  if (!file.exists(file)) {
    stop(
      "File not found: ", file,
      call. = FALSE
    )
  }

  # Identify file extension
  extension <- tolower(tools::file_ext(file))

  # Import Excel file
  if (extension %in% c("xlsx", "xls")) {

    data <- readxl::read_excel(
      path = file,
      sheet = sheet,
      na = na_values
    )

    # Import CSV file
  } else if (extension == "csv") {

    data <- readr::read_csv(
      file = file,
      na = na_values,
      show_col_types = FALSE
    )

  } else {

    stop(
      "Unsupported file format: .", extension,
      ". Please use .xlsx, .xls, or .csv.",
      call. = FALSE
    )
  }

  # Clean column names
  if (clean_names) {
    data <- janitor::clean_names(data)
  }

  # Report import information
  cli::cli_alert_success(
    "Data imported successfully."
  )

  cli::cli_inform(
    "Rows: {nrow(data)}"
  )

  cli::cli_inform(
    "Columns: {ncol(data)}"
  )

  return(data)
}
