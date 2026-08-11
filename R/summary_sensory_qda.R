#' Summarize an integrated sensory QDA result
#'
#' Creates structured summary tables from an object returned by
#' `sensory_qda()`.
#'
#' @param object An object of class `sensory_qda`.
#' @param top_n Integer. Number of top PCA attributes to retain for each
#' selected principal component. Default is 5.
#' @param ... Additional arguments. Currently unused.
#'
#' @return An object of class `summary_sensory_qda` containing:
#' \itemize{
#'   \item `overview`: experimental-design and PCA overview.
#'   \item `attribute_summary`: inferential summary for each sensory
#'   attribute.
#'   \item `assessor_summary`: assessor-review summary by attribute.
#'   \item `pca_variance`: PCA variance table.
#'   \item `pca_top_attributes`: top contributing sensory attributes.
#'   \item `settings`: analysis settings inherited from `sensory_qda()`.
#' }
#'
#' @examples
#' \dontrun{
#' qda_result <- sensory_qda(
#'   data,
#'   attributes = c(
#'     "sweetness",
#'     "bitterness",
#'     "firmness",
#'     "juiciness"
#'   )
#' )
#'
#' qda_summary <- summary(qda_result)
#'
#' qda_summary$attribute_summary
#' qda_summary$assessor_summary
#' qda_summary$pca_variance
#' qda_summary$pca_top_attributes
#' }
#'
#' @export
summary.sensory_qda <- function(
    object,
    top_n = 5,
    ...
) {

  # --------------------------------------------------
  # Validate object
  # --------------------------------------------------

  if (!inherits(
    object,
    "sensory_qda"
  )) {
    stop(
      "`object` must be an object returned by sensory_qda().",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate top_n
  # --------------------------------------------------

  if (
    !is.numeric(top_n) ||
    length(top_n) != 1 ||
    is.na(top_n) ||
    top_n %% 1 != 0 ||
    top_n < 1
  ) {
    stop(
      "`top_n` must be a positive integer.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Overview
  # --------------------------------------------------

  overview <- object$overview

  # --------------------------------------------------
  # Attribute-level inferential summary
  # --------------------------------------------------

  attribute_summary <-
    object$panel$attribute_summary

  attribute_summary <-
    tibble::as_tibble(
      attribute_summary
    )

  # --------------------------------------------------
  # Assessor screening summary by attribute
  # --------------------------------------------------

  assessor_tables <-
    object$panel$assessor_tables

  assessor_summary_list <- lapply(
    names(assessor_tables),
    function(attribute_name) {

      tbl <- assessor_tables[[attribute_name]]

      if (
        !"status" %in%
        names(tbl)
      ) {

        return(
          tibble::tibble(
            attribute =
              attribute_name,
            n_assessors =
              nrow(tbl),
            n_review =
              NA_integer_,
            percent_review =
              NA_real_,
            review_assessors =
              NA_character_
          )
        )
      }

      review_rows <-
        tbl$status ==
        "Review"

      review_ids <-
        tbl$assessor[
          review_rows
        ]

      n_review <-
        sum(
          review_rows,
          na.rm = TRUE
        )

      percent_review <-
        if (
          nrow(tbl) > 0
        ) {
          n_review /
            nrow(tbl) *
            100
        } else {
          NA_real_
        }

      tibble::tibble(
        attribute =
          attribute_name,
        n_assessors =
          nrow(tbl),
        n_review =
          n_review,
        percent_review =
          percent_review,
        review_assessors =
          if (
            length(review_ids) == 0
          ) {
            "None"
          } else {
            paste(
              review_ids,
              collapse = ", "
            )
          }
      )
    }
  )

  assessor_summary <- do.call(
    rbind,
    assessor_summary_list
  )

  assessor_summary <-
    tibble::as_tibble(
      assessor_summary
    )

  # --------------------------------------------------
  # PCA variance summary
  # --------------------------------------------------

  pca_variance <-
    object$pca$variance_table

  pca_variance <-
    tibble::as_tibble(
      pca_variance
    )

  # --------------------------------------------------
  # PCA top sensory attributes
  # --------------------------------------------------

  available_components <-
    object$pca_diagnostics$
    components

  top_attribute_list <- lapply(
    available_components,
    function(pc_number) {

      pc_name <- paste0(
        "PC",
        pc_number
      )

      current <-
        object$pca_diagnostics$
        attribute_diagnostics[
          object$pca_diagnostics$
            attribute_diagnostics$
            component ==
            pc_name,
          ,
          drop = FALSE
        ]

      current <- current[
        order(
          current$
            contribution_percent,
          decreasing = TRUE
        ),
        ,
        drop = FALSE
      ]

      current[
        seq_len(
          min(
            top_n,
            nrow(current)
          )
        ),
        ,
        drop = FALSE
      ]
    }
  )

  pca_top_attributes <- do.call(
    rbind,
    top_attribute_list
  )

  pca_top_attributes <-
    tibble::as_tibble(
      pca_top_attributes
    )

  # --------------------------------------------------
  # Integrated review summary
  # --------------------------------------------------

  review_overview <-
    tibble::tibble(
      n_attributes =
        nrow(
          assessor_summary
        ),
      attributes_with_review =
        sum(
          assessor_summary$
            n_review > 0,
          na.rm = TRUE
        ),
      total_review_flags =
        sum(
          assessor_summary$
            n_review,
          na.rm = TRUE
        )
    )

  # --------------------------------------------------
  # Build summary object
  # --------------------------------------------------

  result <- list(
    overview =
      overview,
    attribute_summary =
      attribute_summary,
    assessor_summary =
      assessor_summary,
    review_overview =
      review_overview,
    pca_variance =
      pca_variance,
    pca_top_attributes =
      pca_top_attributes,
    settings =
      object$settings
  )

  class(result) <-
    "summary_sensory_qda"

  result
}
