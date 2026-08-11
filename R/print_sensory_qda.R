#' Print an integrated sensory QDA result
#'
#' Prints a concise console summary for an object returned by
#' `sensory_qda()`.
#'
#' @param x An object of class `sensory_qda`.
#' @param ... Additional arguments. Currently unused.
#'
#' @return The input object `x`, invisibly.
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
#' print(result)
#' }
#'
#' @export
print.sensory_qda <- function(
    x,
    ...
) {

  # --------------------------------------------------
  # Validate object
  # --------------------------------------------------

  if (!inherits(
    x,
    "sensory_qda"
  )) {
    stop(
      "`x` must be an object returned by sensory_qda().",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Extract overview information
  # --------------------------------------------------

  overview <- x$overview

  n_observations <-
    overview$n_observations

  n_products <-
    overview$n_products

  n_assessors <-
    overview$n_assessors

  n_sessions <-
    overview$n_sessions

  n_attributes <-
    overview$n_attributes

  n_significant_products <-
    overview$
    n_significant_product_attributes

  n_significant_interactions <-
    overview$
    n_significant_interactions

  pc1 <-
    overview$pca_pc1_percent

  pc2 <-
    overview$pca_pc2_percent

  pc12 <-
    overview$pca_pc1_pc2_percent

  # --------------------------------------------------
  # Formatting helper
  # --------------------------------------------------

  format_percent <- function(value) {

    if (
      length(value) == 0 ||
      is.na(value)
    ) {
      return("NA")
    }

    paste0(
      format(
        round(
          value,
          2
        ),
        nsmall = 2,
        trim = TRUE
      ),
      "%"
    )
  }

  # --------------------------------------------------
  # Console output
  # --------------------------------------------------

  cat(
    "\n"
  )

  cat(
    "SensoryToolsR - Integrated QDA Analysis\n"
  )

  cat(
    "=======================================\n\n"
  )

  cat(
    "Experimental design\n"
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "Products:",
      n_products
    )
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "Assessors:",
      n_assessors
    )
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "Sessions:",
      n_sessions
    )
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "Sensory attributes:",
      n_attributes
    )
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "Observations:",
      n_observations
    )
  )

  cat(
    "\n"
  )

  cat(
    "Panel analysis\n"
  )

  cat(
    sprintf(
      "  %-31s %s / %s\n",
      "Significant product effects:",
      n_significant_products,
      n_attributes
    )
  )

  cat(
    sprintf(
      "  %-31s %s / %s\n",
      "Significant P x A interactions:",
      n_significant_interactions,
      n_attributes
    )
  )

  cat(
    "\n"
  )

  cat(
    "Principal component analysis\n"
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "PC1:",
      format_percent(
        pc1
      )
    )
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "PC2:",
      format_percent(
        pc2
      )
    )
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "PC1 + PC2:",
      format_percent(
        pc12
      )
    )
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "Scaling:",
      if (
        isTRUE(
          x$settings$pca_scale
        )
      ) {
        "standardized"
      } else {
        "not standardized"
      }
    )
  )

  # --------------------------------------------------
  # Panel review summary
  # --------------------------------------------------

  assessor_tables <-
    x$panel$assessor_tables

  total_review_flags <- sum(
    vapply(
      assessor_tables,
      function(tbl) {

        if (
          !"status" %in%
          names(tbl)
        ) {
          return(
            0L
          )
        }

        sum(
          tbl$status ==
            "Review",
          na.rm = TRUE
        )
      },
      integer(1)
    )
  )

  attributes_with_review <- sum(
    vapply(
      assessor_tables,
      function(tbl) {

        if (
          !"status" %in%
          names(tbl)
        ) {
          return(
            FALSE
          )
        }

        any(
          tbl$status ==
            "Review",
          na.rm = TRUE
        )
      },
      logical(1)
    )
  )

  cat(
    "\n"
  )

  cat(
    "Assessor screening\n"
  )

  cat(
    sprintf(
      "  %-31s %s\n",
      "Total review flags:",
      total_review_flags
    )
  )

  cat(
    sprintf(
      "  %-31s %s / %s\n",
      "Attributes with review flags:",
      attributes_with_review,
      n_attributes
    )
  )

  cat(
    "\n"
  )

  cat(
    "Available results\n"
  )

  cat(
    "  $overview\n"
  )

  cat(
    "  $validation\n"
  )

  cat(
    "  $summary\n"
  )

  cat(
    "  $panel\n"
  )

  cat(
    "  $pca\n"
  )

  cat(
    "  $pca_diagnostics\n"
  )

  cat(
    "\n"
  )

  invisible(
    x
  )
}
