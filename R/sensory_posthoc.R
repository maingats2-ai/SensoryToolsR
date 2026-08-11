#' Perform Tukey post-hoc comparisons for sensory ANOVA
#'
#' Performs Tukey's Honest Significant Difference test for the product
#' effect from a `sensory_anova` result.
#'
#' @param x An object of class `sensory_anova`.
#' @param conf_level Numeric. Confidence level for Tukey confidence intervals.
#' Default is 0.95.
#'
#' @return An object of class `sensory_posthoc` containing Tukey pairwise
#' comparisons for products.
#'
#' @examples
#' \dontrun{
#' fit <- sensory_anova(
#'   data,
#'   attribute = "sweetness"
#' )
#'
#' posthoc <- sensory_posthoc(fit)
#' }
#'
#' @export
sensory_posthoc <- function(
    x,
    conf_level = 0.95
) {

  if (!inherits(x, "sensory_anova")) {
    stop(
      "`x` must be an object returned by sensory_anova().",
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

  tukey_result <- stats::TukeyHSD(
    x$model,
    which = x$product,
    conf.level = conf_level
  )

  comparisons <- as.data.frame(
    tukey_result[[x$product]]
  )

  comparisons$comparison <- rownames(comparisons)

  rownames(comparisons) <- NULL

  comparisons <- comparisons[
    ,
    c(
      "comparison",
      "diff",
      "lwr",
      "upr",
      "p adj"
    )
  ]

  names(comparisons) <- c(
    "comparison",
    "difference",
    "ci_lower",
    "ci_upper",
    "p_adjusted"
  )

  comparisons$significant <- comparisons$p_adjusted < 0.05

  result <- list(
    attribute = x$attribute,
    product = x$product,
    conf_level = conf_level,
    comparisons = tibble::as_tibble(comparisons),
    method = "Tukey HSD"
  )

  class(result) <- "sensory_posthoc"

  cli::cli_alert_success(
    "Sensory post-hoc analysis completed."
  )

  cli::cli_inform(
    "Attribute: {x$attribute}"
  )

  cli::cli_inform(
    "Method: Tukey HSD"
  )

  cli::cli_inform(
    "Comparisons: {nrow(comparisons)}"
  )

  result
}
