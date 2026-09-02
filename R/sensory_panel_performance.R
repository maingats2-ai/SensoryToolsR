#' Evaluate sensory panel performance by assessor
#'
#' Calculates assessor-level discrimination, repeatability, agreement,
#' scale-use, residual error, and experimental-design completeness for
#' one sensory attribute.
#'
#' @param data A data frame or tibble containing replicated sensory data.
#' @param attribute Character. Name of one numeric sensory attribute.
#' @param product Character. Name of the product/sample column.
#' @param assessor Character. Name of the assessor column.
#' @param session Character. Name of the session or replicate column.
#' @param alpha Numeric. Significance level used to flag weak product
#' discrimination. Default is 0.05.
#' @param agreement_threshold Numeric. Correlation below this value is
#' flagged for review. Default is 0.70.
#' @param repeatability_multiplier Numeric. An assessor is flagged for
#' unusually high repeatability error when their repeatability RMSE is
#' greater than this multiplier times the panel median RMSE.
#' Default is 1.5.
#'
#' @return An object of class `sensory_panel_performance` containing:
#' \itemize{
#'   \item `assessor_table`: assessor-level performance statistics.
#'   \item `panel_summary`: panel-level summary statistics.
#'   \item `attribute`: analysed sensory attribute.
#'   \item `settings`: thresholds used for screening.
#' }
#'
#' @details
#' For each assessor, product discrimination is evaluated using:
#'
#' `score ~ product + session`
#'
#' Repeatability is quantified as the root mean squared deviation of
#' repeated scores from each assessor-product mean.
#'
#' Agreement is calculated as the Pearson correlation between an
#' assessor's product means and the corresponding product means from
#' the remaining panel members.
#'
#' The `status` and `review_reason` fields are screening aids rather than
#' formal ISO acceptance criteria.
#'
#' @examples
#' \dontrun{
#' performance <- sensory_panel_performance(
#'   data,
#'   attribute = "sweetness"
#' )
#'
#' performance$assessor_table
#' }
#'
#' @export
sensory_panel_performance <- function(
    data,
    attribute,
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
    missing(attribute) ||
    !is.character(attribute) ||
    length(attribute) != 1
  ) {
    stop(
      "`attribute` must be the name of one sensory attribute.",
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

  if (
    !is.numeric(alpha) ||
    length(alpha) != 1 ||
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
    repeatability_multiplier <= 0
  ) {
    stop(
      "`repeatability_multiplier` must be greater than 0.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Prepare complete observations
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
      "No complete observations available for panel performance analysis.",
      call. = FALSE
    )
  }

  products <- sort(
    unique(analysis_data[[product]])
  )

  assessors <- sort(
    unique(analysis_data[[assessor]])
  )

  sessions <- sort(
    unique(analysis_data[[session]])
  )

  if (length(products) < 2) {
    stop(
      "At least two products are required.",
      call. = FALSE
    )
  }

  if (length(assessors) < 2) {
    stop(
      "At least two assessors are required.",
      call. = FALSE
    )
  }

  if (length(sessions) < 2) {
    stop(
      "At least two sessions or replicates are required.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Internal standardized dataset
  # --------------------------------------------------

  internal_data <- data.frame(
    assessor = analysis_data[[assessor]],
    product = analysis_data[[product]],
    session = analysis_data[[session]],
    score = analysis_data[[attribute]],
    stringsAsFactors = FALSE
  )

  internal_data$assessor <- factor(
    internal_data$assessor,
    levels = assessors
  )

  internal_data$product <- factor(
    internal_data$product,
    levels = products
  )

  internal_data$session <- factor(
    internal_data$session,
    levels = sessions
  )

  expected_records <- length(products) *
    length(sessions)

  # --------------------------------------------------
  # Calculate assessor-level statistics
  # --------------------------------------------------

  assessor_results <- lapply(
    assessors,
    function(current_assessor) {

      assessor_data <- internal_data[
        internal_data$assessor == current_assessor,
        ,
        drop = FALSE
      ]

      assessor_data$assessor <- droplevels(
        assessor_data$assessor
      )

      assessor_data$product <- droplevels(
        assessor_data$product
      )

      assessor_data$session <- droplevels(
        assessor_data$session
      )

      observed_design <- unique(
        assessor_data[c("product", "session")]
      )

      n_observed_design <- nrow(
        observed_design
      )

      design_complete <- n_observed_design ==
        expected_records

      # -----------------------------------------------
      # Scale use
      # -----------------------------------------------

      assessor_mean <- mean(
        assessor_data$score
      )

      assessor_sd <- stats::sd(
        assessor_data$score
      )

      assessor_min <- min(
        assessor_data$score
      )

      assessor_max <- max(
        assessor_data$score
      )

      assessor_range <- assessor_max -
        assessor_min

      # -----------------------------------------------
      # Discrimination model
      # -----------------------------------------------

      discrimination_f <- NA_real_
      discrimination_p <- NA_real_
      session_f <- NA_real_
      session_p <- NA_real_
      residual_mse <- NA_real_

      if (
        length(unique(assessor_data$product)) >= 2 &&
        length(unique(assessor_data$session)) >= 2
      ) {

        fit <- stats::lm(
          score ~ product + session,
          data = assessor_data
        )

        fit_anova <- stats::anova(
          fit
        )

        if ("product" %in% rownames(fit_anova)) {

          discrimination_f <- fit_anova[
            "product",
            "F value"
          ]

          discrimination_p <- fit_anova[
            "product",
            "Pr(>F)"
          ]
        }

        if ("session" %in% rownames(fit_anova)) {

          session_f <- fit_anova[
            "session",
            "F value"
          ]

          session_p <- fit_anova[
            "session",
            "Pr(>F)"
          ]
        }

        if (
          stats::df.residual(fit) > 0
        ) {

          residual_mse <- stats::deviance(fit) /
            stats::df.residual(fit)
        }
      }

      # -----------------------------------------------
      # Repeatability
      # -----------------------------------------------

      product_means <- stats::aggregate(
        score ~ product,
        data = assessor_data,
        FUN = mean
      )

      repeatability_data <- merge(
        assessor_data,
        product_means,
        by = "product",
        suffixes = c("", "_product_mean")
      )

      repeatability_rmse <- sqrt(
        mean(
          (
            repeatability_data$score -
              repeatability_data$score_product_mean
          )^2
        )
      )

      # -----------------------------------------------
      # Agreement with the remaining panel
      # -----------------------------------------------

      other_data <- internal_data[
        internal_data$assessor != current_assessor,
        ,
        drop = FALSE
      ]

      other_panel_means <- stats::aggregate(
        score ~ product,
        data = other_data,
        FUN = mean
      )

      names(other_panel_means)[2] <-
        "other_panel_mean"

      assessor_product_means <- product_means

      names(assessor_product_means)[2] <-
        "assessor_product_mean"

      agreement_data <- merge(
        assessor_product_means,
        other_panel_means,
        by = "product"
      )

      agreement_correlation <- NA_real_

      if (
        nrow(agreement_data) >= 3 &&
        stats::sd(
          agreement_data$assessor_product_mean
        ) > 0 &&
        stats::sd(
          agreement_data$other_panel_mean
        ) > 0
      ) {

        agreement_correlation <- stats::cor(
          agreement_data$assessor_product_mean,
          agreement_data$other_panel_mean,
          method = "pearson"
        )
      }

      data.frame(
        assessor = as.character(
          current_assessor
        ),
        n_observations = nrow(
          assessor_data
        ),
        expected_design_records = expected_records,
        observed_design_records = n_observed_design,
        design_complete = design_complete,
        mean_score = assessor_mean,
        sd_score = assessor_sd,
        min_score = assessor_min,
        max_score = assessor_max,
        score_range = assessor_range,
        discrimination_f = discrimination_f,
        discrimination_p = discrimination_p,
        session_f = session_f,
        session_p = session_p,
        repeatability_rmse = repeatability_rmse,
        residual_mse = residual_mse,
        agreement_correlation = agreement_correlation,
        stringsAsFactors = FALSE
      )
    }
  )

  assessor_table <- do.call(
    rbind,
    assessor_results
  )

  rownames(assessor_table) <- NULL

  # --------------------------------------------------
  # Relative repeatability screening
  # --------------------------------------------------

  panel_median_rmse <- stats::median(
    assessor_table$repeatability_rmse,
    na.rm = TRUE
  )

  repeatability_limit <-
    repeatability_multiplier *
    panel_median_rmse

  assessor_table$discrimination_flag <-
    is.na(assessor_table$discrimination_p) |
    assessor_table$discrimination_p >= alpha

  assessor_table$agreement_flag <-
    !is.na(
      assessor_table$agreement_correlation
    ) &
    assessor_table$agreement_correlation <
    agreement_threshold

  assessor_table$repeatability_flag <-
    !is.na(
      assessor_table$repeatability_rmse
    ) &
    assessor_table$repeatability_rmse >
    repeatability_limit

  assessor_table$design_flag <-
    !assessor_table$design_complete

  # --------------------------------------------------
  # Build review reasons
  # --------------------------------------------------

  assessor_table$review_reason <- vapply(
    seq_len(nrow(assessor_table)),
    function(i) {

      reasons <- character(0)

      if (assessor_table$design_flag[i]) {
        reasons <- c(
          reasons,
          "Incomplete design"
        )
      }

      if (assessor_table$discrimination_flag[i]) {
        reasons <- c(
          reasons,
          "Weak product discrimination"
        )
      }

      if (assessor_table$agreement_flag[i]) {
        reasons <- c(
          reasons,
          "Low agreement with panel"
        )
      }

      if (assessor_table$repeatability_flag[i]) {
        reasons <- c(
          reasons,
          "High repeatability error"
        )
      }

      if (length(reasons) == 0) {
        "None"
      } else {
        paste(
          reasons,
          collapse = "; "
        )
      }
    },
    character(1)
  )

  assessor_table$status <- ifelse(
    assessor_table$review_reason == "None",
    "OK",
    "Review"
  )

  assessor_table <- tibble::as_tibble(
    assessor_table
  )

  # --------------------------------------------------
  # Panel-level summary
  # --------------------------------------------------

  panel_summary <- tibble::tibble(
    attribute = attribute,
    n_assessors = length(assessors),
    n_products = length(products),
    n_sessions = length(sessions),
    n_observations = nrow(analysis_data),
    median_repeatability_rmse =
      panel_median_rmse,
    repeatability_screening_limit =
      repeatability_limit,
    median_agreement_correlation =
      stats::median(
        assessor_table$agreement_correlation,
        na.rm = TRUE
      ),
    n_assessors_review =
      sum(
        assessor_table$status == "Review"
      )
  )

  # --------------------------------------------------
  # Return object
  # --------------------------------------------------

  result <- list(
    attribute = attribute,
    assessor_table = assessor_table,
    panel_summary = panel_summary,
    settings = list(
      alpha = alpha,
      agreement_threshold =
        agreement_threshold,
      repeatability_multiplier =
        repeatability_multiplier
    )
  )

  class(result) <-
    "sensory_panel_performance"

  cli::cli_alert_success(
    "Sensory panel performance analysis completed."
  )

  cli::cli_inform(
    "Attribute: {attribute}"
  )

  cli::cli_inform(
    "Assessors: {length(assessors)}"
  )

  cli::cli_inform(
    "Products: {length(products)}"
  )

  cli::cli_inform(
    "Sessions: {length(sessions)}"
  )

  cli::cli_inform(
    "Assessors flagged for review: {panel_summary$n_assessors_review}"
  )

  result
}
