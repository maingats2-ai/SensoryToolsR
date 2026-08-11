#' Plot sensory panel performance diagnostics
#'
#' Creates assessor-level diagnostic plots from an object returned by
#' `sensory_panel_performance()`.
#'
#' @param x An object of class `sensory_panel_performance`.
#' @param metric Character. Performance metric to plot. One of
#' `"discrimination"`, `"repeatability"`, or `"agreement"`.
#' @param sort Logical. If TRUE, assessors are ordered by the plotted metric.
#' Default is TRUE.
#' @param show_status Logical. If TRUE, assessor review status is shown using
#' point shape. Default is TRUE.
#'
#' @return A ggplot object.
#'
#' @details
#' Available metrics are:
#'
#' * `discrimination`: assessor product-discrimination F-value.
#' * `repeatability`: assessor repeatability RMSE.
#' * `agreement`: Pearson correlation between assessor product means
#'   and the corresponding means from the remaining panel.
#'
#' For discrimination, larger values generally indicate stronger product
#' differentiation. For repeatability, smaller values indicate better
#' repeatability. For agreement, values closer to 1 indicate stronger
#' agreement with the panel.
#'
#' @examples
#' \dontrun{
#' performance <- sensory_panel_performance(
#'   data,
#'   attribute = "sweetness"
#' )
#'
#' sensory_panel_plot(
#'   performance,
#'   metric = "discrimination"
#' )
#' }
#'
#' @export
sensory_panel_plot <- function(
    x,
    metric = c(
      "discrimination",
      "repeatability",
      "agreement"
    ),
    sort = TRUE,
    show_status = TRUE
) {

  if (!inherits(
    x,
    "sensory_panel_performance"
  )) {
    stop(
      "`x` must be an object returned by sensory_panel_performance().",
      call. = FALSE
    )
  }

  metric <- match.arg(metric)

  if (
    !is.logical(sort) ||
    length(sort) != 1 ||
    is.na(sort)
  ) {
    stop(
      "`sort` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(show_status) ||
    length(show_status) != 1 ||
    is.na(show_status)
  ) {
    stop(
      "`show_status` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  plot_data <- x$assessor_table

  if (metric == "discrimination") {

    plot_data$metric_value <-
      plot_data$discrimination_f

    y_label <- "Product discrimination F-value"

    plot_title <- paste0(
      "Panel discrimination - ",
      x$attribute
    )

    reference_value <- NULL
  }

  if (metric == "repeatability") {

    plot_data$metric_value <-
      plot_data$repeatability_rmse

    y_label <- "Repeatability RMSE"

    plot_title <- paste0(
      "Panel repeatability - ",
      x$attribute
    )

    reference_value <-
      x$panel_summary$
      repeatability_screening_limit
  }

  if (metric == "agreement") {

    plot_data$metric_value <-
      plot_data$agreement_correlation

    y_label <- "Agreement correlation"

    plot_title <- paste0(
      "Panel agreement - ",
      x$attribute
    )

    reference_value <-
      x$settings$
      agreement_threshold
  }

  if (all(is.na(plot_data$metric_value))) {
    stop(
      "No usable values are available for the selected metric.",
      call. = FALSE
    )
  }

  if (sort) {

    decreasing <- metric != "repeatability"

    assessor_order <- plot_data$assessor[
      order(
        plot_data$metric_value,
        decreasing = decreasing,
        na.last = TRUE
      )
    ]

    plot_data$assessor <- factor(
      plot_data$assessor,
      levels = assessor_order
    )
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$assessor,
      y = .data$metric_value
    )
  )

  if (show_status) {

    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(
          shape = .data$status
        ),
        size = 3
      )

  } else {

    p <- p +
      ggplot2::geom_point(
        size = 3
      )
  }

  if (!is.null(reference_value)) {

    p <- p +
      ggplot2::geom_hline(
        yintercept = reference_value,
        linetype = 2
      )
  }

  p <- p +
    ggplot2::labs(
      title = plot_title,
      x = "Assessor",
      y = y_label,
      shape = "Status"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  if (metric == "agreement") {

    p <- p +
      ggplot2::coord_cartesian(
        ylim = c(-1, 1)
      )
  }

  p
}
