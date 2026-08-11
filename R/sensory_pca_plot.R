#' Plot a sensory PCA map
#'
#' Creates a PCA score plot, loading plot, or combined sensory biplot
#' from an object returned by `sensory_pca()`.
#'
#' @param x An object of class `sensory_pca`.
#' @param type Character. Plot type. One of `"biplot"`, `"scores"`,
#' or `"loadings"`. Default is `"biplot"`.
#' @param pc_x Integer. Principal component for the horizontal axis.
#' Default is 1.
#' @param pc_y Integer. Principal component for the vertical axis.
#' Default is 2.
#' @param label_products Logical. If TRUE, product labels are displayed.
#' Default is TRUE.
#' @param label_attributes Logical. If TRUE, sensory attribute labels
#' are displayed. Default is TRUE.
#' @param vector_scale Numeric or NULL. Scaling factor for loading vectors
#' in a biplot. If NULL, a suitable value is calculated automatically.
#' @param show_origin Logical. If TRUE, horizontal and vertical reference
#' lines are shown at zero. Default is TRUE.
#'
#' @return A ggplot object.
#'
#' @details
#' The `"scores"` plot displays products in PCA space.
#'
#' The `"loadings"` plot displays sensory attribute loading coordinates.
#'
#' The `"biplot"` combines product scores and loading vectors. Loading
#' vectors are rescaled only for visualization; the underlying PCA
#' loadings are not altered.
#'
#' Axis labels automatically report the percentage of total variance
#' explained by each selected principal component.
#'
#' @examples
#' \dontrun{
#' pca_result <- sensory_pca(
#'   data,
#'   attributes = c(
#'     "sweetness",
#'     "bitterness",
#'     "firmness"
#'   )
#' )
#'
#' sensory_pca_plot(
#'   pca_result,
#'   type = "biplot"
#' )
#' }
#'
#' @export
sensory_pca_plot <- function(
    x,
    type = c(
      "biplot",
      "scores",
      "loadings"
    ),
    pc_x = 1,
    pc_y = 2,
    label_products = TRUE,
    label_attributes = TRUE,
    vector_scale = NULL,
    show_origin = TRUE
) {

  # --------------------------------------------------
  # Validate PCA object
  # --------------------------------------------------

  if (!inherits(x, "sensory_pca")) {
    stop(
      "`x` must be an object returned by sensory_pca().",
      call. = FALSE
    )
  }

  type <- match.arg(type)

  # --------------------------------------------------
  # Validate PC axes
  # --------------------------------------------------

  available_components <- length(
    x$variance_explained
  )

  if (
    !is.numeric(pc_x) ||
    length(pc_x) != 1 ||
    is.na(pc_x) ||
    pc_x %% 1 != 0 ||
    pc_x < 1 ||
    pc_x > available_components
  ) {
    stop(
      "`pc_x` must identify an available principal component.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(pc_y) ||
    length(pc_y) != 1 ||
    is.na(pc_y) ||
    pc_y %% 1 != 0 ||
    pc_y < 1 ||
    pc_y > available_components
  ) {
    stop(
      "`pc_y` must identify an available principal component.",
      call. = FALSE
    )
  }

  if (pc_x == pc_y) {
    stop(
      "`pc_x` and `pc_y` must refer to different principal components.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate logical arguments
  # --------------------------------------------------

  logical_arguments <- list(
    label_products = label_products,
    label_attributes = label_attributes,
    show_origin = show_origin
  )

  invalid_logical <- vapply(
    logical_arguments,
    function(z) {
      !is.logical(z) ||
        length(z) != 1 ||
        is.na(z)
    },
    logical(1)
  )

  if (any(invalid_logical)) {
    stop(
      "`label_products`, `label_attributes`, and `show_origin` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate vector scale
  # --------------------------------------------------

  if (!is.null(vector_scale)) {

    if (
      !is.numeric(vector_scale) ||
      length(vector_scale) != 1 ||
      is.na(vector_scale) ||
      vector_scale <= 0
    ) {
      stop(
        "`vector_scale` must be NULL or a positive number.",
        call. = FALSE
      )
    }
  }

  # --------------------------------------------------
  # Select component names
  # --------------------------------------------------

  pc_x_name <- paste0(
    "PC",
    pc_x
  )

  pc_y_name <- paste0(
    "PC",
    pc_y
  )

  # --------------------------------------------------
  # Axis labels with explained variance
  # --------------------------------------------------

  x_variance <- x$variance_explained[
    pc_x
  ]

  y_variance <- x$variance_explained[
    pc_y
  ]

  x_label <- paste0(
    pc_x_name,
    " (",
    format(
      round(
        x_variance,
        2
      ),
      nsmall = 2
    ),
    "%)"
  )

  y_label <- paste0(
    pc_y_name,
    " (",
    format(
      round(
        y_variance,
        2
      ),
      nsmall = 2
    ),
    "%)"
  )

  # --------------------------------------------------
  # Prepare score data
  # --------------------------------------------------

  score_data <- data.frame(
    product = x$scores[[x$product]],
    x = x$scores[[pc_x_name]],
    y = x$scores[[pc_y_name]],
    stringsAsFactors = FALSE
  )

  # --------------------------------------------------
  # Prepare loading data
  # --------------------------------------------------

  loading_data <- data.frame(
    attribute = x$loadings$attribute,
    x = x$loadings[[pc_x_name]],
    y = x$loadings[[pc_y_name]],
    stringsAsFactors = FALSE
  )

  # --------------------------------------------------
  # Score plot
  # --------------------------------------------------

  if (type == "scores") {

    p <- ggplot2::ggplot(
      score_data,
      ggplot2::aes(
        x = .data$x,
        y = .data$y
      )
    ) +
      ggplot2::geom_point(
        size = 3
      )

    if (label_products) {

      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = .data$product
          ),
          vjust = -0.7
        )
    }

    p <- p +
      ggplot2::labs(
        title = "PCA product map",
        x = x_label,
        y = y_label
      )
  }

  # --------------------------------------------------
  # Loading plot
  # --------------------------------------------------

  if (type == "loadings") {

    p <- ggplot2::ggplot(
      loading_data,
      ggplot2::aes(
        x = .data$x,
        y = .data$y
      )
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 0,
          y = 0,
          xend = .data$x,
          yend = .data$y
        ),
        arrow = grid::arrow(
          length = grid::unit(
            0.18,
            "cm"
          )
        )
      )

    if (label_attributes) {

      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = .data$attribute
          ),
          vjust = -0.6
        )
    }

    p <- p +
      ggplot2::labs(
        title = "PCA sensory attribute loadings",
        x = x_label,
        y = y_label
      )
  }

  # --------------------------------------------------
  # Combined biplot
  # --------------------------------------------------

  if (type == "biplot") {

    if (is.null(vector_scale)) {

      score_limit <- max(
        abs(
          c(
            score_data$x,
            score_data$y
          )
        ),
        na.rm = TRUE
      )

      loading_limit <- max(
        abs(
          c(
            loading_data$x,
            loading_data$y
          )
        ),
        na.rm = TRUE
      )

      if (
        is.finite(score_limit) &&
        is.finite(loading_limit) &&
        loading_limit > 0
      ) {

        vector_scale <-
          0.75 *
          score_limit /
          loading_limit

      } else {

        vector_scale <- 1
      }
    }

    loading_data$x_plot <-
      loading_data$x *
      vector_scale

    loading_data$y_plot <-
      loading_data$y *
      vector_scale

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = score_data,
        ggplot2::aes(
          x = .data$x,
          y = .data$y
        ),
        size = 3
      )

    if (label_products) {

      p <- p +
        ggplot2::geom_text(
          data = score_data,
          ggplot2::aes(
            x = .data$x,
            y = .data$y,
            label = .data$product
          ),
          vjust = -0.7
        )
    }

    p <- p +
      ggplot2::geom_segment(
        data = loading_data,
        ggplot2::aes(
          x = 0,
          y = 0,
          xend = .data$x_plot,
          yend = .data$y_plot
        ),
        arrow = grid::arrow(
          length = grid::unit(
            0.18,
            "cm"
          )
        )
      )

    if (label_attributes) {

      p <- p +
        ggplot2::geom_text(
          data = loading_data,
          ggplot2::aes(
            x = .data$x_plot,
            y = .data$y_plot,
            label = .data$attribute
          ),
          vjust = -0.6
        )
    }

    p <- p +
      ggplot2::labs(
        title = "Sensory PCA biplot",
        x = x_label,
        y = y_label
      )
  }

  # --------------------------------------------------
  # Origin reference lines
  # --------------------------------------------------

  if (show_origin) {

    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.4
      ) +
      ggplot2::geom_vline(
        xintercept = 0,
        linewidth = 0.4
      )
  }

  # --------------------------------------------------
  # Common formatting
  # --------------------------------------------------

  p <- p +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold"
      ),
      axis.title = ggplot2::element_text(
        face = "bold"
      )
    )

  p
}
