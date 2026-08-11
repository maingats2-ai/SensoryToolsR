#' Principal component analysis for sensory profiles
#'
#' Performs principal component analysis (PCA) on product sensory
#' profiles. Replicated sensory observations are first averaged by
#' product for each selected sensory attribute.
#'
#' @param data A data frame or tibble containing sensory data.
#' @param attributes Character vector containing the names of numeric
#' sensory attributes to include in the PCA.
#' @param product Character. Name of the product column.
#' Default is `"product"`.
#' @param center Logical. Should variables be centered before PCA?
#' Default is `TRUE`.
#' @param scale Logical. Should variables be standardized to unit
#' variance before PCA? Default is `FALSE`.
#'
#' @return An object of class `sensory_pca` containing:
#' \itemize{
#'   \item `product_profiles`: product-by-attribute mean matrix as a tibble.
#'   \item `pca_model`: the fitted `prcomp` object.
#'   \item `scores`: product coordinates on the principal components.
#'   \item `loadings`: sensory attribute loadings.
#'   \item `eigenvalues`: eigenvalues for each principal component.
#'   \item `variance_explained`: percentage variance explained by each
#'   principal component.
#'   \item `cumulative_variance`: cumulative percentage variance explained.
#'   \item `variance_table`: combined PCA variance summary.
#'   \item `attributes`: attributes included in the PCA.
#'   \item `product`: name of the product variable.
#'   \item `center`: centering setting.
#'   \item `scale`: scaling setting.
#' }
#'
#' @details
#' Sensory PCA is performed on mean product profiles rather than on
#' individual assessor observations.
#'
#' When all sensory attributes are measured using the same scale and have
#' comparable variances, unscaled PCA (`scale = FALSE`) is often useful.
#'
#' When attributes differ substantially in variance or measurement scale,
#' standardized PCA (`scale = TRUE`) may be appropriate.
#'
#' Missing sensory observations are ignored when product means are
#' calculated. However, PCA cannot proceed if a product-attribute mean
#' remains missing after aggregation.
#'
#' At least three products and two sensory attributes are required.
#'
#' @examples
#' \dontrun{
#' result <- sensory_pca(
#'   data,
#'   attributes = c(
#'     "sweetness",
#'     "bitterness",
#'     "firmness"
#'   )
#' )
#'
#' result$product_profiles
#' result$variance_table
#' result$scores
#' result$loadings
#'
#' standardized_result <- sensory_pca(
#'   data,
#'   attributes = c(
#'     "sweetness",
#'     "bitterness",
#'     "firmness"
#'   ),
#'   scale = TRUE
#' )
#' }
#'
#' @export
sensory_pca <- function(
    data,
    attributes,
    product = "product",
    center = TRUE,
    scale = FALSE
) {

  # --------------------------------------------------
  # Validate data
  # --------------------------------------------------

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame or tibble.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate attributes
  # --------------------------------------------------

  if (
    missing(attributes) ||
    !is.character(attributes) ||
    length(attributes) < 2
  ) {
    stop(
      "`attributes` must contain at least two sensory attribute names.",
      call. = FALSE
    )
  }

  if (anyDuplicated(attributes)) {
    stop(
      "`attributes` must not contain duplicate names.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate product argument
  # --------------------------------------------------

  if (
    !is.character(product) ||
    length(product) != 1 ||
    is.na(product) ||
    product == ""
  ) {
    stop(
      "`product` must be a single column name.",
      call. = FALSE
    )
  }

  if (!product %in% names(data)) {
    stop(
      "Missing product column: ",
      product,
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Check sensory attributes
  # --------------------------------------------------

  missing_attributes <- setdiff(
    attributes,
    names(data)
  )

  if (length(missing_attributes) > 0) {
    stop(
      "Missing sensory attribute column(s): ",
      paste(
        missing_attributes,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  non_numeric <- attributes[
    !vapply(
      data[attributes],
      is.numeric,
      logical(1)
    )
  ]

  if (length(non_numeric) > 0) {
    stop(
      "Sensory attribute(s) must be numeric: ",
      paste(
        non_numeric,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Validate center and scale
  # --------------------------------------------------

  if (
    !is.logical(center) ||
    length(center) != 1 ||
    is.na(center)
  ) {
    stop(
      "`center` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.logical(scale) ||
    length(scale) != 1 ||
    is.na(scale)
  ) {
    stop(
      "`scale` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Check number of products
  # --------------------------------------------------

  n_products <- length(
    unique(
      data[[product]][
        !is.na(data[[product]])
      ]
    )
  )

  if (n_products < 3) {
    stop(
      "PCA requires at least three products.",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Calculate product mean profiles
  # --------------------------------------------------

  product_profiles <- data |>
    dplyr::filter(
      !is.na(.data[[product]])
    ) |>
    dplyr::group_by(
      .data[[product]]
    ) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(attributes),
        ~ {
          if (all(is.na(.x))) {
            NA_real_
          } else {
            mean(
              .x,
              na.rm = TRUE
            )
          }
        }
      ),
      .groups = "drop"
    )

  # --------------------------------------------------
  # Check aggregated profile matrix
  # --------------------------------------------------

  profile_matrix <- as.data.frame(
    product_profiles[
      attributes
    ]
  )

  rownames(profile_matrix) <-
    as.character(
      product_profiles[[product]]
    )

  if (anyNA(profile_matrix)) {

    missing_locations <- which(
      is.na(profile_matrix),
      arr.ind = TRUE
    )

    missing_description <- paste(
      paste0(
        rownames(profile_matrix)[
          missing_locations[, "row"]
        ],
        ":",
        colnames(profile_matrix)[
          missing_locations[, "col"]
        ]
      ),
      collapse = ", "
    )

    stop(
      "PCA cannot be performed because product mean profiles contain missing values: ",
      missing_description,
      ".",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Check zero-variance attributes
  # --------------------------------------------------

  attribute_sd <- vapply(
    profile_matrix,
    stats::sd,
    numeric(1)
  )

  zero_variance_attributes <-
    names(attribute_sd)[
      is.na(attribute_sd) |
        attribute_sd == 0
    ]

  if (length(zero_variance_attributes) > 0) {
    stop(
      "PCA cannot be performed because the following attribute(s) have zero variance across products: ",
      paste(
        zero_variance_attributes,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Run PCA
  # --------------------------------------------------

  pca_model <- stats::prcomp(
    profile_matrix,
    center = center,
    scale. = scale
  )

  # --------------------------------------------------
  # Determine available components
  # --------------------------------------------------

  component_names <- paste0(
    "PC",
    seq_along(pca_model$sdev)
  )

  # --------------------------------------------------
  # Product scores
  # --------------------------------------------------

  scores_matrix <- pca_model$x

  colnames(scores_matrix) <-
    paste0(
      "PC",
      seq_len(
        ncol(scores_matrix)
      )
    )

  scores <- tibble::as_tibble(
    scores_matrix,
    rownames = product
  )

  # --------------------------------------------------
  # Attribute loadings
  # --------------------------------------------------

  loadings_matrix <- pca_model$rotation

  colnames(loadings_matrix) <-
    paste0(
      "PC",
      seq_len(
        ncol(loadings_matrix)
      )
    )

  loadings <- tibble::as_tibble(
    loadings_matrix,
    rownames = "attribute"
  )

  # --------------------------------------------------
  # Eigenvalues and explained variance
  # --------------------------------------------------

  eigenvalues <- pca_model$sdev^2

  total_variance <- sum(
    eigenvalues
  )

  variance_explained <-
    eigenvalues /
    total_variance *
    100

  cumulative_variance <-
    cumsum(
      variance_explained
    )

  variance_table <- tibble::tibble(
    component = component_names,
    eigenvalue = eigenvalues,
    variance_percent =
      variance_explained,
    cumulative_percent =
      cumulative_variance
  )

  # --------------------------------------------------
  # Return result
  # --------------------------------------------------

  result <- list(
    product_profiles =
      tibble::as_tibble(
        product_profiles
      ),
    pca_model =
      pca_model,
    scores =
      scores,
    loadings =
      loadings,
    eigenvalues =
      eigenvalues,
    variance_explained =
      variance_explained,
    cumulative_variance =
      cumulative_variance,
    variance_table =
      variance_table,
    attributes =
      attributes,
    product =
      product,
    center =
      center,
    scale =
      scale
  )

  class(result) <- "sensory_pca"

  # --------------------------------------------------
  # Console report
  # --------------------------------------------------

  cli::cli_alert_success(
    "Sensory PCA completed."
  )

  cli::cli_inform(
    "Products: {nrow(product_profiles)}"
  )

  cli::cli_inform(
    "Attributes: {length(attributes)}"
  )

  cli::cli_inform(
    "Scaling: {if (scale) 'standardized' else 'not standardized'}"
  )

  cli::cli_inform(
    "PC1 variance: {format(round(variance_explained[1], 2), nsmall = 2)}%"
  )

  if (length(variance_explained) >= 2) {
    cli::cli_inform(
      "PC2 variance: {format(round(variance_explained[2], 2), nsmall = 2)}%"
    )

    cli::cli_inform(
      "PC1 + PC2: {format(round(sum(variance_explained[1:2]), 2), nsmall = 2)}%"
    )
  }

  result
}
