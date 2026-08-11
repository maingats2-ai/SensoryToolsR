#' PCA diagnostics for sensory profiles
#'
#' Calculates variable and product diagnostics from a `sensory_pca`
#' object, including loadings, scores, contributions, cos2 values,
#' explained variance, and convenient top-contributor summaries.
#'
#' @param x An object of class `sensory_pca`.
#' @param components Integer vector identifying principal components to
#' include. Default is `c(1, 2)`.
#' @param top_n Integer. Number of top attributes and products to retain
#' for each component. Default is 5.
#'
#' @return An object of class `sensory_pca_diagnostics` containing:
#' \itemize{
#'   \item `attribute_diagnostics`: attribute-level loading,
#'   contribution, and cos2 values by component.
#'   \item `product_diagnostics`: product-level score,
#'   contribution, and cos2 values by component.
#'   \item `variance_table`: PCA variance summary.
#'   \item `top_attributes`: top contributing sensory attributes
#'   for each selected component.
#'   \item `top_products`: top contributing products for each
#'   selected component.
#'   \item `components`: selected principal components.
#'   \item `top_n`: requested number of top contributors.
#' }
#'
#' @details
#' Attribute contribution is calculated from squared PCA loadings,
#' normalized within each component.
#'
#' Attribute cos2 is calculated from squared loading divided by the
#' sum of squared loadings for that attribute across all retained PCs.
#'
#' Product contribution is calculated from squared product scores,
#' normalized within each principal component.
#'
#' Product cos2 is calculated from squared score divided by the
#' squared distance of the product from the PCA origin.
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
#' diagnostics <- sensory_pca_diagnostics(
#'   pca_result,
#'   components = c(1, 2),
#'   top_n = 3
#' )
#'
#' diagnostics$attribute_diagnostics
#' diagnostics$product_diagnostics
#' }
#'
#' @export
sensory_pca_diagnostics <- function(
    x,
    components = c(1, 2),
    top_n = 5
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

  available_components <- length(
    x$variance_explained
  )

  # --------------------------------------------------
  # Validate components
  # --------------------------------------------------

  if (
    !is.numeric(components) ||
    length(components) < 1 ||
    any(is.na(components)) ||
    any(components %% 1 != 0) ||
    any(components < 1) ||
    any(components > available_components)
  ) {
    stop(
      "`components` must contain valid principal component numbers.",
      call. = FALSE
    )
  }

  if (anyDuplicated(components)) {
    stop(
      "`components` must not contain duplicate values.",
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

  component_names <- paste0(
    "PC",
    components
  )

  # --------------------------------------------------
  # Attribute diagnostics
  # --------------------------------------------------

  loading_matrix <- as.matrix(
    x$loadings[
      setdiff(
        names(x$loadings),
        "attribute"
      )
    ]
  )

  rownames(loading_matrix) <-
    x$loadings$attribute

  loading_sq <- loading_matrix^2

  attribute_total_sq <- rowSums(
    loading_sq
  )

  attribute_diag_list <- lapply(
    seq_along(components),
    function(i) {

      pc_number <- components[i]

      pc_name <- paste0(
        "PC",
        pc_number
      )

      loadings_pc <- loading_matrix[
        ,
        pc_name
      ]

      loading_sq_pc <- loadings_pc^2

      contribution_percent <-
        loading_sq_pc /
        sum(loading_sq_pc) *
        100

      cos2 <- loading_sq_pc /
        attribute_total_sq

      tibble::tibble(
        attribute = rownames(
          loading_matrix
        ),
        component = pc_name,
        loading = as.numeric(
          loadings_pc
        ),
        loading_abs = abs(
          as.numeric(
            loadings_pc
          )
        ),
        contribution_percent =
          as.numeric(
            contribution_percent
          ),
        cos2 = as.numeric(
          cos2
        )
      )
    }
  )

  attribute_diagnostics <- do.call(
    rbind,
    attribute_diag_list
  )

  attribute_diagnostics <-
    tibble::as_tibble(
      attribute_diagnostics
    )

  # --------------------------------------------------
  # Product diagnostics
  # --------------------------------------------------

  score_matrix <- as.matrix(
    x$scores[
      setdiff(
        names(x$scores),
        x$product
      )
    ]
  )

  rownames(score_matrix) <-
    x$scores[[x$product]]

  score_sq <- score_matrix^2

  product_total_sq <- rowSums(
    score_sq
  )

  product_diag_list <- lapply(
    seq_along(components),
    function(i) {

      pc_number <- components[i]

      pc_name <- paste0(
        "PC",
        pc_number
      )

      scores_pc <- score_matrix[
        ,
        pc_name
      ]

      score_sq_pc <- scores_pc^2

      score_sum_sq <- sum(
        score_sq_pc
      )

      contribution_percent <- if (
        score_sum_sq > 0
      ) {
        score_sq_pc /
          score_sum_sq *
          100
      } else {
        rep(
          NA_real_,
          length(score_sq_pc)
        )
      }

      cos2 <- ifelse(
        product_total_sq > 0,
        score_sq_pc /
          product_total_sq,
        NA_real_
      )

      tibble::tibble(
        product = rownames(
          score_matrix
        ),
        component = pc_name,
        score = as.numeric(
          scores_pc
        ),
        score_abs = abs(
          as.numeric(
            scores_pc
          )
        ),
        contribution_percent =
          as.numeric(
            contribution_percent
          ),
        cos2 = as.numeric(
          cos2
        )
      )
    }
  )

  product_diagnostics <- do.call(
    rbind,
    product_diag_list
  )

  product_diagnostics <-
    tibble::as_tibble(
      product_diagnostics
    )

  # --------------------------------------------------
  # Top attributes
  # --------------------------------------------------

  top_attributes_list <- lapply(
    component_names,
    function(pc_name) {

      current <- attribute_diagnostics[
        attribute_diagnostics$component ==
          pc_name,
        ,
        drop = FALSE
      ]

      current <- current[
        order(
          current$contribution_percent,
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

  top_attributes <- do.call(
    rbind,
    top_attributes_list
  )

  top_attributes <-
    tibble::as_tibble(
      top_attributes
    )

  # --------------------------------------------------
  # Top products
  # --------------------------------------------------

  top_products_list <- lapply(
    component_names,
    function(pc_name) {

      current <- product_diagnostics[
        product_diagnostics$component ==
          pc_name,
        ,
        drop = FALSE
      ]

      current <- current[
        order(
          current$contribution_percent,
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

  top_products <- do.call(
    rbind,
    top_products_list
  )

  top_products <-
    tibble::as_tibble(
      top_products
    )

  # --------------------------------------------------
  # Variance table for selected PCs
  # --------------------------------------------------

  variance_table <- x$variance_table[
    components,
    ,
    drop = FALSE
  ]

  variance_table <-
    tibble::as_tibble(
      variance_table
    )

  # --------------------------------------------------
  # Return result
  # --------------------------------------------------

  result <- list(
    attribute_diagnostics =
      attribute_diagnostics,
    product_diagnostics =
      product_diagnostics,
    variance_table =
      variance_table,
    top_attributes =
      top_attributes,
    top_products =
      top_products,
    components =
      components,
    top_n =
      top_n
  )

  class(result) <-
    "sensory_pca_diagnostics"

  # --------------------------------------------------
  # Console summary
  # --------------------------------------------------

  cli::cli_alert_success(
    "Sensory PCA diagnostics completed."
  )

  cli::cli_inform(
    "Components analysed: {paste(component_names, collapse = ', ')}"
  )

  cli::cli_inform(
    "Attributes: {length(unique(attribute_diagnostics$attribute))}"
  )

  cli::cli_inform(
    "Products: {length(unique(product_diagnostics$product))}"
  )

  result
}
