make_pca_data <- function() {

  set.seed(2026)

  test_data <- expand.grid(
    assessor = paste0(
      "A0",
      1:6
    ),
    session = paste0(
      "S",
      1:3
    ),
    product = paste0(
      "P",
      1:6
    ),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  product_profiles <- data.frame(
    product = paste0(
      "P",
      1:6
    ),

    sweetness = c(
      7.2, 6.8, 5.0,
      4.7, 3.2, 3.5
    ),

    bitterness = c(
      2.0, 2.5, 4.0,
      4.3, 6.3, 6.0
    ),

    umami = c(
      7.0, 6.5, 5.2,
      5.0, 3.6, 3.8
    ),

    fishy_odor = c(
      2.0, 2.7, 4.3,
      4.0, 6.8, 6.3
    ),

    fresh_odor = c(
      7.5, 6.8, 5.1,
      5.3, 3.1, 3.4
    ),

    aftertaste = c(
      2.5, 3.0, 4.5,
      4.2, 6.4, 6.0
    ),

    firmness = c(
      7.2, 3.6, 7.0,
      3.8, 6.8, 3.5
    ),

    juiciness = c(
      3.5, 7.1, 3.8,
      6.9, 4.0, 7.0
    )
  )

  test_data <- merge(
    test_data,
    product_profiles,
    by = "product",
    sort = FALSE
  )

  attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  assessor_bias <- c(
    A01 = 0.00,
    A02 = 0.15,
    A03 = -0.12,
    A04 = 0.10,
    A05 = -0.08,
    A06 = 0.05
  )

  session_bias <- c(
    S1 = 0.00,
    S2 = 0.08,
    S3 = -0.06
  )

  for (attribute in attributes) {

    test_data[[attribute]] <-
      test_data[[attribute]] +
      assessor_bias[
        test_data$assessor
      ] +
      session_bias[
        test_data$session
      ] +
      stats::rnorm(
        nrow(test_data),
        mean = 0,
        sd = 0.20
      )
  }

  test_data
}


pca_test_attributes <- c(
  "sweetness",
  "bitterness",
  "umami",
  "fishy_odor",
  "fresh_odor",
  "aftertaste",
  "firmness",
  "juiciness"
)


test_that("sensory_pca returns correct object structure", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  expect_s3_class(
    result,
    "sensory_pca"
  )

  expect_s3_class(
    result$product_profiles,
    "tbl_df"
  )

  expect_s3_class(
    result$scores,
    "tbl_df"
  )

  expect_s3_class(
    result$loadings,
    "tbl_df"
  )

  expect_s3_class(
    result$variance_table,
    "tbl_df"
  )

  expect_s3_class(
    result$pca_model,
    "prcomp"
  )
})


test_that("sensory_pca aggregates observations to six product means", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  expect_equal(
    nrow(test_data),
    108
  )

  expect_equal(
    nrow(result$product_profiles),
    6
  )

  expect_equal(
    sort(result$product_profiles$product),
    paste0(
      "P",
      1:6
    )
  )

  expect_equal(
    nrow(result$scores),
    6
  )
})


test_that("sensory_pca returns one loading row per sensory attribute", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  expect_equal(
    nrow(result$loadings),
    8
  )

  expect_equal(
    sort(result$loadings$attribute),
    sort(pca_test_attributes)
  )
})


test_that("sensory_pca variance percentages sum to 100", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  expect_equal(
    sum(result$variance_explained),
    100,
    tolerance = 1e-8
  )

  expect_equal(
    tail(
      result$cumulative_variance,
      1
    ),
    100,
    tolerance = 1e-8
  )
})


test_that("sensory_pca eigenvalues agree with prcomp model", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  expect_equal(
    result$eigenvalues,
    result$pca_model$sdev^2
  )
})


test_that("sensory_pca identifies a meaningful two-dimensional structure", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes,
    scale = FALSE
  )

  expect_gt(
    result$variance_explained[1],
    50
  )

  expect_lt(
    result$variance_explained[1],
    90
  )

  expect_gt(
    result$variance_explained[2],
    15
  )

  expect_gt(
    sum(
      result$variance_explained[1:2]
    ),
    90
  )
})


test_that("sensory_pca PC1 represents the freshness deterioration dimension", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  pc1_loadings <- result$loadings[
    ,
    c(
      "attribute",
      "PC1"
    )
  ]

  fresh_loading <- pc1_loadings$PC1[
    pc1_loadings$attribute ==
      "fresh_odor"
  ]

  fishy_loading <- pc1_loadings$PC1[
    pc1_loadings$attribute ==
      "fishy_odor"
  ]

  sweetness_loading <- pc1_loadings$PC1[
    pc1_loadings$attribute ==
      "sweetness"
  ]

  bitterness_loading <- pc1_loadings$PC1[
    pc1_loadings$attribute ==
      "bitterness"
  ]

  # PCA signs are arbitrary, so test opposite directions
  # rather than requiring specific positive/negative signs.

  expect_lt(
    fresh_loading *
      fishy_loading,
    0
  )

  expect_lt(
    sweetness_loading *
      bitterness_loading,
    0
  )

  expect_gt(
    abs(fresh_loading),
    0.25
  )

  expect_gt(
    abs(fishy_loading),
    0.25
  )
})


test_that("sensory_pca PC2 represents the texture dimension", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  pc2_loadings <- result$loadings[
    ,
    c(
      "attribute",
      "PC2"
    )
  ]

  firmness_loading <- pc2_loadings$PC2[
    pc2_loadings$attribute ==
      "firmness"
  ]

  juiciness_loading <- pc2_loadings$PC2[
    pc2_loadings$attribute ==
      "juiciness"
  ]

  expect_gt(
    abs(firmness_loading),
    0.5
  )

  expect_gt(
    abs(juiciness_loading),
    0.5
  )

  expect_lt(
    firmness_loading *
      juiciness_loading,
    0
  )
})


test_that("sensory_pca keeps freshness variables weak on PC2", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  pc2 <- result$loadings$PC2

  names(pc2) <-
    result$loadings$attribute

  expect_lt(
    abs(pc2["sweetness"]),
    0.20
  )

  expect_lt(
    abs(pc2["bitterness"]),
    0.20
  )

  expect_lt(
    abs(pc2["umami"]),
    0.20
  )
})


test_that("sensory_pca supports standardized PCA", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes,
    scale = TRUE
  )

  expect_true(
    result$scale
  )

  expect_true(
    is.numeric(
      result$pca_model$scale
    )
  )

  expect_equal(
    length(
      result$pca_model$scale
    ),
    8
  )

  expect_gt(
    result$variance_explained[2],
    15
  )
})


test_that("sensory_pca stores centering and scaling settings", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = c(
      "sweetness",
      "bitterness",
      "firmness"
    ),
    center = TRUE,
    scale = FALSE
  )

  expect_true(
    result$center
  )

  expect_false(
    result$scale
  )
})


test_that("sensory_pca tolerates individual missing observations", {

  test_data <- make_pca_data()

  test_data$sweetness[1] <- NA_real_

  expect_no_error(
    result <- sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness",
        "firmness"
      )
    )
  )

  expect_false(
    anyNA(
      result$product_profiles[
        c(
          "sweetness",
          "bitterness",
          "firmness"
        )
      ]
    )
  )
})


test_that("sensory_pca rejects missing product attribute profiles", {

  test_data <- make_pca_data()

  test_data$sweetness[
    test_data$product == "P1"
  ] <- NA_real_

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness",
        "firmness"
      )
    ),
    "product mean profiles contain missing values"
  )
})


test_that("sensory_pca rejects zero variance attributes", {

  test_data <- make_pca_data()

  test_data$sweetness <- 5

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness",
        "firmness"
      )
    ),
    "zero variance"
  )
})


test_that("sensory_pca rejects missing attributes", {

  test_data <- make_pca_data()

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "does_not_exist"
      )
    ),
    "Missing sensory attribute column"
  )
})


test_that("sensory_pca rejects non-numeric attributes", {

  test_data <- make_pca_data()

  test_data$sweetness <-
    as.character(
      test_data$sweetness
    )

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      )
    ),
    "must be numeric"
  )
})


test_that("sensory_pca requires at least two attributes", {

  test_data <- make_pca_data()

  expect_error(
    sensory_pca(
      test_data,
      attributes = "sweetness"
    ),
    "at least two sensory attribute names"
  )
})


test_that("sensory_pca rejects duplicate attributes", {

  test_data <- make_pca_data()

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "sweetness"
      )
    ),
    "must not contain duplicate"
  )
})


test_that("sensory_pca requires at least three products", {

  test_data <- make_pca_data()

  test_data <- test_data[
    test_data$product %in%
      c(
        "P1",
        "P2"
      ),
    ,
    drop = FALSE
  ]

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness",
        "firmness"
      )
    ),
    "at least three products"
  )
})


test_that("sensory_pca validates center and scale arguments", {

  test_data <- make_pca_data()

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      ),
      center = "yes"
    ),
    "`center` must be TRUE or FALSE"
  )

  expect_error(
    sensory_pca(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      ),
      scale = 1
    ),
    "`scale` must be TRUE or FALSE"
  )
})


test_that("sensory_pca variance table is internally consistent", {

  test_data <- make_pca_data()

  result <- sensory_pca(
    test_data,
    attributes = pca_test_attributes
  )

  expect_true(
    all(
      c(
        "component",
        "eigenvalue",
        "variance_percent",
        "cumulative_percent"
      ) %in%
        names(
          result$variance_table
        )
    )
  )

  expect_equal(
    result$variance_table$eigenvalue,
    result$eigenvalues
  )

  expect_equal(
    result$variance_table$variance_percent,
    result$variance_explained
  )

  expect_equal(
    result$variance_table$cumulative_percent,
    result$cumulative_variance
  )
})
