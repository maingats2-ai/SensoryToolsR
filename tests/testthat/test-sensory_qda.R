make_qda_data <- function() {

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

  profiles <- data.frame(
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
    profiles,
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


qda_test_attributes <- c(
  "sweetness",
  "bitterness",
  "umami",
  "fishy_odor",
  "fresh_odor",
  "aftertaste",
  "firmness",
  "juiciness"
)


test_that("sensory_qda returns the correct object structure", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_s3_class(
    result,
    "sensory_qda"
  )

  expect_s3_class(
    result$overview,
    "tbl_df"
  )

  expect_s3_class(
    result$summary,
    "tbl_df"
  )

  expect_s3_class(
    result$panel,
    "sensory_panel_multi"
  )

  expect_s3_class(
    result$pca,
    "sensory_pca"
  )

  expect_s3_class(
    result$pca_diagnostics,
    "sensory_pca_diagnostics"
  )
})


test_that("sensory_qda retains the experimental design", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_equal(
    result$overview$n_observations,
    108
  )

  expect_equal(
    result$overview$n_products,
    6
  )

  expect_equal(
    result$overview$n_assessors,
    6
  )

  expect_equal(
    result$overview$n_sessions,
    3
  )

  expect_equal(
    result$overview$n_attributes,
    8
  )
})


test_that("sensory_qda stores requested sensory attributes", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_equal(
    result$attributes,
    qda_test_attributes
  )
})


test_that("sensory_qda descriptive summary contains every attribute", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_equal(
    sort(
      unique(
        result$summary$attribute
      )
    ),
    sort(
      qda_test_attributes
    )
  )
})


test_that("sensory_qda panel analysis contains every attribute", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_equal(
    result$panel$attributes,
    qda_test_attributes
  )

  expect_equal(
    nrow(
      result$panel$
        attribute_summary
    ),
    8
  )
})


test_that("sensory_qda detects strong product effects", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_true(
    all(
      result$panel$
        attribute_summary$
        product_significant
    )
  )

  expect_equal(
    result$overview$
      n_significant_product_attributes,
    8
  )
})


test_that("sensory_qda retains meaningful two-dimensional PCA", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_gt(
    result$overview$
      pca_pc1_percent,
    50
  )

  expect_lt(
    result$overview$
      pca_pc1_percent,
    90
  )

  expect_gt(
    result$overview$
      pca_pc2_percent,
    15
  )

  expect_gt(
    result$overview$
      pca_pc1_pc2_percent,
    90
  )
})


test_that("sensory_qda PCA contains six products and eight attributes", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_equal(
    nrow(
      result$pca$scores
    ),
    6
  )

  expect_equal(
    nrow(
      result$pca$loadings
    ),
    8
  )
})


test_that("sensory_qda PCA diagnostics identify texture on PC2", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes,
    pca_top_n = 3
  )

  pc2_top <-
    result$
    pca_diagnostics$
    top_attributes[
      result$
        pca_diagnostics$
        top_attributes$
        component ==
        "PC2",
      ,
      drop = FALSE
    ]

  expect_true(
    "firmness" %in%
      pc2_top$attribute
  )

  expect_true(
    "juiciness" %in%
      pc2_top$attribute
  )
})


test_that("sensory_qda stores design variable names", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes
  )

  expect_equal(
    result$design$product,
    "product"
  )

  expect_equal(
    result$design$assessor,
    "assessor"
  )

  expect_equal(
    result$design$session,
    "session"
  )
})


test_that("sensory_qda stores analysis settings", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes,
    alpha = 0.01,
    agreement_threshold = 0.75,
    repeatability_multiplier = 2,
    pca_scale = TRUE,
    pca_top_n = 3
  )

  expect_equal(
    result$settings$alpha,
    0.01
  )

  expect_equal(
    result$settings$
      agreement_threshold,
    0.75
  )

  expect_equal(
    result$settings$
      repeatability_multiplier,
    2
  )

  expect_true(
    result$settings$pca_scale
  )

  expect_equal(
    result$settings$pca_top_n,
    3
  )
})


test_that("sensory_qda supports standardized PCA", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes,
    pca_scale = TRUE
  )

  expect_true(
    result$pca$scale
  )

  expect_gt(
    result$pca$
      variance_explained[2],
    15
  )
})


test_that("sensory_qda accepts selected PCA components", {

  test_data <-
    make_qda_data()

  result <- sensory_qda(
    test_data,
    attributes =
      qda_test_attributes,
    pca_components = c(
      1,
      2,
      3
    )
  )

  expect_equal(
    result$
      pca_diagnostics$
      components,
    c(
      1,
      2,
      3
    )
  )
})


test_that("sensory_qda rejects missing sensory attributes", {

  test_data <-
    make_qda_data()

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "does_not_exist"
      )
    ),
    "Missing sensory attribute column"
  )
})


test_that("sensory_qda rejects non-numeric sensory attributes", {

  test_data <-
    make_qda_data()

  test_data$sweetness <-
    as.character(
      test_data$sweetness
    )

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      )
    ),
    "must be numeric"
  )
})


test_that("sensory_qda requires at least two sensory attributes", {

  test_data <-
    make_qda_data()

  expect_error(
    sensory_qda(
      test_data,
      attributes =
        "sweetness"
    ),
    "at least two sensory attribute names"
  )
})


test_that("sensory_qda rejects duplicate sensory attributes", {

  test_data <-
    make_qda_data()

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "sweetness"
      )
    ),
    "must not contain duplicate"
  )
})


test_that("sensory_qda requires at least three products", {

  test_data <-
    make_qda_data()

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
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      )
    ),
    "at least three products"
  )
})


test_that("sensory_qda requires replicated sessions", {

  test_data <-
    make_qda_data()

  test_data <- test_data[
    test_data$session ==
      "S1",
    ,
    drop = FALSE
  ]

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      )
    ),
    "at least two sessions or replicates"
  )
})


test_that("sensory_qda validates alpha", {

  test_data <-
    make_qda_data()

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      ),
      alpha = 2
    ),
    "`alpha` must be a single number between 0 and 1"
  )
})


test_that("sensory_qda validates PCA settings", {

  test_data <-
    make_qda_data()

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      ),
      pca_scale = "yes"
    ),
    "`pca_scale` must be TRUE or FALSE"
  )

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      ),
      pca_top_n = 0
    ),
    "`pca_top_n` must be a positive integer"
  )

  expect_error(
    sensory_qda(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      ),
      pca_components =
        c(
          1,
          1
        )
    ),
    "must not contain duplicate"
  )
})


test_that("sensory_qda rejects unavailable PCA components", {

  test_data <-
    make_qda_data()

  expect_error(
    sensory_qda(
      test_data,
      attributes =
        qda_test_attributes,
      pca_components = 99
    ),
    "unavailable principal component"
  )
})
