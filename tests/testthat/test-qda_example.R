test_that("qda_example is available as package data", {

  expect_true(
    exists("qda_example")
  )

  expect_s3_class(
    qda_example,
    "tbl_df"
  )
})


test_that("qda_example has the expected dimensions", {

  expect_equal(
    nrow(qda_example),
    108
  )

  expect_equal(
    ncol(qda_example),
    11
  )
})


test_that("qda_example contains the expected columns", {

  expected_columns <- c(
    "product",
    "assessor",
    "session",
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  expect_equal(
    names(qda_example),
    expected_columns
  )
})


test_that("qda_example has the intended experimental design", {

  expect_equal(
    length(unique(qda_example$product)),
    6
  )

  expect_equal(
    length(unique(qda_example$assessor)),
    6
  )

  expect_equal(
    length(unique(qda_example$session)),
    3
  )
})


test_that("qda_example contains complete assessor-product-session combinations", {

  design_rows <- unique(
    qda_example[
      c(
        "assessor",
        "product",
        "session"
      )
    ]
  )

  expect_equal(
    nrow(design_rows),
    108
  )
})


test_that("qda_example contains the expected design labels", {

  expect_setequal(
    unique(qda_example$product),
    paste0("P", 1:6)
  )

  expect_setequal(
    unique(qda_example$assessor),
    paste0("A0", 1:6)
  )

  expect_setequal(
    unique(qda_example$session),
    paste0("S", 1:3)
  )
})


test_that("qda_example sensory attributes are numeric", {

  sensory_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  numeric_status <- vapply(
    qda_example[sensory_attributes],
    is.numeric,
    logical(1)
  )

  expect_true(
    all(numeric_status)
  )
})


test_that("qda_example design variables have appropriate types", {

  expect_true(
    is.character(qda_example$product)
  )

  expect_true(
    is.character(qda_example$assessor)
  )

  expect_true(
    is.character(qda_example$session)
  )
})


test_that("qda_example contains no missing values", {

  expect_equal(
    sum(is.na(qda_example)),
    0
  )
})


test_that("qda_example has one observation per design combination", {

  design_key <- paste(
    qda_example$assessor,
    qda_example$product,
    qda_example$session,
    sep = "_"
  )

  expect_false(
    anyDuplicated(design_key) > 0
  )

  expect_equal(
    length(unique(design_key)),
    108
  )
})


test_that("qda_example sensory scores are within a plausible range", {

  sensory_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  sensory_matrix <- as.matrix(
    qda_example[sensory_attributes]
  )

  expect_true(
    all(is.finite(sensory_matrix))
  )

  expect_gt(
    min(sensory_matrix),
    0
  )

  expect_lt(
    max(sensory_matrix),
    10
  )
})


test_that("qda_example reproduces the intended PCA structure", {

  sensory_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  result <- sensory_pca(
    qda_example,
    attributes = sensory_attributes,
    scale = FALSE
  )

  expect_s3_class(
    result,
    "sensory_pca"
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
    sum(result$variance_explained[1:2]),
    90
  )
})


test_that("qda_example retains the intended PC1 taste and odor dimension", {

  sensory_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  result <- sensory_pca(
    qda_example,
    attributes = sensory_attributes,
    scale = FALSE
  )

  sweetness_loading <- result$loadings$PC1[
    result$loadings$attribute == "sweetness"
  ]

  bitterness_loading <- result$loadings$PC1[
    result$loadings$attribute == "bitterness"
  ]

  fishy_loading <- result$loadings$PC1[
    result$loadings$attribute == "fishy_odor"
  ]

  fresh_loading <- result$loadings$PC1[
    result$loadings$attribute == "fresh_odor"
  ]

  expect_gt(
    abs(sweetness_loading),
    0.30
  )

  expect_gt(
    abs(bitterness_loading),
    0.30
  )

  expect_gt(
    abs(fishy_loading),
    0.30
  )

  expect_gt(
    abs(fresh_loading),
    0.30
  )

  # Sweetness and fresh odor should point in the same
  # direction on PC1.
  expect_gt(
    sweetness_loading * fresh_loading,
    0
  )

  # Bitterness and fishy odor should point in the same
  # direction on PC1.
  expect_gt(
    bitterness_loading * fishy_loading,
    0
  )

  # Fresh and deterioration-related attributes should
  # oppose one another on PC1.
  expect_lt(
    sweetness_loading * bitterness_loading,
    0
  )

  expect_lt(
    fresh_loading * fishy_loading,
    0
  )
})


test_that("qda_example retains the intended PC2 texture contrast", {

  sensory_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  result <- sensory_pca(
    qda_example,
    attributes = sensory_attributes,
    scale = FALSE
  )

  firmness_loading <- result$loadings$PC2[
    result$loadings$attribute == "firmness"
  ]

  juiciness_loading <- result$loadings$PC2[
    result$loadings$attribute == "juiciness"
  ]

  expect_gt(
    abs(firmness_loading),
    0.5
  )

  expect_gt(
    abs(juiciness_loading),
    0.5
  )

  # Firmness and juiciness should point in opposite
  # directions on PC2.
  expect_lt(
    firmness_loading * juiciness_loading,
    0
  )
})


test_that("qda_example works with sensory_validate", {

  result <- sensory_validate(
    qda_example
  )

  expect_true(
    is.list(result)
  )
})


test_that("qda_example works with sensory_summary", {

  sensory_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  result <- sensory_summary(
    qda_example,
    attributes = sensory_attributes
  )

  expect_true(
    is.list(result)
  )
})


test_that("qda_example works with integrated sensory_qda", {

  sensory_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  result <- sensory_qda(
    qda_example,
    attributes = sensory_attributes
  )

  expect_s3_class(
    result,
    "sensory_qda"
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
