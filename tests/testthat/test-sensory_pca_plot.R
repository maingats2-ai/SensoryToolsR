make_pca_plot_data <- function() {

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


pca_plot_attributes <- c(
  "sweetness",
  "bitterness",
  "umami",
  "fishy_odor",
  "fresh_odor",
  "aftertaste",
  "firmness",
  "juiciness"
)


make_pca_plot_result <- function() {

  test_data <-
    make_pca_plot_data()

  sensory_pca(
    test_data,
    attributes =
      pca_plot_attributes
  )
}


test_that("sensory_pca_plot returns a ggplot object", {

  result <-
    make_pca_plot_result()

  p <- sensory_pca_plot(
    result
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


test_that("sensory_pca_plot supports all three plot types", {

  result <-
    make_pca_plot_result()

  p1 <- sensory_pca_plot(
    result,
    type = "biplot"
  )

  p2 <- sensory_pca_plot(
    result,
    type = "scores"
  )

  p3 <- sensory_pca_plot(
    result,
    type = "loadings"
  )

  expect_s3_class(
    p1,
    "ggplot"
  )

  expect_s3_class(
    p2,
    "ggplot"
  )

  expect_s3_class(
    p3,
    "ggplot"
  )
})


test_that("sensory_pca_plot uses meaningful PC1 and PC2 axis labels", {

  result <-
    make_pca_plot_result()

  p <- sensory_pca_plot(
    result,
    pc_x = 1,
    pc_y = 2
  )

  expect_match(
    p$labels$x,
    "PC1"
  )

  expect_match(
    p$labels$y,
    "PC2"
  )

  expect_match(
    p$labels$x,
    "%"
  )

  expect_match(
    p$labels$y,
    "%"
  )

  expect_match(
    p$labels$x,
    "71"
  )

  expect_match(
    p$labels$y,
    "27"
  )
})


test_that("sensory_pca_plot works with a genuinely two-dimensional PCA", {

  result <-
    make_pca_plot_result()

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

  p <- sensory_pca_plot(
    result,
    type = "biplot",
    pc_x = 1,
    pc_y = 2
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


test_that("sensory_pca_plot supports different PC combinations", {

  result <-
    make_pca_plot_result()

  p <- sensory_pca_plot(
    result,
    pc_x = 1,
    pc_y = 3
  )

  expect_s3_class(
    p,
    "ggplot"
  )

  expect_match(
    p$labels$x,
    "PC1"
  )

  expect_match(
    p$labels$y,
    "PC3"
  )
})


test_that("sensory_pca_plot rejects identical PCA axes", {

  result <-
    make_pca_plot_result()

  expect_error(
    sensory_pca_plot(
      result,
      pc_x = 1,
      pc_y = 1
    ),
    "must refer to different principal components"
  )
})


test_that("sensory_pca_plot rejects unavailable components", {

  result <-
    make_pca_plot_result()

  expect_error(
    sensory_pca_plot(
      result,
      pc_x = 1,
      pc_y = 99
    ),
    "available principal component"
  )
})


test_that("sensory_pca_plot validates input object", {

  expect_error(
    sensory_pca_plot(
      data.frame(
        x = 1:3
      )
    ),
    "`x` must be an object returned by sensory_pca"
  )
})


test_that("sensory_pca_plot validates vector scale", {

  result <-
    make_pca_plot_result()

  expect_error(
    sensory_pca_plot(
      result,
      vector_scale = -1
    ),
    "`vector_scale` must be NULL or a positive number"
  )
})


test_that("sensory_pca_plot validates logical arguments", {

  result <-
    make_pca_plot_result()

  expect_error(
    sensory_pca_plot(
      result,
      label_products = "yes"
    ),
    "must be TRUE or FALSE"
  )

  expect_error(
    sensory_pca_plot(
      result,
      label_attributes = 1
    ),
    "must be TRUE or FALSE"
  )

  expect_error(
    sensory_pca_plot(
      result,
      show_origin = NA
    ),
    "must be TRUE or FALSE"
  )
})


test_that("sensory_pca_plot supports manual vector scaling", {

  result <-
    make_pca_plot_result()

  p <- sensory_pca_plot(
    result,
    type = "biplot",
    vector_scale = 3
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


test_that("sensory_pca_plot works with standardized PCA", {

  test_data <-
    make_pca_plot_data()

  result <- sensory_pca(
    test_data,
    attributes =
      pca_plot_attributes,
    scale = TRUE
  )

  p <- sensory_pca_plot(
    result
  )

  expect_s3_class(
    p,
    "ggplot"
  )

  expect_true(
    result$scale
  )

  expect_gt(
    result$variance_explained[2],
    15
  )
})


test_that("score plot retains six products", {

  result <-
    make_pca_plot_result()

  expect_equal(
    nrow(result$scores),
    6
  )

  p <- sensory_pca_plot(
    result,
    type = "scores"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


test_that("loading plot retains eight sensory attributes", {

  result <-
    make_pca_plot_result()

  expect_equal(
    nrow(result$loadings),
    8
  )

  p <- sensory_pca_plot(
    result,
    type = "loadings"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


test_that("PCA plot data preserve the intended texture relationship", {

  result <-
    make_pca_plot_result()

  firmness_loading <-
    result$loadings$PC2[
      result$loadings$attribute ==
        "firmness"
    ]

  juiciness_loading <-
    result$loadings$PC2[
      result$loadings$attribute ==
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
