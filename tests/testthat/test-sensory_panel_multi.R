make_multi_panel_data <- function() {

  set.seed(123)

  test_data <- expand.grid(
    assessor = paste0(
      "A0",
      1:5
    ),
    session = paste0(
      "S",
      1:3
    ),
    product = paste0(
      "P",
      1:4
    ),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  product_profiles <- data.frame(
    product = c(
      "P1", "P2", "P3", "P4"
    ),
    sweetness = c(
      7.5, 6.0, 4.5, 3.0
    ),
    bitterness = c(
      2.0, 3.5, 5.0, 6.5
    ),
    firmness = c(
      7.0, 6.0, 4.5, 3.5
    )
  )

  test_data <- merge(
    test_data,
    product_profiles,
    by = "product",
    sort = FALSE
  )

  assessor_bias <- c(
    A01 = 0.00,
    A02 = 0.20,
    A03 = -0.15,
    A04 = 0.10,
    A05 = -0.10
  )

  session_bias <- c(
    S1 = 0.00,
    S2 = 0.10,
    S3 = -0.05
  )

  attributes <- c(
    "sweetness",
    "bitterness",
    "firmness"
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
        sd = 0.18
      )
  }

  test_data
}


test_that("sensory_panel_multi returns correct object structure", {

  test_data <- make_multi_panel_data()

  result <- sensory_panel_multi(
    test_data,
    attributes = c(
      "sweetness",
      "bitterness",
      "firmness"
    )
  )

  expect_s3_class(
    result,
    "sensory_panel_multi"
  )

  expect_s3_class(
    result$attribute_summary,
    "tbl_df"
  )

  expect_equal(
    nrow(result$attribute_summary),
    3
  )

  expect_equal(
    result$attributes,
    c(
      "sweetness",
      "bitterness",
      "firmness"
    )
  )
})


test_that("sensory_panel_multi creates results for every attribute", {

  test_data <- make_multi_panel_data()

  attributes <- c(
    "sweetness",
    "bitterness",
    "firmness"
  )

  result <- sensory_panel_multi(
    test_data,
    attributes = attributes
  )

  expect_equal(
    names(result$anova_results),
    attributes
  )

  expect_equal(
    names(result$performance_results),
    attributes
  )

  expect_equal(
    names(result$anova_tables),
    attributes
  )

  expect_equal(
    names(result$assessor_tables),
    attributes
  )
})


test_that("sensory_panel_multi produces expected summary columns", {

  test_data <- make_multi_panel_data()

  result <- sensory_panel_multi(
    test_data,
    attributes = c(
      "sweetness",
      "bitterness",
      "firmness"
    )
  )

  expected_columns <- c(
    "attribute",
    "product_p_value",
    "assessor_p_value",
    "session_p_value",
    "interaction_p_value",
    "product_significant",
    "interaction_significant",
    "median_repeatability_rmse",
    "median_agreement_correlation",
    "n_assessors_review"
  )

  expect_true(
    all(
      expected_columns %in%
        names(result$attribute_summary)
    )
  )
})


test_that("sensory_panel_multi detects strong product effects", {

  test_data <- make_multi_panel_data()

  result <- sensory_panel_multi(
    test_data,
    attributes = c(
      "sweetness",
      "bitterness",
      "firmness"
    )
  )

  expect_true(
    all(
      result$attribute_summary$
        product_significant
    )
  )

  expect_true(
    all(
      result$attribute_summary$
        product_p_value < 0.05
    )
  )
})


test_that("sensory_panel_multi stores assessor tables", {

  test_data <- make_multi_panel_data()

  result <- sensory_panel_multi(
    test_data,
    attributes = c(
      "sweetness",
      "bitterness",
      "firmness"
    )
  )

  expect_equal(
    nrow(
      result$assessor_tables$sweetness
    ),
    5
  )

  expect_true(
    "repeatability_rmse" %in%
      names(
        result$assessor_tables$sweetness
      )
  )

  expect_true(
    "agreement_correlation" %in%
      names(
        result$assessor_tables$sweetness
      )
  )
})


test_that("sensory_panel_multi rejects missing attributes", {

  test_data <- make_multi_panel_data()

  expect_error(
    sensory_panel_multi(
      test_data,
      attributes = c(
        "sweetness",
        "does_not_exist"
      )
    ),
    "Missing sensory attribute column"
  )
})


test_that("sensory_panel_multi rejects non-numeric attributes", {

  test_data <- make_multi_panel_data()

  test_data$sweetness <-
    as.character(
      test_data$sweetness
    )

  expect_error(
    sensory_panel_multi(
      test_data,
      attributes = c(
        "sweetness",
        "bitterness"
      )
    ),
    "Sensory attribute.*must be numeric"
  )
})


test_that("sensory_panel_multi rejects duplicate attribute names", {

  test_data <- make_multi_panel_data()

  expect_error(
    sensory_panel_multi(
      test_data,
      attributes = c(
        "sweetness",
        "sweetness"
      )
    ),
    "must not contain duplicate"
  )
})


test_that("sensory_panel_multi validates alpha", {

  test_data <- make_multi_panel_data()

  expect_error(
    sensory_panel_multi(
      test_data,
      attributes = "sweetness",
      alpha = 2
    ),
    "`alpha` must be a single number between 0 and 1"
  )
})

test_that("sensory_panel_multi validates alpha before analysis", {

  test_data <- make_multi_panel_data()

  test_data <-
    test_data[
      test_data$product ==
        unique(test_data$product)[1],
      ,
      drop = FALSE
    ]

  expect_error(
    sensory_panel_multi(
      test_data,
      attributes = "sweetness",
      alpha = NaN
    ),
    "`alpha` must be a single number between 0 and 1"
  )
})

test_that("sensory_panel_multi validates agreement threshold before analysis", {

  test_data <- make_multi_panel_data()

  test_data <-
    test_data[
      test_data$product ==
        unique(test_data$product)[1],
      ,
      drop = FALSE
    ]

  expect_error(
    sensory_panel_multi(
      test_data,
      attributes = "sweetness",
      agreement_threshold = NaN
    ),
    "`agreement_threshold` must be between -1 and 1"
  )
})

test_that("sensory_panel_multi validates repeatability multiplier before analysis", {

  test_data <- make_multi_panel_data()

  test_data <-
    test_data[
      test_data$product ==
        unique(test_data$product)[1],
      ,
      drop = FALSE
    ]

  expect_error(
    sensory_panel_multi(
      test_data,
      attributes = "sweetness",
      repeatability_multiplier = Inf
    ),
    "`repeatability_multiplier` must be a finite number greater than 0"
  )
})

test_that("sensory_panel_multi propagates undefined agreement review", {

  test_data <- qda_example

  test_data$sweetness[
    test_data$assessor != "A01" &
      test_data$session == "S1"
  ] <- 4.5

  test_data$sweetness[
    test_data$assessor != "A01" &
      test_data$session == "S2"
  ] <- 5.0

  test_data$sweetness[
    test_data$assessor != "A01" &
      test_data$session == "S3"
  ] <- 5.5

  result <- suppressWarnings(
    sensory_panel_multi(
      test_data,
      attributes = "sweetness"
    )
  )

  a01_result <-
    result$assessor_tables$sweetness[
      result$assessor_tables$sweetness$assessor == "A01",
      ,
      drop = FALSE
    ]

  expect_true(
    is.na(a01_result$agreement_correlation)
  )

  expect_true(
    a01_result$agreement_flag
  )

  expect_equal(
    a01_result$review_reason,
    "Agreement unavailable"
  )

  expect_equal(
    a01_result$status,
    "Review"
  )
})
