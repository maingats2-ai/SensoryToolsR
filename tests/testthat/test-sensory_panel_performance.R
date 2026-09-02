make_panel_performance_data <- function() {

  test_data <- expand.grid(
    assessor = c(
      "A01", "A02", "A03", "A04", "A05"
    ),
    session = c(
      "S1", "S2", "S3"
    ),
    product = c(
      "P1", "P2", "P3", "P4"
    ),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  product_effect <- c(
    P1 = 7.5,
    P2 = 6.0,
    P3 = 4.5,
    P4 = 3.0
  )

  assessor_effect <- c(
    A01 = 0.0,
    A02 = 0.3,
    A03 = -0.2,
    A04 = 0.1,
    A05 = -0.1
  )

  session_effect <- c(
    S1 = 0.00,
    S2 = 0.10,
    S3 = -0.05
  )

  residual_pattern <- c(
    0.10, -0.08, 0.06, -0.12,
    0.05, 0.09, -0.07, 0.04,
    -0.05, 0.12, -0.09, 0.03
  )

  test_data$sweetness <-
    product_effect[
      test_data$product
    ] +
    assessor_effect[
      test_data$assessor
    ] +
    session_effect[
      test_data$session
    ] +
    rep(
      residual_pattern,
      length.out = nrow(test_data)
    )

  a05_rows <-
    test_data$assessor == "A05"

  test_data$sweetness[a05_rows] <-
    test_data$sweetness[a05_rows] +
    rep(
      c(
        -0.45,
        0.35,
        0.50,
        -0.30,
        0.40,
        -0.50
      ),
      length.out = sum(a05_rows)
    )

  test_data
}


test_that("sensory_panel_performance returns correct object structure", {

  test_data <-
    make_panel_performance_data()

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  expect_s3_class(
    result,
    "sensory_panel_performance"
  )

  expect_s3_class(
    result$assessor_table,
    "tbl_df"
  )

  expect_s3_class(
    result$panel_summary,
    "tbl_df"
  )

  expect_equal(
    nrow(result$assessor_table),
    5
  )

  expect_equal(
    result$panel_summary$n_assessors,
    5
  )

  expect_equal(
    result$panel_summary$n_products,
    4
  )

  expect_equal(
    result$panel_summary$n_sessions,
    3
  )

  expect_equal(
    result$panel_summary$n_observations,
    60
  )
})


test_that("sensory_panel_performance calculates discrimination statistics", {

  test_data <-
    make_panel_performance_data()

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    all(
      is.finite(
        result$assessor_table$
          discrimination_f
      )
    )
  )

  expect_true(
    all(
      result$assessor_table$
        discrimination_p >= 0 &
        result$assessor_table$
        discrimination_p <= 1
    )
  )

  expect_true(
    all(
      result$assessor_table$
        discrimination_p < 0.05
    )
  )
})


test_that("sensory_panel_performance calculates repeatability metrics", {

  test_data <-
    make_panel_performance_data()

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    all(
      result$assessor_table$
        repeatability_rmse >= 0
    )
  )

  expect_true(
    all(
      result$assessor_table$
        residual_mse >= 0
    )
  )

  expect_true(
    is.numeric(
      result$panel_summary$
        median_repeatability_rmse
    )
  )
})


test_that("sensory_panel_performance calculates assessor agreement", {

  test_data <-
    make_panel_performance_data()

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    all(
      result$assessor_table$
        agreement_correlation >= -1 &
        result$assessor_table$
        agreement_correlation <= 1
    )
  )

  expect_true(
    all(
      result$assessor_table$
        agreement_correlation > 0.90
    )
  )
})


test_that("sensory_panel_performance detects incomplete assessor design", {

  test_data <-
    make_panel_performance_data()

  test_data <- test_data[
    !(
      test_data$assessor == "A03" &
        test_data$product == "P4" &
        test_data$session == "S3"
    ),
    ,
    drop = FALSE
  ]

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  a03 <- result$assessor_table[
    result$assessor_table$assessor ==
      "A03",
    ,
    drop = FALSE
  ]

  expect_false(
    a03$design_complete
  )

  expect_true(
    a03$design_flag
  )

  expect_equal(
    a03$status,
    "Review"
  )

  expect_match(
    a03$review_reason,
    "Incomplete design"
  )
})


test_that("sensory_panel_performance preserves complete assessor designs", {

  test_data <-
    make_panel_performance_data()

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    all(
      result$assessor_table$
        design_complete
    )
  )

  expect_true(
    all(
      !result$assessor_table$
        design_flag
    )
  )
})


test_that("sensory_panel_performance returns scale-use statistics", {

  test_data <-
    make_panel_performance_data()

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    all(
      result$assessor_table$
        max_score >=
        result$assessor_table$
        min_score
    )
  )

  expect_equal(
    result$assessor_table$
      score_range,
    result$assessor_table$
      max_score -
      result$assessor_table$
      min_score
  )
})


test_that("sensory_panel_performance rejects non-numeric attributes", {

  test_data <-
    make_panel_performance_data()

  test_data$sweetness <-
    as.character(
      test_data$sweetness
    )

  expect_error(
    sensory_panel_performance(
      test_data,
      attribute = "sweetness"
    ),
    "Sensory attribute must be numeric"
  )
})


test_that("sensory_panel_performance requires replicated sessions", {

  test_data <-
    make_panel_performance_data()

  test_data <- test_data[
    test_data$session == "S1",
    ,
    drop = FALSE
  ]

  expect_error(
    sensory_panel_performance(
      test_data,
      attribute = "sweetness"
    ),
    "At least two sessions or replicates"
  )
})


test_that("sensory_panel_performance reports missing columns", {

  test_data <- data.frame(
    assessor = c("A01", "A02"),
    product = c("P1", "P2"),
    sweetness = c(7, 5)
  )

  expect_error(
    sensory_panel_performance(
      test_data,
      attribute = "sweetness"
    ),
    "Missing required column"
  )
})


test_that("sensory_panel_performance validates screening settings", {

  test_data <-
    make_panel_performance_data()

  expect_error(
    sensory_panel_performance(
      test_data,
      attribute = "sweetness",
      alpha = 1.2
    ),
    "`alpha` must be a single number between 0 and 1"
  )

  expect_error(
    sensory_panel_performance(
      test_data,
      attribute = "sweetness",
      agreement_threshold = 2
    ),
    "`agreement_threshold` must be between -1 and 1"
  )

  expect_error(
    sensory_panel_performance(
      test_data,
      attribute = "sweetness",
      repeatability_multiplier = 0
    ),
    "`repeatability_multiplier` must be a finite number greater than 0"
  )
})

test_that("sensory_panel_performance reports assessor session effects", {

  test_data <-
    make_panel_performance_data()

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  a01_data <- test_data[
    test_data$assessor == "A01",
    ,
    drop = FALSE
  ]

  a01_data$product <- factor(
    a01_data$product
  )

  a01_data$session <- factor(
    a01_data$session
  )

  fit <- stats::lm(
    sweetness ~ product + session,
    data = a01_data
  )

  fit_anova <- stats::anova(
    fit
  )

  expected_f <- fit_anova[
    "session",
    "F value"
  ]

  expected_p <- fit_anova[
    "session",
    "Pr(>F)"
  ]

  a01_result <- result$assessor_table[
    result$assessor_table$assessor == "A01",
    ,
    drop = FALSE
  ]

  expect_true(
    all(
      c(
        "session_f",
        "session_p"
      ) %in%
        names(result$assessor_table)
    )
  )

  expect_equal(
    a01_result$session_f,
    expected_f,
    tolerance = 1e-12
  )

  expect_equal(
    a01_result$session_p,
    expected_p,
    tolerance = 1e-12
  )
})

test_that("sensory_panel_performance reports assessor mean level bias", {

  test_data <-
    make_panel_performance_data()

  test_data$sweetness[
    test_data$assessor == "A01"
  ] <-
    test_data$sweetness[
      test_data$assessor == "A01"
    ] + 2

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  a01_data <- test_data[
    test_data$assessor == "A01",
    ,
    drop = FALSE
  ]

  other_data <- test_data[
    test_data$assessor != "A01",
    ,
    drop = FALSE
  ]

  a01_product_means <- stats::aggregate(
    sweetness ~ product,
    data = a01_data,
    FUN = mean
  )

  other_product_means <- stats::aggregate(
    sweetness ~ product,
    data = other_data,
    FUN = mean
  )

  names(a01_product_means)[2] <-
    "assessor_mean"

  names(other_product_means)[2] <-
    "other_panel_mean"

  comparison <- merge(
    a01_product_means,
    other_product_means,
    by = "product"
  )

  expected_bias <- mean(
    comparison$assessor_mean -
      comparison$other_panel_mean
  )

  a01_result <- result$assessor_table[
    result$assessor_table$assessor == "A01",
    ,
    drop = FALSE
  ]

  expect_true(
    "mean_level_bias" %in%
      names(result$assessor_table)
  )

  expect_equal(
    a01_result$mean_level_bias,
    expected_bias,
    tolerance = 1e-12
  )

  expect_gt(
    a01_result$mean_level_bias,
    1.5
  )
})


test_that("sensory_panel_performance flags duplicate design cells", {

  test_data <-
    make_panel_performance_data()

  duplicate_row <- test_data[
    test_data$assessor == "A01" &
      test_data$product == "P1" &
      test_data$session == "S1",
    ,
    drop = FALSE
  ]

  test_data <- rbind(
    test_data,
    duplicate_row
  )

  result <- sensory_panel_performance(
    test_data,
    attribute = "sweetness"
  )

  a01_result <- result$assessor_table[
    result$assessor_table$assessor == "A01",
    ,
    drop = FALSE
  ]

  expect_equal(
    a01_result$n_observations,
    a01_result$expected_design_records + 1
  )

  expect_false(
    a01_result$design_complete
  )

  expect_true(
    a01_result$design_flag
  )

  expect_equal(
    a01_result$status,
    "Review"
  )

  expect_match(
    a01_result$review_reason,
    "design",
    ignore.case = TRUE
  )
})

test_that("sensory_panel_performance rejects infinite repeatability multiplier", {

  expect_error(
    sensory_panel_performance(
      make_panel_performance_data(),
      attribute = "sweetness",
      repeatability_multiplier = Inf
    ),
    "repeatability_multiplier"
  )
})
