test_that("sensory_summary calculates correct descriptive statistics", {

  test_data <- data.frame(
    assessor = c("A01", "A01", "A02", "A02"),
    session = c(1, 1, 1, 1),
    product = c("P1", "P2", "P1", "P2"),
    sweetness = c(6, 5, 7, 6),
    bitterness = c(3, 5, 2, 4)
  )

  result <- sensory_summary(
    test_data,
    product = "product",
    attributes = c(
      "sweetness",
      "bitterness"
    )
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_equal(
    nrow(result),
    4
  )

  expect_equal(
    sort(unique(result$attribute)),
    c("bitterness", "sweetness")
  )

  p1_sweetness <- result[
    result$product == "P1" &
      result$attribute == "sweetness",
    ,
    drop = FALSE
  ]

  expect_equal(
    p1_sweetness$n,
    2
  )

  expect_equal(
    p1_sweetness$mean,
    6.5
  )

  expect_equal(
    p1_sweetness$sd,
    stats::sd(c(6, 7))
  )

  expect_equal(
    p1_sweetness$se,
    0.5
  )

  expect_equal(
    p1_sweetness$median,
    6.5
  )

  expect_equal(
    p1_sweetness$min,
    6
  )

  expect_equal(
    p1_sweetness$max,
    7
  )
})


test_that("sensory_summary handles missing sensory scores correctly", {

  test_data <- data.frame(
    product = c("P1", "P1", "P1"),
    sweetness = c(6, NA, 8)
  )

  result <- sensory_summary(
    test_data,
    attributes = "sweetness"
  )

  expect_equal(
    result$n,
    2
  )

  expect_equal(
    result$mean,
    7
  )

  expect_equal(
    result$median,
    7
  )
})


test_that("sensory_summary calculates CV correctly", {

  test_data <- data.frame(
    product = c("P1", "P1"),
    sweetness = c(6, 8)
  )

  result <- sensory_summary(
    test_data,
    attributes = "sweetness"
  )

  expected_cv <- stats::sd(c(6, 8)) /
    mean(c(6, 8)) * 100

  expect_equal(
    result$cv_percent,
    expected_cv
  )
})


test_that("sensory_summary returns NA CV when mean equals zero", {

  test_data <- data.frame(
    product = c("P1", "P1"),
    sweetness = c(-1, 1)
  )

  result <- sensory_summary(
    test_data,
    attributes = "sweetness"
  )

  expect_true(
    is.na(result$cv_percent)
  )
})


test_that("sensory_summary returns NA SE and CI for one observation", {

  test_data <- data.frame(
    product = "P1",
    sweetness = 6
  )

  result <- sensory_summary(
    test_data,
    attributes = "sweetness"
  )

  expect_equal(
    result$n,
    1
  )

  expect_true(
    is.na(result$sd)
  )

  expect_true(
    is.na(result$se)
  )

  expect_true(
    is.na(result$ci_lower)
  )

  expect_true(
    is.na(result$ci_upper)
  )
})


test_that("sensory_summary rejects a missing product column", {

  test_data <- data.frame(
    sweetness = c(6, 7)
  )

  expect_error(
    sensory_summary(test_data),
    "Product column not found"
  )
})


test_that("sensory_summary rejects unknown attributes", {

  test_data <- data.frame(
    product = c("P1", "P2"),
    sweetness = c(6, 7)
  )

  expect_error(
    sensory_summary(
      test_data,
      attributes = "bitterness"
    ),
    "Unknown attribute column"
  )
})


test_that("sensory_summary rejects non-numeric sensory attributes", {

  test_data <- data.frame(
    product = c("P1", "P2"),
    sweetness = c("high", "low")
  )

  expect_error(
    sensory_summary(
      test_data,
      attributes = "sweetness"
    ),
    "Sensory attributes must be numeric"
  )
})


test_that("sensory_summary validates confidence level", {

  test_data <- data.frame(
    product = c("P1", "P2"),
    sweetness = c(6, 7)
  )

  expect_error(
    sensory_summary(
      test_data,
      attributes = "sweetness",
      conf_level = 1.5
    ),
    "`conf_level` must be a single number between 0 and 1"
  )

  expect_error(
    sensory_summary(
      test_data,
      attributes = "sweetness",
      conf_level = 0
    ),
    "`conf_level` must be a single number between 0 and 1"
  )
})
