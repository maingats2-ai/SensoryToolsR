test_that("sensory_validate recognizes a complete sensory design", {

  test_data <- data.frame(
    assessor = c("A01", "A01", "A02", "A02"),
    session = c(1, 1, 1, 1),
    product = c("P1", "P2", "P1", "P2"),
    sweetness = c(6, 5, 7, 6),
    bitterness = c(3, 5, 2, 4)
  )

  result <- sensory_validate(test_data)

  expect_s3_class(
    result,
    "sensory_validation"
  )

  expect_equal(
    result$n_assessors,
    2
  )

  expect_equal(
    result$n_products,
    2
  )

  expect_equal(
    result$n_sessions,
    1
  )

  expect_equal(
    result$n_expected_design,
    4
  )

  expect_equal(
    result$n_observed_design,
    4
  )

  expect_equal(
    result$n_missing_design,
    0
  )

  expect_true(
    result$panel_complete
  )

  expect_equal(
    result$attributes,
    c("sweetness", "bitterness")
  )
})


test_that("sensory_validate detects an incomplete sensory design", {

  test_data <- data.frame(
    assessor = c("A01", "A01", "A02"),
    session = c(1, 1, 1),
    product = c("P1", "P2", "P1"),
    sweetness = c(6, 5, 7),
    bitterness = c(3, 5, 2)
  )

  result <- sensory_validate(test_data)

  expect_false(
    result$panel_complete
  )

  expect_equal(
    result$n_expected_design,
    4
  )

  expect_equal(
    result$n_observed_design,
    3
  )

  expect_equal(
    result$n_missing_design,
    1
  )

  expect_equal(
    nrow(result$missing_design),
    1
  )

  expect_equal(
    result$missing_design$assessor,
    "A02"
  )

  expect_equal(
    result$missing_design$product,
    "P2"
  )

  expect_equal(
    result$missing_design$session,
    1
  )
})


test_that("sensory_validate detects missing values", {

  test_data <- data.frame(
    assessor = c("A01", "A01", "A02", "A02"),
    session = c(1, 1, 1, 1),
    product = c("P1", "P2", "P1", "P2"),
    sweetness = c(6, NA, 7, 6),
    bitterness = c(3, 5, 2, 4)
  )

  result <- sensory_validate(test_data)

  expect_equal(
    result$total_missing,
    1
  )

  expect_equal(
    result$missing_by_column[["sweetness"]],
    1
  )
})


test_that("sensory_validate detects duplicate design records", {

  test_data <- data.frame(
    assessor = c("A01", "A01", "A01"),
    session = c(1, 1, 1),
    product = c("P1", "P1", "P2"),
    sweetness = c(6, 6, 5),
    bitterness = c(3, 3, 5)
  )

  result <- sensory_validate(test_data)

  expect_gt(
    result$n_duplicate_rows,
    0
  )
})


test_that("sensory_validate reports missing required columns", {

  test_data <- data.frame(
    assessor = c("A01", "A02"),
    sweetness = c(6, 7)
  )

  expect_error(
    sensory_validate(test_data),
    "Missing required column"
  )
})
