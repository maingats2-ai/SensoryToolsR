test_that("sensory_clean trims whitespace in character variables", {

  test_data <- data.frame(
    assessor = c(" A01 ", "A02 "),
    product = c(" P1", "P2 "),
    sweetness = c(6, 7)
  )

  result <- sensory_clean(test_data)

  expect_equal(
    result$assessor,
    c("A01", "A02")
  )

  expect_equal(
    result$product,
    c("P1", "P2")
  )
})


test_that("sensory_clean converts empty strings to NA", {

  test_data <- data.frame(
    assessor = c("A01", ""),
    product = c("P1", "P2"),
    sweetness = c(6, 7)
  )

  result <- sensory_clean(test_data)

  expect_true(
    is.na(result$assessor[2])
  )
})


test_that("sensory_clean removes completely empty columns", {

  test_data <- data.frame(
    assessor = c("A01", "A02"),
    product = c("P1", "P2"),
    sweetness = c(6, 7),
    empty_column = c(NA, NA)
  )

  result <- sensory_clean(test_data)

  expect_false(
    "empty_column" %in% names(result)
  )

  expect_equal(
    ncol(result),
    3
  )
})


test_that("sensory_clean preserves sensory scores", {

  test_data <- data.frame(
    assessor = c(" A01 ", " A02 "),
    product = c(" P1 ", " P2 "),
    sweetness = c(6.25, 7.50),
    bitterness = c(3.1, 4.2)
  )

  result <- sensory_clean(test_data)

  expect_equal(
    result$sweetness,
    c(6.25, 7.50)
  )

  expect_equal(
    result$bitterness,
    c(3.1, 4.2)
  )
})


test_that("sensory_clean does not remove duplicate rows by default", {

  test_data <- data.frame(
    assessor = c("A01", "A01"),
    product = c("P1", "P1"),
    sweetness = c(6, 6)
  )

  result <- sensory_clean(test_data)

  expect_equal(
    nrow(result),
    2
  )
})


test_that("sensory_clean removes exact duplicates when requested", {

  test_data <- data.frame(
    assessor = c("A01", "A01"),
    product = c("P1", "P1"),
    sweetness = c(6, 6)
  )

  result <- sensory_clean(
    test_data,
    remove_duplicates = TRUE
  )

  expect_equal(
    nrow(result),
    1
  )
})


test_that("sensory_clean returns a tibble", {

  test_data <- data.frame(
    assessor = c("A01", "A02"),
    product = c("P1", "P2"),
    sweetness = c(6, 7)
  )

  result <- sensory_clean(test_data)

  expect_s3_class(
    result,
    "tbl_df"
  )
})


test_that("sensory_clean rejects non-data-frame input", {

  expect_error(
    sensory_clean(c(1, 2, 3)),
    "`data` must be a data frame or tibble"
  )
})
