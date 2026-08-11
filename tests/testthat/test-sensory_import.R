test_that("sensory_import imports a CSV file correctly", {

  temp_file <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    Assessor = c("A01", "A02"),
    Product = c("P1", "P2"),
    Sweetness = c(6, 7)
  )

  utils::write.csv(
    test_data,
    temp_file,
    row.names = FALSE
  )

  result <- sensory_import(temp_file)

  expect_s3_class(result, "data.frame")

  expect_equal(
    nrow(result),
    2
  )

  expect_equal(
    ncol(result),
    3
  )

  expect_equal(
    names(result),
    c(
      "assessor",
      "product",
      "sweetness"
    )
  )

  expect_equal(
    result$sweetness,
    c(6, 7)
  )
})


test_that("sensory_import reports an error for a missing file", {

  expect_error(
    sensory_import(
      "this_file_does_not_exist.csv"
    ),
    "File not found"
  )
})


test_that("sensory_import rejects unsupported file formats", {

  temp_file <- tempfile(fileext = ".txt")

  writeLines(
    "test",
    temp_file
  )

  expect_error(
    sensory_import(temp_file),
    "Unsupported file format"
  )
})


test_that("sensory_import requires one valid file path", {

  expect_error(
    sensory_import(),
    "Please provide a single file path"
  )

  expect_error(
    sensory_import(
      c("file1.csv", "file2.csv")
    ),
    "Please provide a single file path"
  )
})
