test_that("sensory_anova fits a valid sensory panel ANOVA", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03", "A04"),
      each = 3
    ),
    product = rep(
      c("P1", "P2", "P3"),
      times = 4
    ),
    sweetness = c(
      7, 5, 3,
      8, 5, 2,
      7, 4, 3,
      8, 5, 3
    )
  )

  result <- sensory_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_s3_class(
    result,
    "sensory_anova"
  )

  expect_s3_class(
    result$model,
    "aov"
  )

  expect_s3_class(
    result$anova_table,
    "tbl_df"
  )

  expect_equal(
    result$n_observations,
    12
  )

  expect_equal(
    result$n_products,
    3
  )

  expect_equal(
    result$n_assessors,
    4
  )

  expect_true(
    is.numeric(result$product_p_value)
  )

  expect_lt(
    result$product_p_value,
    0.05
  )
})


test_that("sensory_anova reports product and assessor terms", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03", "A04"),
      each = 2
    ),
    product = rep(
      c("P1", "P2"),
      times = 4
    ),
    sweetness = c(
      7.2, 4.8,
      7.8, 5.0,
      6.9, 4.7,
      7.5, 5.4
    )
  )

  result <- sensory_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    "product" %in% result$anova_table$term
  )

  expect_true(
    "assessor" %in% result$anova_table$term
  )
})


test_that("sensory_anova handles missing sensory scores", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03", "A04"),
      each = 2
    ),
    product = rep(
      c("P1", "P2"),
      times = 4
    ),
    sweetness = c(
      7.2, 4.8,
      7.8, NA,
      6.9, 4.7,
      7.5, 5.4
    )
  )

  result <- sensory_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_equal(
    result$n_observations,
    7
  )
})


test_that("sensory_anova rejects a missing attribute", {

  test_data <- data.frame(
    assessor = c("A01", "A02"),
    product = c("P1", "P2")
  )

  expect_error(
    sensory_anova(
      test_data,
      attribute = "sweetness"
    ),
    "Missing required column"
  )
})


test_that("sensory_anova rejects non-numeric sensory attributes", {

  test_data <- data.frame(
    assessor = c(
      "A01", "A01",
      "A02", "A02"
    ),
    product = c(
      "P1", "P2",
      "P1", "P2"
    ),
    sweetness = c(
      "high", "low",
      "high", "low"
    )
  )

  expect_error(
    sensory_anova(
      test_data,
      attribute = "sweetness"
    ),
    "Sensory attribute must be numeric"
  )
})


test_that("sensory_anova requires at least two products", {

  test_data <- data.frame(
    assessor = c(
      "A01", "A02", "A03"
    ),
    product = c(
      "P1", "P1", "P1"
    ),
    sweetness = c(
      6, 7, 8
    )
  )

  expect_error(
    sensory_anova(
      test_data,
      attribute = "sweetness"
    ),
    "At least two products"
  )
})


test_that("sensory_anova requires at least two assessors", {

  test_data <- data.frame(
    assessor = c(
      "A01", "A01", "A01"
    ),
    product = c(
      "P1", "P2", "P3"
    ),
    sweetness = c(
      7, 5, 3
    )
  )

  expect_error(
    sensory_anova(
      test_data,
      attribute = "sweetness"
    ),
    "At least two assessors"
  )
})

test_that("sensory_anova warns when assessor-product combinations are replicated", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03"),
      each = 4
    ),

    product = rep(
      c("P1", "P2"),
      times = 6
    ),

    sweetness = c(
      7.0, 5.0,
      7.2, 5.1,

      7.5, 5.3,
      7.3, 5.4,

      6.8, 4.7,
      7.0, 4.9
    )
  )

  expect_warning(
    sensory_anova(
      test_data,
      attribute = "sweetness"
    ),
    "Replicated Assessor x Product observations"
  )
})
