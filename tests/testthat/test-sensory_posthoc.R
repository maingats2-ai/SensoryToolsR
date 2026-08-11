test_that("sensory_posthoc performs Tukey comparisons", {

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

  fit <- sensory_anova(
    test_data,
    attribute = "sweetness"
  )

  result <- sensory_posthoc(fit)

  expect_s3_class(
    result,
    "sensory_posthoc"
  )

  expect_s3_class(
    result$comparisons,
    "tbl_df"
  )

  expect_equal(
    nrow(result$comparisons),
    3
  )

  expect_equal(
    result$method,
    "Tukey HSD"
  )

  expect_true(
    all(
      c(
        "comparison",
        "difference",
        "ci_lower",
        "ci_upper",
        "p_adjusted",
        "significant"
      ) %in% names(result$comparisons)
    )
  )
})


test_that("sensory_posthoc detects significant product differences", {

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

  fit <- sensory_anova(
    test_data,
    attribute = "sweetness"
  )

  result <- sensory_posthoc(fit)

  expect_true(
    any(result$comparisons$significant)
  )

  expect_true(
    all(
      result$comparisons$p_adjusted >= 0 &
        result$comparisons$p_adjusted <= 1
    )
  )
})


test_that("sensory_posthoc validates input class", {

  expect_error(
    sensory_posthoc(
      data.frame(x = 1:3)
    ),
    "`x` must be an object returned by sensory_anova"
  )
})


test_that("sensory_posthoc validates confidence level", {

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

  fit <- sensory_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_error(
    sensory_posthoc(
      fit,
      conf_level = 1.2
    ),
    "`conf_level` must be a single number between 0 and 1"
  )
})
