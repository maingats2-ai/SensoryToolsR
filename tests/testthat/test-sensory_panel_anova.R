test_that("sensory_panel_anova fits a replicated trained-panel model", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03", "A04"),
      each = 6
    ),

    session = rep(
      rep(c("S1", "S2"), each = 3),
      times = 4
    ),

    product = rep(
      c("P1", "P2", "P3"),
      times = 8
    ),

    sweetness = c(
      7.2, 5.1, 3.2,
      7.0, 5.3, 3.4,

      7.8, 5.4, 2.9,
      7.6, 5.2, 3.1,

      6.9, 4.8, 3.5,
      7.1, 4.9, 3.3,

      7.5, 5.0, 3.1,
      7.4, 5.2, 3.2
    )
  )

  result <- sensory_panel_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_s3_class(
    result,
    "sensory_panel_anova"
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
    24
  )

  expect_equal(
    result$n_products,
    3
  )

  expect_equal(
    result$n_assessors,
    4
  )

  expect_equal(
    result$n_sessions,
    2
  )

  expect_lt(
    result$product_p_value,
    0.05
  )
})


test_that("sensory_panel_anova contains the expected ANOVA terms", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03", "A04"),
      each = 6
    ),

    session = rep(
      rep(c("S1", "S2"), each = 3),
      times = 4
    ),

    product = rep(
      c("P1", "P2", "P3"),
      times = 8
    ),

    sweetness = c(
      7.2, 5.1, 3.2,
      7.0, 5.3, 3.4,

      7.8, 5.4, 2.9,
      7.6, 5.2, 3.1,

      6.9, 4.8, 3.5,
      7.1, 4.9, 3.3,

      7.5, 5.0, 3.1,
      7.4, 5.2, 3.2
    )
  )

  result <- sensory_panel_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    "product" %in% result$anova_table$term
  )

  expect_true(
    "assessor" %in% result$anova_table$term
  )

  expect_true(
    "session" %in% result$anova_table$term
  )

  expect_true(
    "product:assessor" %in% result$anova_table$term
  )

  expect_true(
    "Residuals" %in% result$anova_table$term
  )
})


test_that("sensory_panel_anova returns interaction p-value", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03", "A04"),
      each = 6
    ),

    session = rep(
      rep(c("S1", "S2"), each = 3),
      times = 4
    ),

    product = rep(
      c("P1", "P2", "P3"),
      times = 8
    ),

    sweetness = c(
      7.2, 5.1, 3.2,
      7.0, 5.3, 3.4,

      7.8, 5.4, 2.9,
      7.6, 5.2, 3.1,

      6.9, 4.8, 3.5,
      7.1, 4.9, 3.3,

      7.5, 5.0, 3.1,
      7.4, 5.2, 3.2
    )
  )

  result <- sensory_panel_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_true(
    is.numeric(result$interaction_p_value)
  )

  expect_length(
    result$interaction_p_value,
    1
  )
})


test_that("sensory_panel_anova removes incomplete extra observations", {

  test_data <- data.frame(
    assessor = c(
      rep(
        c("A01", "A02", "A03"),
        each = 4
      ),
      "A01"
    ),

    session = c(
      rep(
        c("S1", "S1", "S2", "S2"),
        times = 3
      ),
      "S1"
    ),

    product = c(
      rep(
        c("P1", "P2"),
        times = 6
      ),
      "P1"
    ),

    sweetness = c(
      7.0, 5.0,
      7.2, 5.1,

      7.5, 5.3,
      7.3, 5.4,

      6.8, 4.7,
      7.0, 4.9,

      NA
    )
  )

  result <- sensory_panel_anova(
    test_data,
    attribute = "sweetness"
  )

  expect_equal(
    result$n_observations,
    12
  )
})

test_that("sensory_panel_anova requires at least two sessions", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03"),
      each = 2
    ),

    session = rep(
      "S1",
      6
    ),

    product = rep(
      c("P1", "P2"),
      times = 3
    ),

    sweetness = c(
      7.0, 5.0,
      7.4, 5.2,
      6.8, 4.7
    )
  )

  expect_error(
    sensory_panel_anova(
      test_data,
      attribute = "sweetness"
    ),
    "At least two sessions or replicates"
  )
})


test_that("sensory_panel_anova rejects non-numeric attributes", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02"),
      each = 4
    ),

    session = rep(
      c("S1", "S1", "S2", "S2"),
      times = 2
    ),

    product = rep(
      c("P1", "P2"),
      times = 4
    ),

    sweetness = rep(
      c("high", "low"),
      times = 4
    )
  )

  expect_error(
    sensory_panel_anova(
      test_data,
      attribute = "sweetness"
    ),
    "Sensory attribute must be numeric"
  )
})


test_that("sensory_panel_anova reports missing required columns", {

  test_data <- data.frame(
    assessor = c("A01", "A02"),
    product = c("P1", "P2"),
    sweetness = c(7, 5)
  )

  expect_error(
    sensory_panel_anova(
      test_data,
      attribute = "sweetness"
    ),
    "Missing required column"
  )
})

test_that("product effect uses Product x Assessor as error term", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03", "A04"),
      each = 6
    ),

    session = rep(
      rep(c("S1", "S2"), each = 3),
      times = 4
    ),

    product = rep(
      c("P1", "P2", "P3"),
      times = 8
    ),

    sweetness = c(
      7.2, 5.1, 3.2,
      7.0, 5.3, 3.4,

      7.8, 5.4, 2.9,
      7.6, 5.2, 3.1,

      6.9, 4.8, 3.5,
      7.1, 4.9, 3.3,

      7.5, 5.0, 3.1,
      7.4, 5.2, 3.2
    )
  )

  result <- sensory_panel_anova(
    test_data,
    attribute = "sweetness"
  )

  raw_anova <- stats::anova(
    result$model
  )

  ms_product <- raw_anova[
    "product",
    "Mean Sq"
  ]

  ms_product_assessor <- raw_anova[
    "product:assessor",
    "Mean Sq"
  ]

  df_product <- raw_anova[
    "product",
    "Df"
  ]

  df_product_assessor <- raw_anova[
    "product:assessor",
    "Df"
  ]

  expected_f <-
    ms_product /
    ms_product_assessor

  expected_p <- stats::pf(
    expected_f,
    df1 = df_product,
    df2 = df_product_assessor,
    lower.tail = FALSE
  )

  product_row <-
    result$anova_table[
      result$anova_table$term == "product",
      ,
      drop = FALSE
    ]

  expect_equal(
    product_row$f_value,
    expected_f,
    tolerance = 1e-12
  )

  expect_equal(
    product_row$p_value,
    expected_p,
    tolerance = 1e-12
  )

  expect_equal(
    result$product_p_value,
    expected_p,
    tolerance = 1e-12
  )
})

test_that("sensory_panel_anova rejects an incomplete panel design", {

  test_data <- data.frame(
    assessor = rep(
      c("A01", "A02", "A03"),
      each = 4
    ),

    session = rep(
      c("S1", "S1", "S2", "S2"),
      times = 3
    ),

    product = rep(
      c("P1", "P2"),
      times = 6
    ),

    sweetness = c(
      7.0, 5.0,
      7.2, 5.1,

      7.5, NA,
      7.3, 5.4,

      6.8, 4.7,
      7.0, 4.9
    )
  )

  expect_error(
    sensory_panel_anova(
      test_data,
      attribute = "sweetness"
    ),
    "complete Assessor x Product x Session design"
  )
})
