make_panel_plot_test_data <- function() {

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

  test_data
}


test_that("sensory_panel_plot returns ggplot object", {

  test_data <-
    make_panel_plot_test_data()

  performance <-
    sensory_panel_performance(
      test_data,
      attribute = "sweetness"
    )

  p <- sensory_panel_plot(
    performance,
    metric = "discrimination"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


test_that("sensory_panel_plot supports all three metrics", {

  test_data <-
    make_panel_plot_test_data()

  performance <-
    sensory_panel_performance(
      test_data,
      attribute = "sweetness"
    )

  p1 <- sensory_panel_plot(
    performance,
    metric = "discrimination"
  )

  p2 <- sensory_panel_plot(
    performance,
    metric = "repeatability"
  )

  p3 <- sensory_panel_plot(
    performance,
    metric = "agreement"
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


test_that("sensory_panel_plot validates input class", {

  expect_error(
    sensory_panel_plot(
      data.frame(x = 1:3)
    ),
    "`x` must be an object returned by sensory_panel_performance"
  )
})


test_that("sensory_panel_plot validates metric name", {

  test_data <-
    make_panel_plot_test_data()

  performance <-
    sensory_panel_performance(
      test_data,
      attribute = "sweetness"
    )

  expect_error(
    sensory_panel_plot(
      performance,
      metric = "invalid_metric"
    )
  )
})


test_that("sensory_panel_plot validates logical arguments", {

  test_data <-
    make_panel_plot_test_data()

  performance <-
    sensory_panel_performance(
      test_data,
      attribute = "sweetness"
    )

  expect_error(
    sensory_panel_plot(
      performance,
      sort = "yes"
    ),
    "`sort` must be TRUE or FALSE"
  )

  expect_error(
    sensory_panel_plot(
      performance,
      show_status = "yes"
    ),
    "`show_status` must be TRUE or FALSE"
  )
})
