make_print_qda_data <- function() {

  set.seed(2026)

  test_data <- expand.grid(
    assessor = paste0("A0", 1:6),
    session = paste0("S", 1:3),
    product = paste0("P", 1:6),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  profiles <- data.frame(
    product = paste0("P", 1:6),

    sweetness = c(
      7.2, 6.8, 5.0,
      4.7, 3.2, 3.5
    ),

    bitterness = c(
      2.0, 2.5, 4.0,
      4.3, 6.3, 6.0
    ),

    umami = c(
      7.0, 6.5, 5.2,
      5.0, 3.6, 3.8
    ),

    fishy_odor = c(
      2.0, 2.7, 4.3,
      4.0, 6.8, 6.3
    ),

    fresh_odor = c(
      7.5, 6.8, 5.1,
      5.3, 3.1, 3.4
    ),

    aftertaste = c(
      2.5, 3.0, 4.5,
      4.2, 6.4, 6.0
    ),

    firmness = c(
      7.2, 3.6, 7.0,
      3.8, 6.8, 3.5
    ),

    juiciness = c(
      3.5, 7.1, 3.8,
      6.9, 4.0, 7.0
    )
  )

  test_data <- merge(
    test_data,
    profiles,
    by = "product",
    sort = FALSE
  )

  attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste",
    "firmness",
    "juiciness"
  )

  assessor_bias <- c(
    A01 = 0.00,
    A02 = 0.15,
    A03 = -0.12,
    A04 = 0.10,
    A05 = -0.08,
    A06 = 0.05
  )

  session_bias <- c(
    S1 = 0.00,
    S2 = 0.08,
    S3 = -0.06
  )

  for (attribute in attributes) {

    test_data[[attribute]] <-
      test_data[[attribute]] +
      assessor_bias[test_data$assessor] +
      session_bias[test_data$session] +
      stats::rnorm(
        nrow(test_data),
        mean = 0,
        sd = 0.20
      )
  }

  test_data
}


print_qda_attributes <- c(
  "sweetness",
  "bitterness",
  "umami",
  "fishy_odor",
  "fresh_odor",
  "aftertaste",
  "firmness",
  "juiciness"
)


make_print_qda_result <- function() {

  test_data <- make_print_qda_data()

  sensory_qda(
    test_data,
    attributes = print_qda_attributes
  )
}


test_that("print.sensory_qda returns object invisibly", {

  result <- make_print_qda_result()

  printed <- withVisible(
    print(result)
  )

  expect_false(
    printed$visible
  )

  expect_identical(
    printed$value,
    result
  )
})


test_that("print.sensory_qda prints package heading", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  expect_true(
    any(
      grepl(
        "SensoryToolsR - Integrated QDA Analysis",
        output,
        fixed = TRUE
      )
    )
  )
})


test_that("print.sensory_qda prints experimental design", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  expected_labels <- c(
    "Experimental design",
    "Products:",
    "Assessors:",
    "Sessions:",
    "Sensory attributes:",
    "Observations:"
  )

  for (label in expected_labels) {

    expect_true(
      any(
        grepl(
          label,
          output,
          fixed = TRUE
        )
      )
    )
  }
})


test_that("print.sensory_qda reports correct experimental counts", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  product_line <- output[
    grepl("Products:", output, fixed = TRUE)
  ]

  assessor_line <- output[
    grepl("Assessors:", output, fixed = TRUE)
  ]

  session_line <- output[
    grepl("Sessions:", output, fixed = TRUE)
  ]

  attribute_line <- output[
    grepl("Sensory attributes:", output, fixed = TRUE)
  ]

  observation_line <- output[
    grepl("Observations:", output, fixed = TRUE)
  ]

  expect_match(
    product_line,
    "6"
  )

  expect_match(
    assessor_line,
    "6"
  )

  expect_match(
    session_line,
    "3"
  )

  expect_match(
    attribute_line,
    "8"
  )

  expect_match(
    observation_line,
    "108"
  )
})


test_that("print.sensory_qda prints panel analysis", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  expect_true(
    any(
      grepl(
        "Panel analysis",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Significant product effects:",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Significant P x A interactions:",
        output,
        fixed = TRUE
      )
    )
  )
})


test_that("print.sensory_qda prints PCA information", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  expected_labels <- c(
    "Principal component analysis",
    "PC1:",
    "PC2:",
    "PC1 + PC2:",
    "Scaling:"
  )

  for (label in expected_labels) {

    expect_true(
      any(
        grepl(
          label,
          output,
          fixed = TRUE
        )
      )
    )
  }
})


test_that("print.sensory_qda reports expected PCA percentages", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  pc1_line <- output[
    grepl("PC1:", output, fixed = TRUE) &
      !grepl("PC1 + PC2", output, fixed = TRUE)
  ]

  pc2_line <- output[
    grepl("PC2:", output, fixed = TRUE) &
      !grepl("PC1 + PC2", output, fixed = TRUE)
  ]

  combined_line <- output[
    grepl("PC1 + PC2:", output, fixed = TRUE)
  ]

  expect_match(
    pc1_line,
    "71"
  )

  expect_match(
    pc2_line,
    "27"
  )

  expect_match(
    combined_line,
    "99"
  )
})


test_that("print.sensory_qda reports assessor screening", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  expect_true(
    any(
      grepl(
        "Assessor screening",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Total review flags:",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "Attributes with review flags:",
        output,
        fixed = TRUE
      )
    )
  )
})


test_that("print.sensory_qda lists available result components", {

  result <- make_print_qda_result()

  output <- capture.output(
    print(result)
  )

  expected_components <- c(
    "$overview",
    "$validation",
    "$summary",
    "$panel",
    "$pca",
    "$pca_diagnostics"
  )

  for (component in expected_components) {

    expect_true(
      any(
        grepl(
          component,
          output,
          fixed = TRUE
        )
      )
    )
  }
})


test_that("print.sensory_qda reports standardized scaling", {

  test_data <- make_print_qda_data()

  result <- sensory_qda(
    test_data,
    attributes = print_qda_attributes,
    pca_scale = TRUE
  )

  output <- capture.output(
    print(result)
  )

  scaling_line <- output[
    grepl(
      "Scaling:",
      output,
      fixed = TRUE
    )
  ]

  expect_true(
    any(
      grepl(
        "standardized",
        scaling_line,
        fixed = TRUE
      )
    )
  )
})


test_that("print.sensory_qda rejects non-QDA objects", {

  expect_error(
    print.sensory_qda(
      data.frame(
        x = 1:3
      )
    ),
    "`x` must be an object returned by sensory_qda"
  )
})
