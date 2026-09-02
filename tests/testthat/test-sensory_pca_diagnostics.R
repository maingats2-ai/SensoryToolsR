make_pca_diagnostic_data <- function() {

  set.seed(2026)

  test_data <- expand.grid(
    assessor = paste0(
      "A0",
      1:6
    ),
    session = paste0(
      "S",
      1:3
    ),
    product = paste0(
      "P",
      1:6
    ),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  profiles <- data.frame(
    product = paste0(
      "P",
      1:6
    ),

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
      assessor_bias[
        test_data$assessor
      ] +
      session_bias[
        test_data$session
      ] +
      stats::rnorm(
        nrow(test_data),
        mean = 0,
        sd = 0.20
      )
  }

  test_data
}


pca_diagnostic_attributes <- c(
  "sweetness",
  "bitterness",
  "umami",
  "fishy_odor",
  "fresh_odor",
  "aftertaste",
  "firmness",
  "juiciness"
)


make_pca_diagnostic_result <- function() {

  test_data <-
    make_pca_diagnostic_data()

  sensory_pca(
    test_data,
    attributes =
      pca_diagnostic_attributes
  )
}


test_that("sensory_pca_diagnostics returns correct object structure", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result
  )

  expect_s3_class(
    result,
    "sensory_pca_diagnostics"
  )

  expect_s3_class(
    result$attribute_diagnostics,
    "tbl_df"
  )

  expect_s3_class(
    result$product_diagnostics,
    "tbl_df"
  )

  expect_s3_class(
    result$variance_table,
    "tbl_df"
  )

  expect_s3_class(
    result$top_attributes,
    "tbl_df"
  )

  expect_s3_class(
    result$top_products,
    "tbl_df"
  )
})


test_that("attribute contributions sum to 100 within each component", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(1, 2)
  )

  totals <- aggregate(
    contribution_percent ~ component,
    data = result$attribute_diagnostics,
    FUN = sum
  )

  expect_equal(
    totals$contribution_percent,
    c(
      100,
      100
    ),
    tolerance = 1e-8
  )
})


test_that("product contributions sum to 100 within each component", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(1, 2)
  )

  totals <- aggregate(
    contribution_percent ~ component,
    data = result$product_diagnostics,
    FUN = sum
  )

  expect_equal(
    totals$contribution_percent,
    c(
      100,
      100
    ),
    tolerance = 1e-8
  )
})


test_that("attribute cos2 values lie between zero and one", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result
  )

  expect_true(
    all(
      result$attribute_diagnostics$cos2 >= 0 &
        result$attribute_diagnostics$cos2 <= 1
    )
  )
})


test_that("product cos2 values lie between zero and one", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result
  )

  expect_true(
    all(
      is.na(
        result$product_diagnostics$cos2
      ) |
        (
          result$product_diagnostics$cos2 >= 0 &
            result$product_diagnostics$cos2 <= 1
        )
    )
  )
})


test_that("diagnostics retain the two-dimensional PCA structure", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      2
    )
  )

  expect_gt(
    result$variance_table$variance_percent[
      result$variance_table$component ==
        "PC1"
    ],
    50
  )

  expect_lt(
    result$variance_table$variance_percent[
      result$variance_table$component ==
        "PC1"
    ],
    90
  )

  expect_gt(
    result$variance_table$variance_percent[
      result$variance_table$component ==
        "PC2"
    ],
    15
  )

  expect_gt(
    sum(
      result$variance_table$
        variance_percent
    ),
    90
  )
})


test_that("PC1 top attributes represent freshness and deterioration", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      2
    ),
    top_n = 5
  )

  pc1_top <- result$top_attributes[
    result$top_attributes$component ==
      "PC1",
    ,
    drop = FALSE
  ]

  expect_true(
    "fishy_odor" %in%
      pc1_top$attribute
  )

  expect_true(
    "fresh_odor" %in%
      pc1_top$attribute
  )

  expect_true(
    "bitterness" %in%
      pc1_top$attribute
  )

  expect_true(
    "sweetness" %in%
      pc1_top$attribute
  )
})


test_that("PC1 freshness and deterioration attributes point in opposite directions", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 1
  )

  pc1 <- result$attribute_diagnostics

  fresh_loading <- pc1$loading[
    pc1$attribute ==
      "fresh_odor"
  ]

  fishy_loading <- pc1$loading[
    pc1$attribute ==
      "fishy_odor"
  ]

  sweetness_loading <- pc1$loading[
    pc1$attribute ==
      "sweetness"
  ]

  bitterness_loading <- pc1$loading[
    pc1$attribute ==
      "bitterness"
  ]

  expect_lt(
    fresh_loading *
      fishy_loading,
    0
  )

  expect_lt(
    sweetness_loading *
      bitterness_loading,
    0
  )
})


test_that("PC2 is dominated by firmness and juiciness", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      2
    ),
    top_n = 3
  )

  pc2_top <- result$top_attributes[
    result$top_attributes$component ==
      "PC2",
    ,
    drop = FALSE
  ]

  expect_true(
    "firmness" %in%
      pc2_top$attribute
  )

  expect_true(
    "juiciness" %in%
      pc2_top$attribute
  )
})


test_that("firmness and juiciness dominate PC2 contribution", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 2
  )

  pc2 <- result$attribute_diagnostics

  firmness_contribution <-
    pc2$contribution_percent[
      pc2$attribute ==
        "firmness"
    ]

  juiciness_contribution <-
    pc2$contribution_percent[
      pc2$attribute ==
        "juiciness"
    ]

  expect_gt(
    firmness_contribution,
    40
  )

  expect_gt(
    juiciness_contribution,
    40
  )

  expect_gt(
    firmness_contribution +
      juiciness_contribution,
    90
  )
})


test_that("firmness and juiciness point in opposite directions on PC2", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 2
  )

  pc2 <- result$attribute_diagnostics

  firmness_loading <-
    pc2$loading[
      pc2$attribute ==
        "firmness"
    ]

  juiciness_loading <-
    pc2$loading[
      pc2$attribute ==
        "juiciness"
    ]

  expect_lt(
    firmness_loading *
      juiciness_loading,
    0
  )
})


test_that("PC2 freshness-related attributes make small contributions", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 2
  )

  pc2 <- result$attribute_diagnostics

  freshness_attributes <- c(
    "sweetness",
    "bitterness",
    "umami",
    "fishy_odor",
    "fresh_odor",
    "aftertaste"
  )

  freshness_contributions <-
    pc2$contribution_percent[
      pc2$attribute %in%
        freshness_attributes
    ]

  expect_true(
    all(
      freshness_contributions < 5
    )
  )
})


test_that("diagnostics contain all requested components", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      3
    )
  )

  expect_equal(
    unique(
      result$attribute_diagnostics$component
    ),
    c(
      "PC1",
      "PC3"
    )
  )

  expect_equal(
    unique(
      result$product_diagnostics$component
    ),
    c(
      "PC1",
      "PC3"
    )
  )
})


test_that("top_n controls number of returned top contributors", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      2
    ),
    top_n = 3
  )

  attribute_counts <- table(
    result$top_attributes$component
  )

  product_counts <- table(
    result$top_products$component
  )

  expect_true(
    all(
      attribute_counts == 3
    )
  )

  expect_true(
    all(
      product_counts == 3
    )
  )
})


test_that("top attributes are ordered by contribution", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 1,
    top_n = 5
  )

  contributions <-
    result$top_attributes$
    contribution_percent

  expect_true(
    all(
      diff(contributions) <= 0
    )
  )
})


test_that("top products are ordered by contribution", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 1,
    top_n = 5
  )

  contributions <-
    result$top_products$
    contribution_percent

  expect_true(
    all(
      diff(contributions) <= 0
    )
  )
})


test_that("diagnostics retain PCA loading values", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 1
  )

  expected <-
    pca_result$loadings$PC1

  actual <-
    result$attribute_diagnostics$
    loading

  expect_equal(
    actual,
    expected
  )
})


test_that("diagnostics retain PCA score values", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 1
  )

  expected <-
    pca_result$scores$PC1

  actual <-
    result$product_diagnostics$
    score

  expect_equal(
    actual,
    expected
  )
})


test_that("PC1 strongly represents extreme freshness products", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = 1
  )

  pc1_products <-
    result$product_diagnostics

  p1_cos2 <-
    pc1_products$cos2[
      pc1_products$product ==
        "P1"
    ]

  p5_cos2 <-
    pc1_products$cos2[
      pc1_products$product ==
        "P5"
    ]

  expect_gt(
    p1_cos2,
    0.70
  )

  expect_gt(
    p5_cos2,
    0.70
  )
})


test_that("sensory_pca_diagnostics rejects invalid input object", {

  expect_error(
    sensory_pca_diagnostics(
      data.frame(
        x = 1:3
      )
    ),
    "`x` must be an object returned by sensory_pca"
  )
})


test_that("sensory_pca_diagnostics rejects unavailable components", {

  pca_result <-
    make_pca_diagnostic_result()

  expect_error(
    sensory_pca_diagnostics(
      pca_result,
      components = 99
    ),
    "valid principal component numbers"
  )
})


test_that("sensory_pca_diagnostics rejects duplicate components", {

  pca_result <-
    make_pca_diagnostic_result()

  expect_error(
    sensory_pca_diagnostics(
      pca_result,
      components = c(
        1,
        1
      )
    ),
    "must not contain duplicate values"
  )
})


test_that("sensory_pca_diagnostics validates top_n", {

  pca_result <-
    make_pca_diagnostic_result()

  expect_error(
    sensory_pca_diagnostics(
      pca_result,
      top_n = 0
    ),
    "`top_n` must be a positive integer"
  )

  expect_error(
    sensory_pca_diagnostics(
      pca_result,
      top_n = 2.5
    ),
    "`top_n` must be a positive integer"
  )
})


test_that("variance table contains selected components", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      2
    )
  )

  expect_equal(
    result$variance_table$component,
    c(
      "PC1",
      "PC2"
    )
  )
})

test_that("attribute cos2 matches PCA variable geometry", {

  pca_result <-
    make_pca_diagnostic_result()

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      2
    )
  )

  profile_matrix <- as.matrix(
    pca_result$product_profiles[
      pca_result$attributes
    ]
  )

  processed_profile_matrix <-
    profile_matrix

  if (!identical(
    pca_result$pca_model$center,
    FALSE
  )) {

    processed_profile_matrix <- sweep(
      processed_profile_matrix,
      2,
      pca_result$pca_model$center,
      FUN = "-"
    )
  }

  if (!identical(
    pca_result$pca_model$scale,
    FALSE
  )) {

    processed_profile_matrix <- sweep(
      processed_profile_matrix,
      2,
      pca_result$pca_model$scale,
      FUN = "/"
    )
  }

  attribute_inertia <- colSums(
    processed_profile_matrix^2
  ) / (
    nrow(processed_profile_matrix) - 1
  )

  variable_coordinates <- sweep(
    pca_result$pca_model$rotation,
    2,
    pca_result$pca_model$sdev,
    FUN = "*"
  )

  expected_cos2 <- sweep(
    variable_coordinates^2,
    1,
    attribute_inertia,
    FUN = "/"
  )

  actual_pc1 <-
    result$attribute_diagnostics$cos2[
      result$attribute_diagnostics$
        component == "PC1"
    ]

  actual_pc2 <-
    result$attribute_diagnostics$cos2[
      result$attribute_diagnostics$
        component == "PC2"
    ]

  expect_equal(
    actual_pc1,
    unname(
      expected_cos2[, "PC1"]
    ),
    tolerance = 1e-10
  )

  expect_equal(
    actual_pc2,
    unname(
      expected_cos2[, "PC2"]
    ),
    tolerance = 1e-10
  )
})


test_that("attribute cos2 is correct for standardized PCA", {

  test_data <-
    make_pca_diagnostic_data()

  pca_result <- sensory_pca(
    test_data,
    attributes =
      pca_diagnostic_attributes,
    scale = TRUE
  )

  result <- sensory_pca_diagnostics(
    pca_result,
    components = c(
      1,
      2
    )
  )

  variable_coordinates <- sweep(
    pca_result$pca_model$rotation,
    2,
    pca_result$pca_model$sdev,
    FUN = "*"
  )

  expected_cos2 <-
    variable_coordinates^2

  actual_pc1 <-
    result$attribute_diagnostics$cos2[
      result$attribute_diagnostics$
        component == "PC1"
    ]

  actual_pc2 <-
    result$attribute_diagnostics$cos2[
      result$attribute_diagnostics$
        component == "PC2"
    ]

  expect_equal(
    actual_pc1,
    unname(
      expected_cos2[, "PC1"]
    ),
    tolerance = 1e-10
  )

  expect_equal(
    actual_pc2,
    unname(
      expected_cos2[, "PC2"]
    ),
    tolerance = 1e-10
  )

  expect_true(
    all(
      result$attribute_diagnostics$cos2 >= 0 &
        result$attribute_diagnostics$cos2 <= 1
    )
  )
})
