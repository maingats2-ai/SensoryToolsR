make_summary_qda_data <- function() {

  set.seed(
    2026
  )

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
        nrow(
          test_data
        ),
        mean = 0,
        sd = 0.20
      )
  }

  test_data
}


summary_qda_attributes <- c(
  "sweetness",
  "bitterness",
  "umami",
  "fishy_odor",
  "fresh_odor",
  "aftertaste",
  "firmness",
  "juiciness"
)


make_summary_qda_result <- function() {

  test_data <-
    make_summary_qda_data()

  sensory_qda(
    test_data,
    attributes =
      summary_qda_attributes
  )
}


test_that("summary.sensory_qda returns correct object structure", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expect_s3_class(
    result,
    "summary_sensory_qda"
  )

  expect_s3_class(
    result$overview,
    "tbl_df"
  )

  expect_s3_class(
    result$attribute_summary,
    "tbl_df"
  )

  expect_s3_class(
    result$assessor_summary,
    "tbl_df"
  )

  expect_s3_class(
    result$review_overview,
    "tbl_df"
  )

  expect_s3_class(
    result$pca_variance,
    "tbl_df"
  )

  expect_s3_class(
    result$pca_top_attributes,
    "tbl_df"
  )
})


test_that("summary.sensory_qda contains eight sensory attributes", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expect_equal(
    nrow(
      result$attribute_summary
    ),
    8
  )

  expect_equal(
    sort(
      result$attribute_summary$
        attribute
    ),
    sort(
      summary_qda_attributes
    )
  )
})


test_that("summary.sensory_qda assessor summary contains every attribute", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expect_equal(
    nrow(
      result$assessor_summary
    ),
    8
  )

  expect_equal(
    sort(
      result$assessor_summary$
        attribute
    ),
    sort(
      summary_qda_attributes
    )
  )
})


test_that("summary.sensory_qda reports six assessors per attribute", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expect_true(
    all(
      result$assessor_summary$
        n_assessors ==
        6
    )
  )
})


test_that("summary.sensory_qda review percentages lie between zero and 100", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expect_true(
    all(
      result$assessor_summary$
        percent_review >= 0 &
        result$assessor_summary$
        percent_review <= 100
    )
  )
})


test_that("summary.sensory_qda review overview is internally consistent", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expected_attributes_with_review <-
    sum(
      result$assessor_summary$
        n_review > 0
    )

  expected_total_reviews <-
    sum(
      result$assessor_summary$
        n_review
    )

  expect_equal(
    result$review_overview$
      attributes_with_review,
    expected_attributes_with_review
  )

  expect_equal(
    result$review_overview$
      total_review_flags,
    expected_total_reviews
  )
})


test_that("summary.sensory_qda PCA variance matches original QDA result", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expect_equal(
    result$pca_variance,
    qda_result$pca$
      variance_table
  )
})


test_that("summary.sensory_qda returns meaningful PC1 and PC2 variance", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result
  )

  expect_gt(
    result$pca_variance$
      variance_percent[
        result$pca_variance$
          component ==
          "PC1"
      ],
    50
  )

  expect_gt(
    result$pca_variance$
      variance_percent[
        result$pca_variance$
          component ==
          "PC2"
      ],
    15
  )
})


test_that("summary.sensory_qda returns top PCA attributes", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result,
    top_n = 3
  )

  component_counts <- table(
    result$pca_top_attributes$
      component
  )

  expect_true(
    all(
      component_counts == 3
    )
  )
})


test_that("summary.sensory_qda identifies texture attributes on PC2", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result,
    top_n = 3
  )

  pc2_top <-
    result$pca_top_attributes[
      result$pca_top_attributes$
        component ==
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


test_that("summary.sensory_qda keeps top attributes ordered by contribution", {

  qda_result <-
    make_summary_qda_result()

  result <- summary(
    qda_result,
    top_n = 5
  )

  for (
    component_name in
    unique(
      result$pca_top_attributes$
      component
    )
  ) {

    current <-
      result$pca_top_attributes[
        result$pca_top_attributes$
          component ==
          component_name,
        ,
        drop = FALSE
      ]

    expect_true(
      all(
        diff(
          current$
            contribution_percent
        ) <= 0
      )
    )
  }
})


test_that("summary.sensory_qda preserves QDA settings", {

  test_data <-
    make_summary_qda_data()

  qda_result <- sensory_qda(
    test_data,
    attributes =
      summary_qda_attributes,
    alpha = 0.01,
    agreement_threshold = 0.75,
    repeatability_multiplier = 2,
    pca_scale = TRUE
  )

  result <- summary(
    qda_result
  )

  expect_equal(
    result$settings$alpha,
    0.01
  )

  expect_equal(
    result$settings$
      agreement_threshold,
    0.75
  )

  expect_equal(
    result$settings$
      repeatability_multiplier,
    2
  )

  expect_true(
    result$settings$
      pca_scale
  )
})


test_that("summary.sensory_qda validates top_n", {

  qda_result <-
    make_summary_qda_result()

  expect_error(
    summary(
      qda_result,
      top_n = 0
    ),
    "`top_n` must be a positive integer"
  )

  expect_error(
    summary(
      qda_result,
      top_n = 2.5
    ),
    "`top_n` must be a positive integer"
  )
})


test_that("summary.sensory_qda rejects non-QDA objects", {

  expect_error(
    summary.sensory_qda(
      data.frame(
        x = 1:3
      )
    ),
    "`object` must be an object returned by sensory_qda"
  )
})
