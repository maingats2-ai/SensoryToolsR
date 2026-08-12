# SensoryToolsR

<!-- badges: start -->

<!-- badges: end -->

**SensoryToolsR** is an R package for the analysis of sensory evaluation data, with an initial focus on **Quantitative Descriptive Analysis (QDA)** and trained sensory panel data.

The package provides an integrated workflow for importing, checking, cleaning, summarizing, analysing, and visualizing sensory data. It also includes tools for evaluating panel performance and exploring multivariate sensory profiles using principal component analysis (PCA).

> **Development status:** SensoryToolsR is currently under active development. Version 0.1.0 should be considered a development version.

## Main features

SensoryToolsR currently provides tools for:

* importing sensory datasets from CSV and Excel files;
* cleaning sensory datasets;
* validating experimental structure and panel completeness;
* generating descriptive statistics for sensory attributes;
* performing product-level ANOVA;
* performing Tukey post-hoc comparisons;
* analysing product, assessor, session, and Product × Assessor effects;
* evaluating individual assessor and overall panel performance;
* analysing multiple sensory attributes across a panel;
* performing PCA on product sensory profiles;
* generating PCA plots and PCA diagnostics;
* integrating the main analyses into a QDA workflow.

## Installation

SensoryToolsR is currently available from GitHub.

Install the development version with:

```r
# install.packages("pak")
pak::pak("maingats2-ai/SensoryToolsR")
```

Alternatively, using `remotes`:

```r
# install.packages("remotes")
remotes::install_github("maingats2-ai/SensoryToolsR")
```

Then load the package:

```r
library(SensoryToolsR)
```

## Core workflow

A typical SensoryToolsR workflow is:

```text
Import
  ↓
Validate
  ↓
Clean
  ↓
Validate again
  ↓
Descriptive summary
  ↓
ANOVA / panel analysis
  ↓
Panel performance
  ↓
PCA
  ↓
Integrated QDA analysis
```

The corresponding R functions include:

```r
sensory_import()
sensory_validate()
sensory_clean()
sensory_summary()
sensory_anova()
sensory_posthoc()
sensory_panel_anova()
sensory_panel_performance()
sensory_panel_multi()
sensory_panel_plot()
sensory_pca()
sensory_pca_diagnostics()
sensory_pca_plot()
sensory_qda()
```

## Example dataset

SensoryToolsR includes a simulated replicated QDA dataset named `qda_example`.

The dataset contains:

* 6 products;
* 6 assessors;
* 3 sessions;
* 108 assessor-product-session observations;
* 8 sensory attributes.

Load it with:

```r
library(SensoryToolsR)

data(qda_example)

dim(qda_example)
head(qda_example)
```

The sensory attributes are:

```r
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
```

The dataset is simulated for demonstration, teaching, package examples, and testing. It does not represent measurements from human participants or a specific commercial product.

## Importing sensory data

CSV, XLS, and XLSX files can be imported with `sensory_import()`.

```r
sensory_data <- sensory_import(
  "sensory_data.csv"
)
```

By default, column names are cleaned to provide consistent names for subsequent analysis.

## Data validation

Before statistical analysis, the experimental structure can be checked using:

```r
validation <- sensory_validate(
  sensory_data
)
```

The validation procedure examines features such as:

* number of observations;
* number of assessors;
* number of products;
* number of sessions;
* sensory attributes;
* missing values;
* duplicate design records;
* expected versus observed assessor–product–session combinations;
* panel completeness.

A complete balanced structure can therefore be identified before further analysis.

## Data cleaning

Data can be cleaned using:

```r
clean_data <- sensory_clean(
  sensory_data
)
```

It is good practice to validate the data again after cleaning:

```r
sensory_validate(
  clean_data
)
```

## Descriptive sensory statistics

Product-level descriptive statistics can be generated with:

```r
summary_result <- sensory_summary(
  clean_data,
  attributes = c(
    "sweetness",
    "bitterness",
    "umami"
  )
)

summary_result
```

The output includes statistics such as sample size, mean, standard deviation, standard error, median, coefficient of variation, confidence intervals, minimum, and maximum where applicable.

## Sensory ANOVA

A sensory attribute can be analysed using:

```r
anova_result <- sensory_anova(
  clean_data,
  attribute = "sweetness"
)
```

The ANOVA result provides information about product and assessor effects.

The ANOVA table can be inspected with:

```r
anova_result$anova_table
```

## Post-hoc comparisons

When appropriate, product differences can be investigated using Tukey post-hoc comparisons:

```r
posthoc_result <- sensory_posthoc(
  clean_data,
  attribute = "sweetness"
)
```

## Panel ANOVA

For replicated panel data containing assessors, products, and sessions:

```r
panel_result <- sensory_panel_anova(
  clean_data,
  attribute = "sweetness"
)
```

The panel model evaluates effects including:

* product;
* assessor;
* session;
* Product × Assessor interaction.

The Product × Assessor interaction is particularly useful for investigating whether assessors use sensory attributes consistently across products.

## Panel performance

Individual assessor performance can be explored using:

```r
performance_result <- sensory_panel_performance(
  clean_data,
  attribute = "sweetness"
)
```

This functionality is intended to help identify assessor-level patterns that may require further review.

## Multi-attribute panel analysis

Several sensory attributes can be analysed together using:

```r
panel_multi_result <- sensory_panel_multi(
  clean_data,
  attributes = c(
    "sweetness",
    "bitterness",
    "umami",
    "firmness"
  )
)
```

This provides an overview of panel behaviour across the sensory profile rather than considering only one attribute at a time.

## Principal component analysis

PCA can be performed directly on the included `qda_example` dataset.

```r
library(SensoryToolsR)

data(qda_example)

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

pca_result <- sensory_pca(
  qda_example,
  attributes = attributes
)
```

Inspect the explained variance:

```r
pca_result$variance_table
```

For the packaged `qda_example` dataset, the first two principal components represent the dominant sensory structure.

A PCA biplot can be generated with:

```r
sensory_pca_plot(
  pca_result,
  type = "biplot"
)
```

PCA diagnostics can then be examined with:

```r
pca_diagnostics <- sensory_pca_diagnostics(
  pca_result,
  components = c(1, 2),
  top_n = 5
)

pca_diagnostics$top_attributes
pca_diagnostics$top_products
```

In this simulated dataset, PC1 primarily represents a taste/odor freshness–deterioration contrast, while PC2 primarily represents the firmness–juiciness texture contrast.


## PCA diagnostics

The sensory meaning of the principal components can be explored with:

```r
pca_diagnostics <- sensory_pca_diagnostics(
  pca_result,
  components = c(1, 2),
  top_n = 5
)
```

For example:

```r
pca_diagnostics$top_attributes
pca_diagnostics$top_products
```

These outputs help identify which sensory attributes and products contribute most strongly to each principal component.

## Integrated QDA workflow

The main analytical components can be combined using `sensory_qda()`.

```r
library(SensoryToolsR)

data(qda_example)

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

qda_result <- sensory_qda(
  qda_example,
  attributes = attributes
)
```

Printing the result provides a concise overview:

```r
qda_result
```

A structured summary can be obtained with:

```r
qda_summary <- summary(
  qda_result
)
```

Useful result components include:

```r
qda_result$overview
qda_result$validation
qda_result$summary
qda_result$panel
qda_result$pca
qda_result$pca_diagnostics
```

This allows users to move from an overall QDA summary to detailed statistical outputs.

## Example interpretation

A QDA study may reveal two complementary dimensions of sensory differentiation.

For example, PC1 might primarily distinguish products according to taste and odor attributes such as sweetness, bitterness, fresh odor, fishy odor, umami, and aftertaste, while PC2 may distinguish products mainly according to texture attributes such as firmness and juiciness.

The interpretation should always be based on the actual PCA loadings, contributions, product scores, and explained variance obtained from the dataset being analysed.

## Reproducible sensory analysis

SensoryToolsR is being developed with an emphasis on:

* transparent data checking;
* reproducible statistical analysis;
* interpretable outputs;
* trained-panel performance assessment;
* integrated QDA analysis;
* research-oriented sensory workflows.

The package is intended to complement, rather than replace, careful sensory experimental design and expert interpretation of panel results.

## Package status

SensoryToolsR is currently in active development.

Current development version:

```text
0.1.0
```

The API, output structures, and statistical functionality may change as the package develops.

## Contributing and issues

Suggestions, bug reports, and reproducible examples can be submitted through the GitHub repository:

`maingats2-ai/SensoryToolsR`

## License

SensoryToolsR is released under the MIT License.

## Author

**Mai Nga**

SensoryToolsR was developed to support reproducible statistical analysis and interpretation of sensory evaluation data.
