#' Example replicated QDA sensory dataset
#'
#' A simulated sensory dataset representing a replicated Quantitative
#' Descriptive Analysis (QDA) experiment. Six assessors evaluate six
#' products during three sessions using eight sensory attributes.
#'
#' The product profiles were designed to contain two interpretable
#' sensory dimensions. Taste and odor attributes primarily distinguish
#' products along a freshness/deterioration dimension, while firmness
#' and juiciness provide a largely independent texture dimension.
#'
#' Small assessor, session, and random observational effects are included
#' to provide realistic replicated sensory variation.
#'
#' @format A tibble with 108 rows and 11 variables:
#' \describe{
#'   \item{product}{Product identifier, P1 to P6.}
#'   \item{assessor}{Assessor identifier, A01 to A06.}
#'   \item{session}{Evaluation session, S1 to S3.}
#'   \item{sweetness}{Sweetness intensity score.}
#'   \item{bitterness}{Bitterness intensity score.}
#'   \item{umami}{Umami intensity score.}
#'   \item{fishy_odor}{Fishy odor intensity score.}
#'   \item{fresh_odor}{Fresh odor intensity score.}
#'   \item{aftertaste}{Aftertaste intensity score.}
#'   \item{firmness}{Firmness intensity score.}
#'   \item{juiciness}{Juiciness intensity score.}
#' }
#'
#' @details
#' The dataset is simulated for demonstration, teaching, package examples,
#' and testing. It does not represent measurements from human participants
#' or a specific commercial product.
#'
#' The design contains:
#'
#' \itemize{
#'   \item 6 products;
#'   \item 6 assessors;
#'   \item 3 sessions;
#'   \item 108 assessor-product-session observations;
#'   \item 8 sensory attributes.
#' }
#'
#' Because the dataset is generated using a fixed simulation design, it is
#' suitable for reproducible examples of panel analysis and PCA.
#'
#' @examples
#' data(qda_example)
#'
#' dim(qda_example)
#' head(qda_example)
#'
#' attributes <- c(
#'   "sweetness",
#'   "bitterness",
#'   "umami",
#'   "fishy_odor",
#'   "fresh_odor",
#'   "aftertaste",
#'   "firmness",
#'   "juiciness"
#' )
#'
#' \dontrun{
#' result <- sensory_qda(
#'   qda_example,
#'   attributes = attributes
#' )
#'
#' result
#' }
#'
#' @source Simulated dataset created for SensoryToolsR.
"qda_example"
