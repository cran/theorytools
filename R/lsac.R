#' Synthetic Data: Longitudinal Study of Australian Children
#'
#' This synthetic dataset, based on
#' "Growing Up in Australia - the Longitudinal Study of Australian Children" (LSAC).
#' This longitudinal study covers a representative sample of about 10.000
#' children and their families, and aims to examine children's development
#' from early childhood through to adolescence and adulthood.
#' All variables pertain to the mother; note that fathers also play an important
#' and sometimes unique role in children's emotional development.
#'
#' Except for "coping", all variables were created by taking the row means of
#' several items, omitting missing values. The corresponding item names from the
#' LSAC codebook are given below.
#'
#' \tabular{llll}{
#'   \strong{warmth} \tab \code{numeric} \tab \code{fpa03m1-fpa03m6} \tab Parental warmth scale\cr
#'   \strong{relationship_quality} \tab \code{numeric} \tab \code{fre04m1-fre04m7} \tab Hendrick relationship quality scale\cr
#'   \strong{temperament_negreact} \tab \code{numeric} \tab \code{fse13a1-fse13a4} \tab Temperament scale for reactivity\cr
#'   \strong{emotion_regulation} \tab \code{numeric} \tab \code{fse03c3a-fse03c3e} \tab SDQ Emotional problems scale\cr
#'   \strong{social_functioning} \tab \code{numeric} \tab \code{fgd04b2a-fgd04b2e} \tab Peds QL social functioning\cr
#'   \strong{coping} \tab \code{numeric} \tab \code{fhs26m2} \tab Level of coping
#' }
#' @docType data
#' @keywords datasets
#' @name lsac
#' @usage data(lsac)
#' @references Australian Institute of Family Studies. (2020, March 23). Growing Up in Australia.
#' @format A data frame with 8214 rows and 6 variables.
"lsac"
