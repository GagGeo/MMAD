#' mmr: A package for computating awareness measures for preclinical AD cohorts
#'
#' The mmr package provides a function that allows to compute three measures of
#' awareness: the Meta-Memory Ratio (MMR), the Awareness of Cognitive
#' Decline Categorization (ACDC), and the Awareness of Cognitive Decline Index
#' (ACDI). All of the measures can be computed either for a unique visit or
#' longitudinally.
#'
#' @section Mmr functions:
#' The Participant-Performance Discrepancy :
#'
#' mmr_build: This function take as input a formated data.frame, as well as
#'     some mandatory entries (i.e. memory variables, complaint variables,
#'     vector of variables that have to be treated as normal or binomial).
#'     The output is the same data.frame, with the addition of the computed
#'     measure (i.e. MMR and ACDC).
#'     The mmr_build function is based on the mmr_reglin and mmr_regbin
#'     functions.
#'
#' mmr_acdilong: This function is based on the mmr_build. It takes one more
#'     entry, i.e. the visit variable name in order to compute the measures
#'     within each visits.
#'
#' mmr_reglin: This function perform the prerequisite operations that allows the
#'     further computation of the MMR and ACDC. It stands for normally
#'     distributed variables.
#'
#' mmr_regbin: This function is comparable to the mmr_reglin function but stands
#'     for variables that have to be treated as binomial.
#'
#' The Participant-Informant Discrepancy :
#'
#' mmr_acdi: This function take as input a formated data.frame, some mandatory
#'     (i.e. self appraisal, informant appraisal) and optional entries (i.e.
#'     the delineation limit for ACDI-based groups in terms of standard-
#'     deviation).
#'     The output is the same data.frame, with the addition of the computed
#'     measure (i.e. ACDI, ACDI_z, ACDI_gp).
#'
#' mmr_acdilong: This function is based on the mmr_acdi. It takes one more
#'     entry, i.e. the visit variable name in order to compute the measures
#'     within each visits.

#' @docType package
#' @name mmr
NULL
#> NULL
