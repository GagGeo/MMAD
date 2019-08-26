#' Compute the ACDI (Awareness for Cognitive Decline Index)
#'
#' This index is based on the Participant-Informant Discrepancy. It computes
#' the difference between the participant's appraisal of his/her cognition
#' and the informant's.
#'
#' @param self character (variable name for participant's complaint score)
#' @param ext character (variable name for informant's complaint score)
#' @param vis character (name of the 'visit' variable)
#' @param df data.frame
#' @param lim numeric (defined limits for group delineation. If unspecified,
#'     lim = 1.5)
#'
#' @return data.frame. The return of the function corresponds to the previous
#'     data.frame (i.e. the one in the 'df' parameter) with the new computed values.
#' @export
#'
#' @examples
mmr_acdilong <- function(self, ext, vis, df, lim = 1.5) {

  df.split <- split(x = df, f = df[, vis])

  acdi <- purrr::map(.x = df.split, .f = function(x){
    mmr_acdi(self, ext, df = x, lim)
  })

  output <- do.call("rbind", acdi)
  rownames(output) <- NULL

  return(output)
}
