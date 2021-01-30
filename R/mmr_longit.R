#' Longitudinal Computation of the Meta-Memory Ratio (MMR) and Awareness of Cognitive Decline Categorization (ACDC)
#'
#' Performs a by visit regression on defined memory and complaint variables, and
#' standardized the residuals. After building composite memory and complaint
#' scores, the function compute both the MMR and the ACDC on this basis.
#'
#' This function is based on one other mmr functions: mmr_build

#' @param vis character (name of the 'visit' variable)
#' @param mem character vector (memory variables)
#' @param comp character vector (complaint variables)
#' @param vect.linear character vector (normally distributed variables)
#' @param vect.binomial character vector (variables with binomial distribution)
#' @param df data.frame
#'
#' @return data.frame. The return of the function corresponds to the previous
#'     data.frame (i.e. the one in the 'df' parameter) with the new computed values.
#' @export
#'
#' @examples
#'
#'
mmr_longit <- function(vis, mem, comp, vect.linear, vect.binomial, df) {

  df.split <- split(x = df, f = df[, vis])

  mmr <- purrr::map(.x = df.split, .f = function(x){
    mmr_build(mem = mem,
              comp = comp,
              vect.linear = vect.linear,
              vect.binomial = vect.binomial,
              df = x)
  })

  output <- do.call("rbind", mmr)
  rownames(output) <- NULL

  return(output)
}
