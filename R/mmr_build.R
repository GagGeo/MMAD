#' Compute the Meta-Memory Ratio (MMR) and Awareness of Cognitive Decline Categorization (ACDC)
#'
#' Performs a regression on defined memory and complaint variables, and
#' standardized the residuals. After building composite memory and complaint
#' scores, the function compute both the MMR and the ACDC on this basis.
#'
#' This function is based on two other mmr functions: mmr_regbin and mmr_reglin
#'
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
#' @seealso \code{\link{mmr_reglin}} for normally distributed variables
#' @seealso \code{\link{mmr_regbin}} for variables with binomial distribution
#'
#' @examples
#'
#'
mmr_build <- function(mem, comp, vect.linear, vect.binomial, df){
  reglin <- mmr_reglin(vect.linear, df)
  regbin <- mmr_regbin(vect.binomial, df)
  reglin <- reglin %>% dplyr::select(ID, dplyr::contains(".res"))
  regbin <- regbin %>% dplyr::select(ID, dplyr::contains(".res"))
  output <- df %>% dplyr::right_join(x = df, y = reglin, by = "ID")
  output <- output %>% dplyr::right_join(x = output, y = regbin, by = "ID")

  output <- output %>% dplyr::mutate_at(vars(contains(".res")),
                                 funs(as.vector(scale(x = .,
                                                      center = TRUE,
                                                      scale = TRUE))))
  output <- output %>%
    dplyr::mutate(Memory = rowMeans(select(., stringr::str_c(mem, ".res"))),
           Complaint = rowMeans(select(., stringr::str_c(comp, ".res"))),
           MMR = Memory + Complaint,
           ACDC = case_when(Complaint <= 0 & Memory >= 0 ~ "Normal",
                            Complaint > 0 & Memory >= 0 ~ "SCD",
                            Complaint <= 0 & Memory < 0 ~ "LowAwareness",
                            Complaint > 0 & Memory < 0 ~ "Amnestic"))
  return(output)
}
