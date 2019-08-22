#' Compute the Meta-Memory Ratio (MMR) and Awareness of Cognitive Decline Categorization (ACDC)
#'
#' Performs a regression on defined memory and complaint variables, and
#' standardized the residuals. After building composite memory and complaint
#' scores, the function compute both the MMR and the ACDC on this basis.
#'
#'
#' @param mem character vector (memory variables)
#' @param comp character vector (complaint variables)
#' @param vect.linear character vector (normally distributed variables)
#' @param vect.binomial character vector (variables with binomial distribution)
#' @param df data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
mmr_build <- function(mem, comp, vect.linear, vect.binomial, df){
  reglin <- mmr_reglin(vect.linear, df)
  regbin <- mmr_regbin(vect.binomial, df)
  reglin <- reglin %>% dplyr::select(ID, contains(".res"))
  regbin <- regbin %>% dplyr::select(ID, contains(".res"))
  output <- df %>% dplyr::right_join(x = df, y = reglin, by = "ID")
  output <- output %>% dplyr::right_join(x = output, y = regbin, by = "ID")

  output <- output %>% dplyr::mutate_at(vars(contains(".res")),
                                 funs(as.vector(scale(x = .,
                                                      center = TRUE,
                                                      scale = TRUE))))
  output <- output %>%
    dplyr::mutate(Memory = rowMeans(select(., str_c(mem, ".res"))),
           Complaint = rowMeans(select(., str_c(comp, ".res"))),
           MMR = Memory + Complaint,
           ACDC = case_when(Complaint <= 0 & Memory >= 0 ~ "Normal",
                            Complaint > 0 & Memory >= 0 ~ "SCD",
                            Complaint <= 0 & Memory < 0 ~ "LowAwareness",
                            Complaint > 0 & Memory < 0 ~ "Amnestic"))
  return(output)
}
