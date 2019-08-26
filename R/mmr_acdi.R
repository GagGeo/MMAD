#' Compute the ACDI (Awareness for Cognitive Decline Index)
#'
#' This index is based on the Participant-Informant Discrepancy. It computes
#' the difference between the participant's appraisal of his/her cognition
#' and the informant's.
#'
#' @param self character (variable name for participant's complaint score)
#' @param ext character (variable name for informant's complaint score)
#' @param df data.frame
#' @param lim numeric (defined limits for group delineation. If unspecified,
#'     lim = 1.5)
#'
#' @return data.frame. The return of the function corresponds to the previous
#'     data.frame (i.e. the one in the 'df' parameter) with the new computed values.
#' @export
#'
#' @examples
#'
mmr_acdi <- function(self, ext, df, lim = 1.5) {

  df <- df %>% dplyr::mutate(ACDI = df[,self] - df[,ext])
  df <- df %>%
    dplyr::mutate(ACDI_z = (ACDI - mean(df$ACDI, na.rm = TRUE) /
                       sd(df$ACDI, na.rm = TRUE)),
           ACDI_gp = ifelse(ACDI_z < -lim, "LowAwareness",
                            ifelse(ACDI_z > lim, "SCD", "Accurate")))
  return(df)

}
