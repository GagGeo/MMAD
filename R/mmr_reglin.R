#' Cleaning of normal variables
#'
#' This function apply a regression (using glm()) on normally distributed
#' variables. Then the residuals are added in the original data.frame
#'
#' @param vect.linear character vector (normally distributed variables)
#' @param df data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
mmr_reglin <- function(vect.linear, df) {

  df.temp <- df %>%
    tidyr::gather(key = "Test", value = "Score", vect.linear) %>%
    dplyr::group_by(Test) %>%
    tidyr::nest()

  regtest <- function(x){
    glm(formula = Score ~ Gender + Education + Age, data = x)
  }

  models <- purrr::map(df.temp$data, regtest)

  df.temp <-  df.temp %>%
    dplyr::mutate(mods = models,
           resids = purrr::map2(data, mods, add_residuals)) %>%
    tidyr::unnest(resids) %>% dplyr::select(-Score) %>%
    tidyr::spread(key = Test, value = resid)

  df.temp <- df.temp %>% dplyr::select(ID, vect.linear) %>%
    dplyr::rename_at(.vars = vect.linear,
                     .funs = function(x){paste(x, ".res", sep = "")})

  df.temp <- df %>% dplyr::right_join(x = ., y = df.temp, by = "ID")

  return(df.temp)
}
