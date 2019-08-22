#' Cleaning of binomial variables
#'
#' This function apply a regression (using glm()) on variables with
#' binomial distribution. Then the residuals are added in the original data.frame
#'
#' @param vect.binomial character vector (variables with binomial distribution)
#' @param df data.frame()
#'
#' @return data.frame()
#' @export
#'
#' @examples
mmr_regbin <- function(vect.binomial, df){

  df.temp <- df %>% tidyr::gather(key = "Test", value = "Score",
                           unlist(vect.binomial[,1])) %>%
    dplyr::group_by(Test) %>% tidyr::nest()

  regtest <- function(mesure, vect, df){
    bin.max <- vect.binomial[which(vect.binomial == mesure), 2][[1]]

    reg.mesure <- paste("cbind(",
                        "Score", ", ",
                        bin.max, " - ",
                        "Score", ")", sep = "")

    reg.formula <- as.formula(
      paste(reg.mesure,
            " ~  Gender + Education + Age", sep = ""))

    reg.comp <- glm(formula = reg.formula, data = df, family = "binomial")

    return(reg.comp)
  }

  models <- purrr::map2(df.temp$Test, df.temp$data, function(x, y){
    regtest(mesure = x[[1]], vect = vect.binomial, df = y)})

  df.temp <- df.temp %>%
    mutate(mods = models,
           resids = purrr::map2(data, mods, modelr::add_residuals))

  df.temp <- df.temp %>% dplyr::mutate(resids = purrr::map(resids, function(x){
    x %>% dplyr::mutate(resid = as.list(x[, 'resid'])$resid[,1])
  }))

  df.temp <- df.temp %>% tidyr::unnest(resids) %>%
    dplyr::select(-Score) %>% tidyr::spread(key = Test, value = resid)

  df.temp <- df.temp %>% dplyr::select(ID, as.list(vect.binomial[,1])$test) %>%
    dplyr::rename_at(.vars = unlist(vect.binomial[,1]), .funs = function(x){
      paste(x, ".res", sep = "")})

  df.temp <- df.mmr %>% dplyr::right_join(x = ., y = df.temp, by = "ID")

  return(df.temp)
}
