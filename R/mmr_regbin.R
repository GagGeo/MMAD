mmr_regbin <- function(vect.binomial, df){

  df.temp <- df %>% gather(key = "Test", value = "Score",
                           unlist(vect.binomial[,1])) %>%
    group_by(Test) %>% nest

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

  models <- map2(df.temp$Test, df.temp$data, function(x, y){
    regtest(mesure = x[[1]], vect = vect.binomial, df = y)})

  df.temp <- df.temp %>%
    mutate(mods = models,
           resids = map2(data, mods, add_residuals))

  df.temp <- df.temp %>% mutate(resids = map(resids, function(x){
    x %>% mutate(resid = as.list(x[, 'resid'])$resid[,1])
  }))

  df.temp <- df.temp %>% unnest(resids) %>%
    select(-Score) %>% spread(key = Test, value = resid)

  df.temp <- df.temp %>% select(ID, as.list(vect.binomial[,1])$test) %>%
    rename_at(.vars = unlist(vect.binomial[,1]), .funs = function(x){
      paste(x, ".res", sep = "")})

  df.temp <- df.mmr %>% right_join(x = ., y = df.temp, by = "ID")

  return(df.temp)
}
