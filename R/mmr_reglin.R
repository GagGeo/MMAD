mmr_reglin <- function(vect.linear, df) {

  df.temp <- df %>% gather(key = "Test", value = "Score", vect.linear) %>%
    group_by(Test) %>% nest

  regtest <- function(x){
    glm(formula = Score ~ Gender + Education + Age, data = x)
  }

  models <- map(df.test$data, regtest)

  df.temp <-  df.temp %>%
    mutate(mods = models,
           resids = map2(data, mods, add_residuals)) %>%
    unnest(resids) %>% select(-Score) %>%
    spread(key = Test, value = resid)

  df.temp <- df.temp %>% select(ID, vect.linear) %>%
    rename_at(.vars = vect.linear, .funs = function(x){paste(x, ".res", sep = "")})

  df.temp <- df %>% right_join(x = ., y = df.temp, by = "ID")

  return(df.temp)
}
