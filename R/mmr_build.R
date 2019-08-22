mmr_build <- function(mem, comp, vect.linear, vect.binomial, df){
  reglin <- mmr.reglin(vect.linear, df)
  regbin <- mmr.regbin(vect.binomial, df)
  reglin <- reglin %>% select(ID, contains(".res"))
  regbin <- regbin %>% select(ID, contains(".res"))
  output <- df %>% right_join(x = df, y = reglin, by = "ID")
  output <- output %>% right_join(x = output, y = regbin, by = "ID")

  output <- output %>% mutate_at(vars(contains(".res")),
                                 funs(as.vector(scale(x = .,
                                                      center = TRUE,
                                                      scale = TRUE))))
  output <- output %>%
    mutate(Memory = rowMeans(select(., str_c(mem, ".res"))),
           Complaint = rowMeans(select(., str_c(comp, ".res"))),
           MMR = Memory + Complaint,
           ACDC = case_when(Complaint <= 0 & Memory >= 0 ~ "Normal",
                            Complaint > 0 & Memory >= 0 ~ "SCD",
                            Complaint <= 0 & Memory < 0 ~ "LowAwareness",
                            Complaint > 0 & Memory < 0 ~ "Amnestic"))
  return(output)
}
