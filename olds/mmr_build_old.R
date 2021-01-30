# mmr_build old version
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
