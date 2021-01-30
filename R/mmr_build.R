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
#' @param bl base = NA. character vector = name of the baseline visit
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
mmr_build <- function(mem, comp, vect.linear, vect.binomial, df, bl = NA){

  first_step <- function(vect.linear, vect.binomial, df) {
    reglin <- mmr_reglin(vect.linear, df)
    regbin <- mmr_regbin(vect.binomial, df)
    reglin <- reglin %>% dplyr::select(ID, dplyr::contains(".res"))
    regbin <- regbin %>% dplyr::select(ID, dplyr::contains(".res"))
    output <- df %>% dplyr::right_join(x = df, y = reglin, by = "ID")
    output <- output %>% dplyr::right_join(x = output, y = regbin, by = "ID")
    output
  }
  last_step <- function(x) {
    x %>%
      dplyr::mutate(Memory = rowMeans(select(., stringr::str_c(mem, ".res"))),
                    Complaint = rowMeans(select(., stringr::str_c(comp, ".res"))),
                    MMR = Memory + Complaint,
                    ACDC = case_when(Complaint <= 0 & Memory >= 0 ~ "Normal",
                                     Complaint > 0 & Memory >= 0 ~ "SCD",
                                     Complaint <= 0 & Memory < 0 ~ "LowAwareness",
                                     Complaint > 0 & Memory < 0 ~ "Amnestic"))
  }

  if (is.na(bl)) {
    output <- first_step(vect.linear, vect.binomial, df)
    output <- output %>% dplyr::mutate_at(vars(contains(".res")),
                                          funs(as.vector(scale(x = .,
                                                               center = TRUE,
                                                               scale = TRUE))))
    output <- last_step(x = output)
  } else {
    dfs_vis <- df %>% dplyr::group_split(Visit)

    output <- map(.x = dfs_vis, .f = function(x){
      output <- first_step(vect.linear, vect.binomial, df)
      z_scores <- map(.x = colnames(output)[str_detect(string = colnames(output),
                                                       pattern = ".res")],
                      .f = function(x){
                        x_var <- rlang::sym(x)
                        orgv <- str_remove(string = x, pattern = ".res")
                        output %>% mutate(
                          bl_mean = mean(filter(df, Visit == bl)[,orgv], na.rm = TRUE),
                          sd_mean = sd(filter(df, Visit == bl)[,orgv], na.rm = TRUE),
                          !!x_var := (!!x_var - bl_mean)/sd_mean) %>%
                          select(ID, Visit, x_var) %>% arrange(ID, Visit)
                      })
      df_z_scores <- bind_cols(z_scores, .name_repair = "minimal") %>% .[,!duplicated(names(.))]
      output <- left_join(x = output, y = df_z_scores, c("ID", "Visit")) %>%
        select(!contains(".x")) %>% `colnames<-`(str_remove_all(string = colnames(.), pattern = ".y"))

      output <- last_step(x = output)
    })
    output <- bind_rows(output)
  }
  return(output)
}
