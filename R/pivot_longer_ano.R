#' @export
pivot_longer_ano <- function(dataframe, pais1, pais2){
  dataframe %>%
    pivot_longer(
      cols = pais1:pais2,
      names_to = "pais",
      values_to = "valor")
}


