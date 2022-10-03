#' @export
IDP_func <- function(dataframe, ano, coluna){
  dataframe$Ano <- ano
  dataframe$Ano <- as.numeric(dataframe$Ano)

  dataframe %>%
    rename("Setores" = coluna)
}



