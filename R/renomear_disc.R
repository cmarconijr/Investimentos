#' renomear Discriminação
#'
#' @param dataframe Data frame
#' @description  Renomeia coluna
#' @return Renomeia a coluna Discriminação por Pais
#' @export


renomear_disc <- function(dataframe){
  rename(dataframe, "Pais" = "Discriminação")
}

