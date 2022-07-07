#' Ler linha
#'
#' @param dataframe Data frame
#' @param len Quantidade de colunas
#'
#' @return Data frame filtrado com os países selecionados, colunas renomeadas, e colunas em type numeric
#' @export


ler_linha <- function(dataframe, len){
  dataframe %>%
    renomear_disc() %>%
    selecionarPais() %>%
    mutate(dplyr::across(.cols=2:len, .fns=as.numeric))
}
