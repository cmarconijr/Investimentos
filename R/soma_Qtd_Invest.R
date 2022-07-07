#' Soma de Quantidade de Investidores
#'
#' @param nlinha Data frame
#' @param len Quantidade de colunas
#' @param ano Anos
#'
#' @return Data frame contendo o somatório da quantidade de investidores dos países
#' @export


soma_Qtd_Invest <- function(nlinha,len, ano){
  nlinha[is.na(nlinha)] <- 0
  soma <- apply(nlinha[,2:len], 2, FUN=sum)
  def <- data_frame(ano, soma)
  def <- def %>%
    pivot_wider(names_from = ano, values_from = soma)
}
