#' Outros
#'
#' @param df_por_setor Data frame que contém os dados do Setor de atividade econômica
#' @param plan3_reference Referência à celula (L3 OU L4) da aba 3 da planilha de Investimentos
#'
#' @return Célula 'outros' da tabela do Setor de atividade econômica
#' @export


outros_func<- function(df_por_setor, plan3_reference){
  outros <- apply(df_por_setor[,2], 2, FUN=sum)
  outros <- data.frame(outros)
  outros2 <- plan3_reference$`2020`
  outros2 <- data.frame(outros2)
  outros <- outros2 - outros

  outros$setor_outros <- c("Outros")
  outros <- outros %>%
    select(setor_outros, outros2)
}




