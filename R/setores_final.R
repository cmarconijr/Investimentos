#' Setores Final
#'
#' @param nomePlan Planilha
#' @param placeholder Segunda planilha para guardar os dados da primeira
#' @description  Soma os valores
#' @return Data frame contendo os setores selecionados e os valores dos países somados transformando em apenas uma linha
#' @export


# Provavelmente há modo melhor de se criar essa função
setores_final <- function(nomePlan, placeholder){
  colnames(nomePlan)[1] <- "Setores"
  n <- ncol(nomePlan)
  placeholder <- nomePlan %>% select(2:all_of(n))
  placeholder <- placeholder%>%
    mutate(Valores = rowSums(placeholder))
  n <- ncol(placeholder)
  nomePlan <- nomePlan[ , 1]
  placeholder <- placeholder[ , n]
  nomePlan <- bind_cols(nomePlan, placeholder)

}
