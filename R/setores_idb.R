#' Setores IDB
#'
#' @param nomePlan Data frame
#'
#' @return Data frame contendo somente os setores e países selecionados para a criação da tabela de Setor de atividade econômica
#' @export


setores_idb <- function(nomePlan){
  nomePlan <- nomePlan[c(13,8,12,5,15), -2]
  nomePlan <- turn_numeric(nomePlan,77)
  nomePlan <- nomePlan %>%
    pivot_longer(cols = 2:77, names_to = "Pais", values_to = "valor")
  nomePlan[is.na(nomePlan)] <- 0

  nomePlan <- nomePlan%>%
    filter(Pais %in% pais)
  nomePlan <- nomePlan %>%
    pivot_wider(names_from = Pais,values_from = valor)

}
