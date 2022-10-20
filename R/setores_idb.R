#' Setores IDB
#'
#' @param nomePlan Data frame
#'
#' @return Data frame contendo somente os setores e países selecionados para a criação da tabela de Setor de atividade econômica
#' @export

# Função atualizada em 17/10/2022
setores_idb <- function(nomePlan){
  nomePlan <- nomePlan[c(13,8,12,5,15), -1]
  nomePlan <- bind_cols(setores_idb_df, nomePlan)

  nomePlan <- turn_numeric(nomePlan,78)
  nomePlan[is.na(nomePlan)] <- 0

  nomePlan <- nomePlan %>%
    pivot_longer(cols = 2:78, names_to = "Pais", values_to = "valor")

  nomePlan <- nomePlan%>%
    filter(Pais %in% pais)

  nomePlan <- nomePlan %>%
    pivot_wider(names_from = Pais,values_from = valor)

}









