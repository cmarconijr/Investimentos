#' setores
#'
#' @param nomePlan Planilha
#'
#' @return Data frame contendo os setores selecionados e os valores de cada país
#' @export


setores <- function(nomePlan){
  nomePlan <- nomePlan[c(4,5,6,8,9,12), -2]
  nomePlan <- turn_numeric(nomePlan,35)
  nomePlan <- nomePlan %>%
    pivot_longer(cols = 2:35, names_to = "Pais", values_to = "valor")
  nomePlan[is.na(nomePlan)] <- 0



  nomePlan <- nomePlan%>%
    filter(Pais %in% pais)
  nomePlan <- nomePlan %>%
    pivot_wider(names_from = Pais,values_from = valor)

}
