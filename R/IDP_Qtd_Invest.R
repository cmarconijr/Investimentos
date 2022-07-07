#' Quantidade de Investidores IDP
#'
#' @param nomePlan Planilha
#' @param ano Anos
#'
#' @return Data frame com os dados referente a quantidade de investidores dos paises selecionados
#' @export
#'

IDP_Qtd_Invest <- function(nomePlan, ano){
  nomePlan %>%
    rename("Pais" = "Discrimina\\u00e7\\u00e3o") %>%
    select(Pais, all_of(ano)) %>%
    filter(Pais %in% pais) %>%
    na.omit() %>%
    arrange_all() %>%
    mutate(dplyr::across(.cols=2:3, .fns=as.numeric))

}
