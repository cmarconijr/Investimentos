#' Criar Tabela
#'
#' @param tabela Data frame
#' @param names Lista com os nomes das linhas
#'
#' @return Tabela ordenada com os valores dos países selecionados, e os nomes das linhas.
#' @export


criar_tabela <- function(tabela, names){
  tabela$names <- names
  tabela <- tabela %>%
    select(names, anos)
}
