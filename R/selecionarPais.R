#' selecionarPais
#'
#' @param dataFrame data frame
#' @description Países selecionados
#' @return um data frame filtrado por país e com os anos que foram selecionados
#' @export

# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


selecionarPais <- function(dataFrame) {

  dataFrame %>%
    filter(Pais %in% pais) %>%
    select("Pais", anos) %>%
    arrange_all()

}
