#' @export

bloco_paises <- function(bloco) {

  lista_blocos <- read_csv("bloco_paises.csv") %>%
    dplyr::distinct(no_bloco) %>%
    dplyr::pull(no_bloco)

  if (bloco %in% lista_blocos) {
    paises <- lista_blocos %>%
      dplyr::filter(no_bloco == bloco) %>%
      dplyr::distinct(no_pais) %>%
      dplyr::pull(no_pais)
    return(paises)
  }

  else {
    print("Bloco n\u00e3o identificado, tente ajustar o grupo de acordo com a lista.")
  }

}




