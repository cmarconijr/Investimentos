
#'@export
  gerar_investimentos <- function(lista_paises) {
    rmarkdown::render(system.file("rmd", "Investimentos_Relatorios.Rmd", package = "Investimentos"),
                      params = list(
                        title = paste0("Investimento Brasil - ", lista_paises),
                        paises = pais_port
                      ),
                      output_dir = here::here("Home/investimentos/Modelo-Automatico-investimentos/pacote_investimentos/Investimentos/Relatorios Prontos"),
                      output_file = paste0("investimento_", pais_port))
  }




  library(Investimentos)
  purrr::walk(Investimentos::gerar_investimentos)

  Investimentos:::criar_linha()

#================================================================================================================================














