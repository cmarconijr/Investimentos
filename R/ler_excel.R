#' Ler Excel
#'
#' @param planName Planilha
#' @param sht Aba da planilha
#' @param skp skip
#'
#' @return um objeto da classe data.frame, tbl, ou tbl_df
#' @export


ler_excel <- function(planName, sht, skp){
  read_excel(planName,
             sheet = sht, skip = skp)
}
