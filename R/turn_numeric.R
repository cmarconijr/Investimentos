#' turn numeric
#'
#' @param dataframe Data frame
#' @param len quantidade de colunas
#'
#' @return Colunas no type numeric
#' @export


turn_numeric <- function(dataframe, len){
  dataframe %>% mutate(dplyr::across(.cols=2:len, .fns=as.numeric))
}
