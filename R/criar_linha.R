# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

criar_linha <- function(nomelinha, nomeTab,lin){
  nomeTab <- select (nomeTab,-c("names","2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","2019"))
  linhas <- c(lin)
  nomelinha <- nomelinha[linhas,]

}

