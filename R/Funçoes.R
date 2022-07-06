library(readxl)


#'@export
selecionarPais <- function(dataFrame){

  dataFrame %>%
    filter(Pais %in% pais) %>%
    select("Pais", anos) %>%
    arrange_all()

}


#'@export
ler_excel <- function(planName, sht, skp){
  read_excel(planName,
             sheet = sht, skip = skp)
}


#'@export
turn_numeric <- function(planName, len){
  planName %>% mutate(dplyr::across(.cols=2:len, .fns=as.numeric))
}


#'@export
renomear_disc <- function(dataframe){
  rename(dataframe, "Pais" = "Discriminação")
}


#'@export
ler_linha <- function(nomePlan,len){
  nomePlan %>%
    renomear_disc() %>%
    selecionarPais() %>%
    mutate(dplyr::across(.cols=2:len, .fns=as.numeric))
}


#'@export
soma_linhas <- function(nlinha){
  nlinha[is.na(nlinha)] <- 0
  soma <- apply(nlinha[,2:12], 2, FUN=sum)
  def <- data_frame(anos, soma)
  def <- def %>%
    pivot_wider(names_from = anos, values_from = soma)
}


#'@export
criar_tabela <- function(tabela, names){
  tabela$names <- names
  tabela <- tabela %>%
    select(names, anos)
}


#'@export
setores <- function(nomePlan){
  nomePlan <- nomePlan[c(4,8,6,5,12,9), -2]
  nomePlan <- turn_numeric(nomePlan,35)
  nomePlan <- nomePlan %>%
    pivot_longer(cols = 2:35, names_to = "Pais", values_to = "valor")
  nomePlan[is.na(nomePlan)] <- 0



  nomePlan <- nomePlan%>%
    filter(Pais %in% pais)
  nomePlan <- nomePlan %>%
    pivot_wider(names_from = Pais,values_from = valor)

}


#'@export
setores_final <- function(nomePlan, placeholder){
  colnames(nomePlan)[1] <- "Setores"
  n <- ncol(nomePlan)
  placeholder <- nomePlan %>% select(2:all_of(n))
  placeholder <- placeholder%>%
    mutate(Valores = rowSums(placeholder))
  n <- ncol(placeholder)
  nomePlan <- nomePlan[ , 1]
  placeholder <- placeholder[ , n]
  nomePlan <- bind_cols(nomePlan, placeholder)
}


#'@export
criar_linha <- function(nomelinha, nomeTab,lin){
  nomeTab <- select (nomeTab,-c("names","2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018","2019"))
  linhas <- c(lin)
  nomelinha <- nomelinha[linhas,]

}

#'@export
IDP_Qtd_Invest <- function(nomePlan, ano){
  nomePlan %>%
    rename("Pais" = "Discriminação") %>%
    select(Pais, all_of(ano)) %>%
    filter(Pais %in% pais) %>%
    na.omit() %>%
    arrange_all() %>%
    mutate(dplyr::across(.cols=2:3, .fns=as.numeric))

}

#'@export
soma_Qtd_Invest <- function(nlinha,len, ano){
  nlinha[is.na(nlinha)] <- 0
  soma <- apply(nlinha[,2:len], 2, FUN=sum)
  def <- data_frame(ano, soma)
  def <- def %>%
    pivot_wider(names_from = ano, values_from = soma)
}

#'@export
outros_func<- function(df_por_setor, plan3_reference){
  outros <- apply(df_por_setor[,2], 2, FUN=sum)
  outros <- data.frame(outros)
  outros2 <- plan3_reference$`2020`
  outros2 <- data.frame(outros2)
  outros <- outros2 - outros

  outros$setor_outros <- c("Outros")
  outros <- outros %>%
    select(setor_outros, outros2)
}

#'@export
setores_idb <- function(nomePlan){
  nomePlan <- nomePlan[c(13,8,12,5,15), -2]
  nomePlan <- turn_numeric(nomePlan,77)
  nomePlan <- nomePlan %>%
    pivot_longer(cols = 2:77, names_to = "Pais", values_to = "valor")
  nomePlan[is.na(nomePlan)] <- 0



  nomePlan <- nomePlan%>%
    filter(Pais %in% pais)
  nomePlan <- nomePlan %>%
    pivot_wider(names_from = Pais,values_from = valor)

}


