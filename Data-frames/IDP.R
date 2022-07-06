#Código para pegar dados do IDE


library(magrittr)
library(tidyverse)
library(readxl)
library(dplyr)


# Baixa a Planilha

httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/TabelasCompletasPosicaoIDP.xlsx",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "TabelasCompletasPosicaoIDP.xlsx"), overwrite = T))

# Carrega a página 5 da Planilha IDP
PAG_IDP_Invest_Imediato <- read_xlsx("data-raw/TabelasCompletasPosicaoIDP.xlsx",
                      sheet = "5", skip = 4, col_types = c("text", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric")
)

# Carrega a página 6 da Planilha IDP
PAG_IDP_Control_Final <- read_xlsx("data-raw/TabelasCompletasPosicaoIDP.xlsx",
                      sheet = "6", skip = 4, col_types = c("text", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric")
)

# Carrega a página 7 da Planilha IDP
PAG_IDP_Oper_Intercomp <- read_xlsx("data-raw/TabelasCompletasPosicaoIDP.xlsx",
                      sheet = "7", skip = 4, col_types = c("text", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric")
)


# Lista de países IDP
lista_paises_IDP <- c("Países Baixos", "Estados Unidos", "Espanha", "Luxemburgo", "França", "Reino Unido",
                      "Japão", "Canadá", "Chile", "Ilhas Virgens Britânicas", "Suíça", "Alemanha", "Noruega",
                      "Cingapura", "Itália", "México", "Ilhas Cayman", "Bermudas", "Coréia do Sul", "Ilhas Jersey",
                      "Bélgica", "Suécia", "Bahamas", "Portugal", "Uruguai", "Irlanda", "Austrália", "Panamá", "Colômbia",
                      "Dinamarca", "China", "Curaçao", "Argentina", "Áustria", "Finlândia", "Hong Kong", "Malta",
                      "Israel", "Chipre", "Emirados Árabes Unidos", "África do Sul", "Nova Zelândia", "Maurício", "Peru",
                      "Angola", "Barbados", "Índia", "Uruguai", "Indonésia", "Israel", "Bahrein", "Tailândia", "Taiwan",
                      "Guernesey", "Catar", "Angola", "Rússia", "Ilha de Man", "Suíça", "Samoa", "Hungria", "Mônaco",
                      "São Vicente e Granadinas", "Eslováquia", "Aruba", "Trinidad e Tobago", "Demais")

lista_paises_IDP_Setor_Inv_Imed <- c("Países Baixos", "Estados Unidos", "Espanha", "Luxemburgo", "França",
                       "Reino Unido","Japão", "Canadá", "Chile", "Ilhas Virgens Britânicas",
                       "Suíça", "Alemanha", "Noruega","Cingapura", "Itália", "México", 
                       "Ilhas Cayman", "Bermudas", "Coréia do Sul", "Ilhas Jersey",
                        "Bélgica", "Suécia", "Bahamas", "Portugal", "Uruguai", "Irlanda",
                       "Dinamarca", "China", "Curaçao", "Argentina", "Áustria", "Finlândia")

lista_paises_IDP_Setor_Control_Final <- c("Estados Unidos", "Espanha", "França", "Bélgica",
                                          "Reino Unido", "China", "Países Baixos", "Japão",
                                          "Alemanha", "Suíça", "Luxemburgo", "Canadá", "Brasil",
                                          "Itália", "Ilhas Cayman", "Portugal", "Cingapura", "Noruega", 
                                          "Bermudas", "México", "Coréia do Sul", "Chile", "Suécia", 
                                          "Irlanda", "Austrália", "Ilhas Virgens Britânicas", "Índia",
                                          "Colômbia", "África do Sul", "Uruguai", "Argentina", "Áustria",
                                          "Panamá", "Dinamarca", "Indonésia")

# Transforma a lista de países da Planilha IDP em um df 
df_paises_IDP <- data.frame(lista_paises_IDP)

# Lista com os anos
anos <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020")

PAG_IDP_Invest_Imediato <- rename(PAG_IDP_Invest_Imediato, "Pais" = "Discriminação")
PAG_IDP_Control_Final <- rename(PAG_IDP_Control_Final, "Pais" = "Discriminação")
PAG_IDP_Oper_Intercomp <- rename(PAG_IDP_Oper_Intercomp,  "Pais" = "Discriminação")

#Filtra as páginas do df
DF_IDP_Invest_Imediato <- PAG_IDP_Invest_Imediato %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDP_Invest_Imediato")


# Omite os NA's no df 
DF_IDP_Invest_Imediato <- na.omit(DF_IDP_Invest_Imediato)

# Organiza o df por ordem alfabética
DF_IDP_Invest_Imediato <- arrange_all(DF_IDP_Invest_Imediato)

#----------------------------------------  // ---------------------------------------- 

#Página 6
DF_IDP_Control_Final <- PAG_IDP_Control_Final %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDP_Control_Final")




DF_IDP_Control_Final <- na.omit(DF_IDP_Control_Final)
DF_IDP_Control_Final <- arrange_all(DF_IDP_Control_Final)

#Página 7
DF_IDP_Oper_Intercomp <- PAG_IDP_Oper_Intercomp %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDP_Oper_Intercomp")



DF_IDP_Oper_Intercomp <- na.omit(DF_IDP_Oper_Intercomp)
DF_IDP_Oper_Intercomp <- arrange_all(DF_IDP_Oper_Intercomp)

#-------------------------------- // -------------------------------------------

DF_Geral <- full_join(DF_Geral,DF_IDP_Invest_Imediato, by = c("Pais", "Ano"))
DF_Geral <- full_join(DF_Geral, DF_IDP_Control_Final, by = c("Pais", "Ano"))
DF_Geral <- full_join(DF_Geral,DF_IDP_Oper_Intercomp, by = c("Pais", "Ano"))

# Quadro de pizza 

PAG_IDP_Por_Setor_Inv_Imed <- read_xlsx("data-raw/TabelasCompletasPosicaoIDP.xlsx",
                               sheet = "13", range = "A5:AJ20", col_types = c("text", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", "numeric", "numeric")
)

anos_extenso <- c("ANO: 2020", "ANO: 2019", "ANO: 2018", "ANO: 2017", "ANO: 2016",
                  "ANO: 2015", "ANO: 2014", "ANO: 2013", "ANO: 2012", "ANO: 2011", "ANO: 2010")

DF_IDP_Por_Setor_Inv_Imed  <- PAG_IDP_Por_Setor_Inv_Imed %>%
  select("ANO: 2020", lista_paises_IDP_Setor_Inv_Imed)%>%
  pivot_longer(cols = lista_paises_IDP_Setor_Inv_Imed, names_to = "Pais", values_to = "IDP_Por_Setor_Inv_Imed ")

DF_IDP_Por_Setor_Inv_Imed <- na.omit(DF_IDP_Por_Setor_Inv_Imed)

# Organiza o df por ordem alfabética
DF_IDP_Por_Setor_Inv_Imed <- arrange_all(DF_IDP_Por_Setor_Inv_Imed)

#-------------------------------- // -------------------------------------------

PAG_IDP_Por_Setor_Control_Final <- read_xlsx("data-raw/TabelasCompletasPosicaoIDP.xlsx",
                                        sheet = "14", range = "A5:AJ20", col_types = c("text", "numeric", 
                                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", "numeric")
)

DF_IDP_Por_Setor_Control_Final  <- PAG_IDP_Por_Setor_Control_Final %>%
  select("ANO: 2020", lista_paises_IDP_Setor_Control_Final)%>%
  pivot_longer(cols = lista_paises_IDP_Setor_Control_Final, names_to = "Pais", values_to = "IDP_Setor_Control_Final")

DF_IDP_Por_Setor_Control_Final <- na.omit(DF_IDP_Por_Setor_Control_Final)

# Organiza o df por ordem alfabética
DF_IDP_Por_Setor_Control_Final <- arrange_all(DF_IDP_Por_Setor_Control_Final)

