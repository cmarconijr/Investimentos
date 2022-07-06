#Código para pegar dados do IDE
library(dplyr)
library(magrittr)
library(readxl)
library(tidyverse)


# Baixa a Planilha
httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/TabelasCompletasPosicaoIDE.xlsx",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "TabelasCompletasPosicaoIDE.xlsx"), overwrite = T))

# Lista de países IDE
lista_paises_IDE <- c("Ilhas Virgens Britânicas", "Panamá", "Bahamas", "Suíça", "Ilhas Cayman",
                      "Estados Unidos", "Luxemburgo", "Liechtenstein", "Portugal", "Belize",
                      "Uruguai", "Reino Unido", "Nova Zelândia", "São Vicente e Granadinas", "Ilhas Jersey",
                      "Alemanha", "Bermudas","Israel", "Países Baixos",
                      "França", "Cingapura", "Curaçao", "Canadá",
                      "Guernesey", "Anguila", "Itália", "Costa Rica",
                      "Paraguai", "Ilha de Man", "Ilhas Marshall", "Ilhas Virgens (EUA)", "Maurício",
                      "Mônaco", "Hong Kong", "Ilhas São Cristóvão e Neves", "Andorra",
                      "Bélgica", "Líbano", "Emirados Árabes Unidos", "México",
                      "Rússia", "Samoa", "Porto Rico", "Argentina",
                      "Antigua e Barbuda", "Coréia do Sul", "Malta",
                      "Áustria", "Espanha","Chile",
                      "Colômbia","Peru", "Hungria", "República Dominicana","Venezuela",
                      "Suécia","Angola","Gibraltar","Ilhas Turcas e Caicos",
                      "Irlanda", "Seychelles", "Austrália","China", "Japão",
                      "Guatemala", "Cuba", "Bolívia", "África do Sul",
                      "Ilhas Menores Distantes dos EUA", "Tailândia", "Equador", "Aruba","Turquia",
                      "Gana", "El Salvador","Noruega","Santa Lúcia", "Índia",
                      "Moçambique", "Honduras", "Grécia","Dinamarca", "Taiwan",
                      "Egito","Polônia", "Romênia", "República Tcheca", "Ilhas Lebuan",
                      "Antilhas Holandesas", "Bahrein", "Eslováquia", "Eslovênia","Zâmbia",
                      "Barbados","Argélia","Malásia","Marrocos", "Nigéria",
                      "Trinidad e Tobago","Finlândia","Indonésia","Congo","Chipre",
                      "Guiné Equatorial","Cabo Verde","Macau", "Demais","Não alocado")

lista_paises_IDE_Por_Setor <- c("Países Baixos", "Ilhas Cayman", "Ilhas Virgens Britânicas",
                            "Bahamas", "Estados Unidos", "Luxemburgo", "Áustria", "Panamá", "Espanha", "Reino Unido", 
                            "Chile", "Bermudas", "Uruguai", "Portugal", "Argentina", "Colômbia", "França", "Bélgica",
                            "Suíça", "Paraguai", "México", "Canadá", "Peru", "Hungria", "República Dominicana",
                            "Venezuela", "Suécia", "Curaçao", "Angola", "Itália", "Gibraltar", "Belize", "Alemanha", 
                            "Ilhas Turcas e Caicos", "Nova Zelândia", "Liechtenstein", "Ilhas Virgens (EUA)", "Irlanda",
                            "Seychelles", "Austrália", "China", "Japão", "Anguila", "São Vicente e Granadinas", 
                            "Ilhas Jersey", "Ilhas São Cristóvão e Neves", "Cingapura", "Hong Kong", "Guatemala",
                            "Guernesey", "Cuba", "Bolívia", "África do Sul", "Costa Rica", "Israel", "Malta", 
                            "Ilhas Menores Distantes dos EUA", "Tailândia", "Equador", "Aruba", "Ilhas Marshall",
                            "Maurício", "Ilha de Man", "Turquia", "Antigua e Barbuda", "Gana", "El Salvador", 
                            "Emirados Árabes Unidos", "Líbano", "Noruega", "Santa Lúcia", "Índia", "Moçambique",
                            "Honduras", "Grécia", "Dinamarca", "Coréia do Sul")



# Realiza leitura da página 3 da Planilha IDE 
PAG_IDE_Invest_Imediato <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                                        sheet = "3", skip = 4, col_types = c("text", "numeric", "numeric",
                                                                             "numeric", "numeric", "numeric",
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric",
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric",
                                                                             "numeric", "numeric", "numeric",
                                                                             "numeric", "numeric"))

PAG_IDE_Invest_Imediato <- rename(PAG_IDE_Invest_Imediato, "Pais" = "Discriminação")

# Transforma a lista de países da Planilha IDE em um df 
df_paises_IDE <- data.frame(lista_paises_IDE)





# Lista com os anos até 2020
anos <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020")




# Lista com os anos até 2019
anos_ate19 <- c("2010", "2011", "2012", "2013", "2014", "2015",
                "2016", "2017", "2018", "2019")





#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_Invest_Imediato <- PAG_IDE_Invest_Imediato %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDE_Invest_Imediato")




# Omite os NA's no df 
DF_IDE_Invest_Imediato <- na.omit(DF_IDE_Invest_Imediato)

# Organiza o df por ordem alfabética
DF_IDE_Invest_Imediato <- arrange_all(DF_IDE_Invest_Imediato)





# ----------------------------------------------- // -----------------------------------------------




# Realiza leitura da página 9 da Planilha IDE 
PAG_IDE_Oper_Intercomp <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                                 sheet = "9", skip = 4, col_types = c("text", "numeric", "numeric",
                                                                      "numeric", "numeric", "numeric",
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric",
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric",
                                                                      "numeric", "numeric", "numeric",
                                                                      "numeric", "numeric"))


PAG_IDE_Oper_Intercomp <- rename(PAG_IDE_Oper_Intercomp, "Pais" = "Discriminação")


#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_Oper_Intercomp <- PAG_IDE_Oper_Intercomp %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDE_Oper_Intercomp")




# Omite os NA's no df 
DF_IDE_Oper_Intercomp <- na.omit(DF_IDE_Oper_Intercomp)

# Organiza o df por ordem alfabética
DF_IDE_Oper_Intercomp <- arrange_all(DF_IDE_Oper_Intercomp)





# ----------------------------------------------- // -----------------------------------------------




# Realiza leitura da página 11 da Planilha IDE 
PAG_IDE_Acoes <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                        sheet = "11", skip = 4, col_types = c("text", "numeric", "numeric",
                                                              "numeric", "numeric", "numeric",
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric",
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric",
                                                              "numeric", "numeric", "numeric",
                                                              "numeric", "numeric"))

PAG_IDE_Acoes <- rename(PAG_IDE_Acoes, "Pais" = "Discriminação")


#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_Acoes <- PAG_IDE_Acoes %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDE_Acoes")




# Omite os NA's no df
DF_IDE_Acoes <- na.omit(DF_IDE_Acoes)

# Organiza o df por ordem alfabética
DF_IDE_Acoes <- arrange_all(DF_IDE_Acoes)





# ----------------------------------------------- // -----------------------------------------------




# Realiza leitura da página 13 da Planilha IDE
PAG_IDE_RF_Longo_Prazo <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                                 sheet = "13", skip = 4, col_types = c("text", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric"))

PAG_IDE_RF_Longo_Prazo <- rename(PAG_IDE_RF_Longo_Prazo, "Pais" = "Discriminação")


#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_RF_Longo_Prazo <- PAG_IDE_RF_Longo_Prazo %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDE_RF_Longo_Prazo")




# Omite os NA's no df
DF_IDE_RF_Longo_Prazo <- na.omit(DF_IDE_RF_Longo_Prazo)

# Organiza o df por ordem alfabética
DF_IDE_RF_Longo_Prazo <- arrange_all(DF_IDE_RF_Longo_Prazo)




# ----------------------------------------------- // -----------------------------------------------



# Realiza leitura da página 12 da Planilha IDE
PAG_IDE_RF_Curto_Prazo <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                                 sheet = "12", skip = 4, col_types = c("text", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric"))

PAG_IDE_RF_Curto_Prazo <- rename(PAG_IDE_RF_Curto_Prazo, "Pais" = "Discriminação")


#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_RF_Curto_Prazo <- PAG_IDE_RF_Curto_Prazo %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDE_RF_Curto_Prazo")




# Omite os NA's no df
DF_IDE_RF_Curto_Prazo <- na.omit(DF_IDE_RF_Curto_Prazo)

# Organiza o df por ordem alfabética
DF_IDE_RF_Curto_Prazo <- arrange_all(DF_IDE_RF_Curto_Prazo)





# ----------------------------------------------- // -----------------------------------------------




# Realiza leitura da página 14 da Planilha IDE
PAG_IDE_Moedas_19 <- TabelasCompletasPosicaoIDE <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", 
                                                              sheet = "14", skip = 4, col_types = c("text", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric"))

PAG_IDE_Moedas_19 <- rename(PAG_IDE_Moedas_19, "Pais" = "Discriminação")



#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_Moedas_19 <- PAG_IDE_Moedas_19 %>%
  select("Pais", anos_ate19)%>%
  pivot_longer(cols = anos_ate19, names_to = "Ano", values_to = "IDE_Moedas_19")




# Omite os NA's no df
DF_IDE_Moedas_19 <- na.omit(DF_IDE_Moedas_19)

# Organiza o df por ordem alfabética
DF_IDE_Moedas_19 <- arrange_all(DF_IDE_Moedas_19)





# ----------------------------------------------- // -----------------------------------------------




# Realiza leitura da página 15 da Planilha IDE
PAG_IDE_Moedas_20 <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                            sheet = "15", skip = 4, col_types = c("text", "numeric", 
                                                                  "numeric"))


PAG_IDE_Moedas_20 <- rename(PAG_IDE_Moedas_20, "Pais" = "Discriminação")

#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_Moedas_20 <- PAG_IDE_Moedas_20 %>%
  select("Pais", "2020")%>%
  pivot_longer(cols = "2020", names_to = "Ano", values_to = "IDE_Moedas_20")




# Omite os NA's no df
DF_IDE_Moedas_20 <- na.omit(DF_IDE_Moedas_20)

# Organiza o df por ordem alfabética
DF_IDE_Moedas_20 <- arrange_all(DF_IDE_Moedas_20)





# ----------------------------------------------- // -----------------------------------------------



# Realiza leitura da página 16 da Planilha IDE
PAG_IDE_Imoveis_19 <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                             sheet = "16", skip = 4, col_types = c("text", "numeric", 
                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", "numeric", "numeric"))

PAG_IDE_Imoveis_19 <- rename(PAG_IDE_Imoveis_19, "Pais" = "Discriminação")


#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_Imoveis_19 <- PAG_IDE_Imoveis_19 %>%
  select("Pais", anos_ate19)%>%
  pivot_longer(cols = anos_ate19, names_to = "Ano", values_to = "IDE_Imoveis_19")



# Omite os NA's no df
DF_IDE_Imoveis_19 <- na.omit(DF_IDE_Imoveis_19)

# Organiza o df por ordem alfabética
DF_IDE_Imoveis_19 <- arrange_all(DF_IDE_Imoveis_19)





# ----------------------------------------------- // -----------------------------------------------




# Realiza leitura da página 17 da Planilha IDE
PAG_IDE_Imoveis_20 <- read_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                             sheet = "17", skip = 4, col_types = c("text", "numeric", 
                                                                   "numeric"))
PAG_IDE_Imoveis_20 <- rename(PAG_IDE_Imoveis_20, "Pais" = "Discriminação")



#Filtra as páginas do df com Nome do país, ano e valor
DF_IDE_Imoveis_20 <- PAG_IDE_Imoveis_20 %>%
  select("Pais", "2020")%>%
  pivot_longer(cols = "2020", names_to = "Ano", values_to = "IDE_Imoveis_20")




# Omite os NA's no df
DF_IDE_Imoveis_20 <- na.omit(DF_IDE_Imoveis_20)

# Organiza o df por ordem alfabética
DF_IDE_Imoveis_20 <- arrange_all(DF_IDE_Imoveis_20)




# Realiza um Full_Join colocando APENAS UMA COLUNA para o país e o ano e colocando os valores dos outros dataframe's em colunas separadas 
DF_Geral <- full_join(DF_IDE_Invest_Imediato, DF_IDE_Oper_Intercomp, by = c("Pais", "Ano"))

DF_Geral <- full_join(DF_Geral, DF_IDE_Acoes, by = c("Pais", "Ano"))

DF_Geral <- full_join(DF_Geral, DF_IDE_RF_Longo_Prazo, by = c("Pais", "Ano"))

DF_Geral <- full_join(DF_Geral, DF_IDE_RF_Curto_Prazo, by = c("Pais", "Ano"))

# Transforma NA em 0 nas respectivas respectivas colunas
DF_Geral$IDE_RF_Longo_Prazo[is.na(DF_Geral$IDE_RF_Longo_Prazo)] <- 0
DF_Geral$IDE_RF_Curto_Prazo[is.na(DF_Geral$IDE_RF_Curto_Prazo)] <- 0
DF_Geral$IDE_RF_Curto_Prazo[is.na(DF_Geral$IDE_Acoes)] <- 0


# Realiza o somatório para criar a coluna Investimento em Carteira
DF_Geral <- mutate(DF_Geral, Invest_Em_Carteira = IDE_RF_Longo_Prazo + IDE_RF_Curto_Prazo + IDE_Acoes)

DF_IDE_Moedas_20 <- full_join(DF_IDE_Moedas_19, DF_IDE_Moedas_20, by = c("Pais", "Ano"))
DF_IDE_Moedas_20 <- mutate_all(DF_IDE_Moedas_20, replace_na, 0)
DF_IDE_Moedas_20$IDE_Moedas_19 <- ifelse(DF_IDE_Moedas_20$IDE_Moedas_19 == 0, DF_IDE_Moedas_20$IDE_Moedas_20 ,DF_IDE_Moedas_20$IDE_Moedas_19)
DF_IDE_Moedas_20$IDE_Moedas_20 <- NULL




DF_Geral <- full_join(DF_Geral, DF_IDE_Moedas_20, by = c("Pais", "Ano"))
DF_Geral <- rename(DF_Geral, "IDE_Moedas" = "IDE_Moedas_19")

# ------------------------ // --------------------------------------------

DF_IDE_Imoveis_20 <- full_join(DF_IDE_Imoveis_19, DF_IDE_Imoveis_20, by = c("Pais", "Ano"))
DF_IDE_Imoveis_20 <- mutate_all(DF_IDE_Imoveis_20, replace_na, 0)
DF_IDE_Imoveis_20$IDE_Imoveis_19 <- ifelse(DF_IDE_Imoveis_20$IDE_Imoveis_19 == 0, DF_IDE_Imoveis_20$IDE_Imoveis_20 ,DF_IDE_Imoveis_20$IDE_Imoveis_19)
DF_IDE_Imoveis_20$IDE_Imoveis_20 <- NULL





DF_Geral <- full_join(DF_Geral, DF_IDE_Imoveis_20, by = c("Pais", "Ano"))
DF_Geral <- rename(DF_Geral, "IDE_Imoveis" = "IDE_Imoveis_19")


PAG_IDE_Por_Setor <- read_xlsx("data-raw/TabelasCompletasPosicaoIDE.xlsx",
                                             sheet = "18", range = "A5:BZ24", col_types = "text")

DF_IDE_Por_Setor  <- PAG_IDE_Por_Setor %>%
  select("ANO: 2020", lista_paises_IDE_Por_Setor)%>%
  pivot_longer(cols = lista_paises_IDE_Por_Setor, names_to = "Pais", values_to = "IDE_Por_Setor")

DF_IDE_Por_Setor <- na.omit(DF_IDE_Por_Setor)

# Organiza o df por ordem alfabética
DF_IDE_Por_Setor <- arrange_all(DF_IDE_Por_Setor)

