#Código para pegar dados do Intercia Passivo

# Baixa a Planilha
httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/InterciaPassivop.xls",
          config = httr::config(ssl_verifypeer = F),          
          httr::write_disk(here::here("data-raw", "InterciaPassivop.xls"), overwrite = T))


# Carrega a página da planilha
Inter_Passivo_Ingressos <- read_excel("data-raw/InterciaPassivop.xls", 
                               sheet = "Ingressos por país", skip = 4, 
                               col_types = c("text", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric")
)

Inter_Passivo_Ingressos <- rename(Inter_Passivo_Ingressos, "Pais" = "Discriminação")

# Lista com os anos
anos <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020")

#Filtra as páginas do df
DF_Inter_Passivo_Ingressos <- Inter_Passivo_Ingressos %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "Inter_Passivo_Ingressos")

DF_Inter_Passivo_Ingressos <- na.omit(DF_Inter_Passivo_Ingressos)
DF_Inter_Passivo_Ingressos <- arrange_all(DF_Inter_Passivo_Ingressos)


Inter_Passivo_Amortizacoes <- read_excel("data-raw/InterciaPassivop.xls",
                      sheet = "Amortizações por país", skip = 4,
                      col_types = c("text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))

Inter_Passivo_Amortizacoes <- rename(Inter_Passivo_Amortizacoes, "Pais" = "Discriminação")

DF_Inter_Passivo_Amortizacoes <- Inter_Passivo_Amortizacoes %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "Inter_Passivo_Amortizacoes")

DF_Inter_Passivo_Amortizacoes <- na.omit(DF_Inter_Passivo_Amortizacoes)
DF_Inter_Passivo_Amortizacoes <- arrange_all(DF_Inter_Passivo_Amortizacoes)


DF_Geral <- full_join(DF_Geral,DF_Inter_Passivo_Ingressos, by = c("Pais", "Ano"))
DF_Geral <- full_join(DF_Geral,DF_Inter_Passivo_Amortizacoes, by = c("Pais", "Ano"))

#//////////////////////////////////////////////testes///////////////////////////////////////////////////////////////////


DF_Geral$Inter_Passivo_Amortizacoes[is.na(DF_Geral$Inter_Passivo_Amortizacoes)] <- 0

DF_Geral$Inter_Passivo_Ingressos[is.na(DF_Geral$Inter_Passivo_Ingressos)] <- 0

DF_Geral <- mutate(DF_Geral, Fluxo_Liquido = Inter_Passivo_Ingressos - Inter_Passivo_Amortizacoes)
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

