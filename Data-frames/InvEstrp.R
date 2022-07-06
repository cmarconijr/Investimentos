#Código para pegar dados do InvEstrp
library(rccdates)
# Baixa a Planilha
httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/InvEstrp.xls",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "InvEstrp.xls"), overwrite = T))

# Carrega a página da planilha
InvEstrp_fluxo_Invest <- read_xls("data-raw/InvEstrp.xls",
                      sheet = "IDP ingresso por país", skip = 4, col_types = c("text", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric")
)

InvEstrp_fluxo_Invest <- rename(InvEstrp_fluxo_Invest, "Pais" = "Discriminação")
# Lista com os anos
anos <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020")

#Filtra as páginas do df
DF_InvEstrp_fluxo_Invest <- InvEstrp_fluxo_Invest %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "value")

DF_InvEstrp_fluxo_Invest <- na.omit(DF_InvEstrp_fluxo_Invest)
DF_InvEstrp_fluxo_Invest <- arrange_all(DF_InvEstrp_fluxo_Invest)

DF_Geral <- full_join(DF_Geral,DF_InvEstrp_fluxo_Invest, by = c("Pais", "Ano"))
DF_Geral <- rename(DF_Geral, "InvEstrp_fluxo_Invest" = "value")
