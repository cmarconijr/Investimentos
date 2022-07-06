#Código para pegar dados do InvBrap


# Baixa a Planilha
httr::GET("https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/InvBrap.xls",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "InvBrap.xls"), overwrite = T))

# Carrega a página da planilha
IDE_Fluxo_Invest_Imediato <- read_xls("data-raw/InvBrap.xls",
                      sheet = "IDE saídas por país", skip = 4,
                      col_types = c("text", 
                                    "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric")
)

IDE_Fluxo_Invest_Imediato <- rename(IDE_Fluxo_Invest_Imediato, "Pais" = "Discriminação")

# Lista com os anos
anos <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020")

#Filtra as páginas do df
DF_IDE_Fluxo_Invest_Imediato <- IDE_Fluxo_Invest_Imediato %>%
  select("Pais", anos)%>%
  pivot_longer(cols = anos, names_to = "Ano", values_to = "IDE_Fluxo_Invest_Imediato")

DF_IDE_Fluxo_Invest_Imediato <- na.omit(DF_IDE_Fluxo_Invest_Imediato)
DF_IDE_Fluxo_Invest_Imediato <- arrange_all(DF_IDE_Fluxo_Invest_Imediato)

DF_Geral <- full_join(DF_Geral,DF_IDE_Fluxo_Invest_Imediato, by = c("Pais", "Ano"))
