pais<-"China"


library(readxl)
library(tidyverse)
library(ggrepel)
install.packages("htmltools")
library(htmltools)

library(scales)

##----------------------------------------##---------------------------------------##------------------------------------------------------##-

## fazendo data frame IDP

# Carrega a página 6 da Planilha IDP
Invest_Imediato_IDP <- ler_excel("data-raw/TabelasCompletasPosicaoIDP.xlsx", "5", 4)

# Criando rank
idp <- Invest_Imediato_IDP %>%
  select(Discriminação,"2010", ...3, "2011", ...5, "2012", ...7, "2013", ...9, "2014", ...11, "2015", ...13, "2016", ...15, "2017", ...17,
         "2018", ...19, "2019", ...21, "2020", ...23)  %>%
  slice(-(1:5))%>%
  mutate(tipo = "IDP") %>%
  mutate(rankIDP = dplyr::row_number())

# Descobrindo o rank do país
posicao_pais_idp <- idp %>%
  dplyr::filter(Discriminação == pais) %>%
  dplyr::ungroup() %>%
  pull(rankIDP)

# Descobrindo ultima linha do df
ultima_linha_idp <- idp %>%
  dplyr::filter(Discriminação == "Demais") %>%
  dplyr::ungroup() %>%
  pull(rankIDP)

# Excluindo ultimas linhas desnecessárias do df
idp <- idp %>%
  slice(1:ultima_linha_idp)

# Criando data frame com os ranks
rank_paises_IDP <- if(posicao_pais_idp == nrow(idp)){
  slice(idp,c( posicao_pais_idp-2, posicao_pais_idp-1, posicao_pais_idp))
}else{
  if(posicao_pais_idp == 1L){
    slice(idp,c( posicao_pais_idp, posicao_pais_idp+1, posicao_pais_idp+2))
  }else{
    slice(idp,c( posicao_pais_idp-1, posicao_pais_idp, posicao_pais_idp+1))
  }
}



##---------------------------------------------##---------------------------------------------##---------------------------------------------##

## fazendo data frame IDE


# Carrega a página 3 da Planilha IDE
Invest_Imediato_IDE <- ler_excel("data-raw/TabelasCompletasPosicaoIDE.xlsx", "3", 4)

# Criando rank
ide <- Invest_Imediato_IDE %>%
  select(Discriminação,  "2010", ...9, "2011", ...11, "2012", ...13, "2013", ...15, "2014", ...17, "2015", ...19, "2016", ...21, "2017", ...23,
         "2018", ...25, "2019", ...27, "2020", ...29)  %>%
  slice(-(1:5))%>%
  mutate(tipo = "IDE") %>%
  mutate(rankIDE = dplyr::row_number())

# Descobrindo o rank do país
posicao_pais_ide <- ide %>%
  dplyr::filter(Discriminação == pais) %>%
  dplyr::ungroup() %>%
  pull(rankIDE)

# Descobrindo o rank do país
ultima_linha_ide <- ide %>%
  dplyr::filter(Discriminação == "Demais1/2/") %>%
  dplyr::ungroup() %>%
  pull(rankIDE)

# Excluindo ultimas linhas desnecessárias do df
ide <- ide %>%
  slice(1:ultima_linha_ide)

# Criando data frame com os ranks

rank_paises_IDE <- if(posicao_pais_ide == nrow(ide)){
  slice(ide,c( posicao_pais_ide-2, posicao_pais_ide-1, posicao_pais_ide))
}else{
  if(posicao_pais_ide == 1L){
    slice(ide,c( posicao_pais_ide, posicao_pais_ide+1, posicao_pais_ide+2))
  }else{
    slice(ide,c( posicao_pais_ide-1, posicao_pais_ide, posicao_pais_ide+1))
  }
}

##------------------------------------------#--------------------------------------#-----------------------------------#----------------------#

#Juntando os df


novo_rank_paises_IDP <- rank_paises_IDP%>%
  select(Discriminação, rankIDP, tipo,"2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019", "2020")%>%
  pivot_longer(cols = c("2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019", "2020"), names_to = "ano", values_to = "value")%>%
  rename(rank = rankIDP)

df_percent_idp <- rank_paises_IDP%>%
  select(Discriminação,rankIDP, ...3, ...5, ...7, ...9, ...11, ...13, ...15, ...17, ...19, ...21, ...23)%>%
  rename("2010" = ...3)%>%
  rename("2011" = ...5)%>%
  rename("2012" = ...7)%>%
  rename("2013" = ...9)%>%
  rename("2014" = ...11)%>%
  rename("2015" = ...13)%>%
  rename("2016" = ...15)%>%
  rename("2017" = ...17)%>%
  rename("2018" = ...19)%>%
  rename("2019" = ...21)%>%
  rename("2020" = ...23)%>%
  pivot_longer(cols = c("2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019", "2020"), names_to = "ano", values_to = "percent")%>%
  rename(rank = rankIDP)

novo_rank_paises_IDP <- left_join(novo_rank_paises_IDP, df_percent_idp, by = c('Discriminação', 'ano', 'rank'))

novo_rank_paises_IDP$percent <- as.numeric(novo_rank_paises_IDP$percent)

#-----------------#------------------

novo_rank_paises_IDE <- rank_paises_IDE%>%
  select(Discriminação, rankIDE, tipo,"2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019", "2020")%>%
  pivot_longer(cols = c("2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019", "2020"), names_to = "ano", values_to = "value")%>%
  rename(rank = rankIDE)

df_percent_ide <- rank_paises_IDE%>%
  select(Discriminação,rankIDE, ...9, ...11, ...13, ...15, ...17, ...19, ...21, ...23, ...25, ...27, ...29)%>%
  rename("2010" = ...9)%>%
  rename("2011" = ...11)%>%
  rename("2012" = ...13)%>%
  rename("2013" = ...15)%>%
  rename("2014" = ...17)%>%
  rename("2015" = ...19)%>%
  rename("2016" = ...21)%>%
  rename("2017" = ...23)%>%
  rename("2018" = ...25)%>%
  rename("2019" = ...27)%>%
  rename("2020" = ...29)%>%
  pivot_longer(cols = c("2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019", "2020"), names_to = "ano", values_to = "percent")%>%
  rename(rank = rankIDE)

novo_rank_paises_IDE <- left_join(novo_rank_paises_IDE, df_percent_ide, by = c('Discriminação', 'ano', 'rank'))

novo_rank_paises_IDE$percent <- as.numeric(novo_rank_paises_IDE$percent)



#dividindo por 100 para botar em porcentagem, o IDP não precisava
novo_rank_paises_IDE$percent <- novo_rank_paises_IDE$percent / 100


df_geral <- bind_rows(novo_rank_paises_IDP, novo_rank_paises_IDE)
df_geral$value <- as.numeric(df_geral$value)
df_geral$ano <- as.numeric(df_geral$ano)
##------------------------------------------##------------------------------------------##--------------------------------------##------------#

#gráfico parceiros próximos

df_geral %>%
  select(Discriminação, value, rank, tipo, ano)%>%
  filter(ano == max(ano))%>%
  ggplot2::ggplot() +
  ggplot2::geom_col(
    ggplot2::aes(tidytext::reorder_within(Discriminação, value, tipo), value, fill = tipo),
    show.legend = F) +
  ggplot2::geom_col(data = . %>%
                      dplyr::filter(Discriminação == pais),
                    ggplot2::aes(tidytext::reorder_within(Discriminação, value, tipo), value, fill = "red"),
                    show.legend = F) +
  ggplot2::geom_label(ggplot2::aes(tidytext::reorder_within(Discriminação, value, tipo), value,
                                   label = rank)) +
  ggplot2::facet_wrap(~tipo, scales = "free_y") +
  ggplot2::labs(title = NULL,
                subtitle = NULL,
                x = NULL, y = NULL, caption = "") +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
  ggthemes::scale_fill_tableau() +
  tidytext::scale_x_reordered()+
  ggplot2::labs(title = glue::glue("Brasil - {pais}, parceiros de investimento próximos, em 2020"),
                caption = "Fonte: Banco Central", x = NULL, y = NULL)

##------------------------------------------##------------------------------------------##--------------------------------------##------------#
#gráfico proporção parceiros próximos
proporção <- df_geral %>%
  select(Discriminação, percent, rank, tipo, ano)%>%
  filter(ano == max(ano))


  ggplot(proporção, aes(x = percent, y = rank, group = " " ))+
    geom_point()+
    geom_point(data = proporção %>%
                 dplyr::filter(proporção$Discriminação == pais), ggplot2::aes(color = Discriminação), show.legend = F)+
    geom_label_repel(label = proporção$Discriminação)+
    theme_minimal()+
    scale_y_reverse(breaks = scales::breaks_pretty(), labels = scales::label_ordinal())+
    scale_x_continuous(labels = scales::label_percent())+
    facet_wrap(~tipo, scales = "free_y")+
    ggplot2::labs(title = glue::glue("Brasil - {pais}, ranking e proporção de Investimentos, em 2020"),
                  caption = "Fonte: Banco Central", x = NULL, y = NULL)


##------------------------------------------##------------------------------------------##--------------------------------------##------------#
#gráfico evolução parceiros próximos

df_geral %>%
  ggplot2::ggplot(ggplot2::aes(ano, value)) +
  ggplot2::geom_point() +
  ggplot2::geom_line(ggplot2::aes(color = Discriminação,
                                  group = Discriminação),
                     size = 1.5,
                     linetype = 4,
                     show.legend = F) +
  ggplot2::geom_line(data = . %>%
                       dplyr::filter(Discriminação == pais),
                     ggplot2::aes(color = Discriminação,
                                  group = Discriminação),
                     size = 3,
                     show.legend = F) +

  ggrepel::geom_label_repel(data = . %>%
                              dplyr::filter(ano == max(ano) |
                                              ano == min(ano) |
                                              ano == max(ano)-(max(ano)-min(ano))/2),
                            ggplot2::aes(ano, value, label = Discriminação),
                            size = 2, show.legend = F) +

  ggplot2::facet_wrap(~ tipo, scales = "free",
                      nrow = 2) +
  ggthemes::scale_color_tableau() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = NULL,
                caption = NULL,
                x = NULL, y = NULL)+
  ggplot2::scale_y_continuous(labels = scales::label_number_si())+
  ggplot2::scale_x_continuous(limits = c(2010, 2020),
                              breaks = scales::breaks_pretty())+
  ggplot2::labs(title = glue::glue("Brasil - {pais}, evolução do investimento até 2020"),
                caption = "Fonte: Banco Central", x = NULL, y = NULL)

##------------------------------------------##------------------------------------------##--------------------------------------##------------#


#tabela parceiros próximos

pp <- df_geral %>%
  rename(pct_percent = percent)%>%
  dplyr::group_by(Discriminação, tipo) %>%
  dplyr::arrange(dplyr::desc(ano), .by_group = T) %>%
  dplyr::mutate(pct_var = value/dplyr::lead(value)-1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(ano >= max(ano)-3) %>%
  dplyr::select(-c(rank)) %>%
  dplyr::group_by(ano, tipo) %>%
  dplyr::arrange(dplyr::desc(value), .by_group = T) %>%
  dplyr::arrange(dplyr::desc(ano)) %>%
  dplyr::relocate(ano, tipo, Discriminação, value, .data$pct_var, .data$pct_percent) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("val"), scales::label_number_si(accuracy = 0.01))) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("pct_") , scales::label_percent(decimal.mark = ",", accuracy = .01)))

pp %>%
  kableExtra::kbl(booktabs = T, col.names = c("Ano", "Inv. direto no país e Inv. direto no exterior", "Pa\u00eds", "Valor", "Varia\u00e7\u00e3o", "Propor\u00e7\u00e3o")) %>%
  kableExtra::kable_styling(font_size = 7, full_width = T, latex_options = c("hold_position")) %>%
  kableExtra::column_spec(3, width = "20em") %>%
  kableExtra::collapse_rows(columns = 1:2, latex_hline = "full", valign = "top",
                            row_group_label_position = "stack", target = 2) %>%
  kableExtra::add_header_above(header = c(setNames(6,"Comércio Intraindústria e Índice de Concentração")), bold = T) %>%
  kableExtra::row_spec(which(pp$Discriminação == pais), bold = T) %>%
  kableExtra::column_spec(1:2, background = "white", color = "black")

