
  lista_paises_IDP_CF <- IDP_Por_Setor_Control_Final %>%
    select(pais)

  lista_paises_IDP_CF <- distinct(lista_paises_IDP_CF)


  lista_paises_IDP_CF <- rename(lista_paises_IDP_CF,  "paises_CF" = "pais")

  paises_CF <- c(lista_paises_IDP_CF$paises_CF)

  write_csv(lista_paises_IDP_CF, "lista_paises_IDP_CF.CSV")


  #------------------------------------------------------------------------------------------------

  lista_paises_IDP_II <- IDP_Por_Setor_Inv_Imed %>%
    select(pais)

  lista_paises_IDP_II <- distinct(lista_paises_IDP_II)


  lista_paises_IDP_II <- rename(lista_paises_IDP_II,  "paises_II" = "pais")

  paises_II <- c(lista_paises_IDP_II$paises_II)

  write_csv(lista_paises_IDP_II, "lista_paises_IDP_II.CSV")
  #------------------------------------------------------------------------------------------------

  lista_paises_IDP_II <- rename(lista_paises_IDP_II,   "pais" = "paises_II")
  lista_paises_IDP_CF <- rename(lista_paises_IDP_CF,   "pais" = "paises_CF")

  lista <- bind_rows(lista_paises_IDP_II, lista_paises_IDP_CF)
  lista <- distinct(lista)


  write_csv(lista, "lista.csv")






  lista_paises_IDP_II <- order(col_character(lista_paises_IDP_II$paises_II))

  lista_paises_IDP_II <- arrange(lista_paises_IDP_II$paises_II)

  lista_paises_IDP_II[41,1] <- "a"
  lista_paises_IDP_II[42,1] <- "a"



  #----------------------------------------------------------------------------------------------------------------------------------------------

  lista_paises_IDP_CF <- c("Estados Unidos", "Espanha",	"França", "Bélgica", "Reino Unido", "China", "Países Baixos", "Japão", "Alemanha", "Suíça",	"Luxemburgo", "Canadá",
                           "Brasil", "Itália", "Ilhas Cayman", "Portugal", "Cingapura", "Noruega", "Bermudas", "México", "Coréia do Sul", "Chile", "Suécia", "Irlanda",
                           "Austrália", "Ilhas Virgens Britânicas", "Índia", "Colômbia", "África do Sul", "Uruguai", "Argentina", "Áustria", "Panamá", "Dinamarca", "Indonésia",
                           "Emirados Árabes Unidos", "Israel", "Finlândia", "Ilhas Jersey", "Angola", "Ilha de Man", "Bahamas")




  lista_paises_IDP_II <- c("Países Baixos", "Estados Unidos", "Espanha", "Luxemburgo", "França", "Japão", "Reino Unido", "México", "Alemanha", "Canadá", "Ilhas Cayman", "Suíça", "Bermudas",
                           "Chile", "Portugal", "Itália", "Austrália", "Áustria", "Ilhas Virgens Britânicas", "Uruguai", "Suécia", "Bélgica", "Noruega", "Panamá", "Argentina",
                           "Finlândia", "Colômbia", "Coréia do Sul", "Bahamas", "Irlanda", "Dinamarca", "Angola", "Barbados", "China", "Curaçao", "Chipre", "Cingapura", "Ilhas Jersey",
                           "Hong Kong", "Emirados Árabes Unidos")






