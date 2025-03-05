
ler_dados <- function(
  aula = 'dados/aula.csv',
  jogo = 'dados/jogo.csv',
  nquestoes1 = 16   # qtde de questões da parte 1
) {

  # Registros de aula não têm data
  df_aula <- readr::read_csv(aula) %>%
    mutate(
      grupo = 'aula',
      data = NA_Date_,
      .before = 1
    )
    
  df_jogo <- readr::read_csv(jogo) %>%
    mutate(
      grupo = 'jogo',
      data = lubridate::mdy(data),
      .before = 1
    )
  
  df_aula %>%
    bind_rows(df_jogo) %>%
    mutate(
      acertos = acertos_parte1 + acertos_parte2,
      questoes = nquestoes1 + (100 * acertos_parte2 / pct_parte2),
      pct = 100 * acertos / questoes
    )
  
}
