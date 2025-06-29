
ler_dados <- function(
  aula = 'dados/aula.csv',
  jogo = 'dados/jogo.csv'
) {

  # Alguns registros de aula não têm data
  df_aula <- readr::read_csv(
    aula,
    col_types = '?ccnncnnc'
  ) %>% 
    mutate(grupo = 'aula')

  df_jogo <- readr::read_csv(
    jogo,
    col_types = '?ccnncnnc'
  )  %>% 
    mutate(grupo = 'jogo')

  # Temos um problema:
  # A quantidade de questões na parte 2 do teste para alunos do jogo varia!
  # Este dado está na fórmula do excel que calcula o percentual de acertos:
  # 
  # =Tabela137472[[#This Row];[N de Acertos2]]/2
  # 
  # É o denominador (2).
  # 
  # Coloquei os números de questões em colunas separadas na planilha, tanto
  # para a aula quanto para o jogo.
  
  df_aula %>%
    bind_rows(df_jogo) %>%
    mutate(
      distraido = distraido == 'Sim', 
      pct_parte1 = parse_number(
        pct_parte1,
        locale = locale(decimal_mark = ',', grouping_mark = '')
      ),
      pct_parte2 = parse_number(
        pct_parte2,
        locale = locale(decimal_mark = ',', grouping_mark = '')
      ),
      acertos = acertos_parte1 + acertos_parte2,
      questoes = nquestoes_parte1 + nquestoes_parte2,
      pct = 100 * acertos / questoes
    )
  
}
