
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
  
  df <- df_aula %>%
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
  
  df
  
}


escolher_grupos <- function(df, n_aula, n_jogo) {
  
  set.seed(1234)
  
  # Escolher grupo jogo (não há distraídos)
  df_jogo <- df %>% 
    filter(grupo == 'jogo') %>% 
    slice_sample(n = n_jogo)
  
  # Escolher grupo aula (não dorminhocos)
  df_aula <- df %>% 
    filter(
      grupo == 'aula',
      !distraido
    ) %>% 
    slice_sample(n = n_aula)
  
  # Completar grupo aula com dorminhocos
  if (nrow(df_aula) < n_aula) {
    diferenca <- n_aula - nrow(df_aula)
    df_dorminhocos <- df %>% 
      filter(
        grupo == 'aula',
        distraido
      ) %>% 
      slice_sample(n = diferenca)
    df_aula <- bind_rows(df_aula, df_dorminhocos)  
  }
  
  # Juntar
  df_jogo %>% 
    bind_rows(df_aula)

}


gerar_histogramas <- function(df) {
  
  brks_barras <- seq(0, 100, 10)
  brks_x <- seq(0, 100, 10)
  brks_y <- seq(0, 15, 2)
  limite_y <- 12
  
  histograma_aula <- ggplot() +
    geom_histogram(
      aes(x = pct),
      df %>% filter(grupo == 'aula'),
      breaks = brks_barras,
      fill = 'darkblue',
      alpha = .8
    ) +
    scale_x_continuous(
      breaks = brks_x,
      limits = c(0, 100)
    ) +
    scale_y_continuous(
      breaks = brks_y,
      limits = c(NA, limite_y)
    ) +
    labs(
      x = '% acertos',
      y = 'sujeitos',
      title = 'Histograma do percentual de acertos\n(grupo AULA)'
    )
 
    histograma_jogo <- ggplot() +
      geom_histogram(
        aes(x = pct),
        df %>% filter(grupo == 'jogo'),
        breaks = brks_barras,
        fill = 'darkred',
        alpha = .8
      ) +
      scale_x_continuous(
        breaks = brks_x,
        limits = c(0, 100)
      ) +
      scale_y_continuous(
        breaks = brks_y,
        limits = c(NA, limite_y)
      ) +
      labs(
        x = '% acertos',
        y = 'sujeitos',
        title = 'Histograma do percentual de acertos\n(grupo JOGO)'
      )
 
    list(
      aula = histograma_aula,
      jogo = histograma_jogo
    )
  
}


gerar_ogivas <- function(df) {
  
  brks_barras <- seq(0, 100, 10)
  brks_x <- seq(0, 100, 10)
  brks_y <- seq(0, 15, 2)
  limite_y <- 12
  
  ogiva_aula <- ggplot() +
    geom_step(
      aes(x = pct),
      df %>% filter(grupo == 'aula'),
      stat = 'ecdf',
#      breaks = brks_barras,
      color = 'darkblue'
    ) +
    scale_x_continuous(
      breaks = brks_x,
      limits = c(0, 100)
    ) +
    scale_y_continuous(
      breaks = brks_y,
      limits = c(NA, limite_y)
    ) +
    labs(
      x = '% acertos',
      y = '% sujeitos',
      title = 'Frequência acumulada do percentual de acertos\n(grupo AULA)'
    )
 
  ogiva_jogo <- NULL
    # histograma_jogo <- ggplot() +
    #   geom_histogram(
    #     aes(x = pct),
    #     df %>% filter(grupo == 'jogo'),
    #     breaks = brks_barras,
    #     fill = 'darkred',
    #     alpha = .8
    #   ) +
    #   scale_x_continuous(
    #     breaks = brks_x,
    #     limits = c(0, 100)
    #   ) +
    #   scale_y_continuous(
    #     breaks = brks_y,
    #     limits = c(NA, limite_y)
    #   ) +
    #   labs(
    #     x = '% acertos',
    #     y = 'sujeitos',
    #     title = 'Histograma do percentual de acertos (grupo JOGO)'
    #   )
 
    list(
      aula = ogiva_aula,
      jogo = ogiva_jogo
    )
  
}


construir_intervalos <- function(df){
  
  t_aula <- t.test(
    df %>% filter(grupo == 'aula') %>% pull(pct)
  )
  
  media_aula <- t_aula$estimate
  inf_aula <- t_aula$conf.int[1]
  sup_aula <- t_aula$conf.int[2]
  
  t_jogo <- t.test(
    df %>% filter(grupo == 'jogo') %>% pull(pct)
  )
  
  media_jogo <- t_jogo$estimate
  inf_jogo <- t_jogo$conf.int[1]
  sup_jogo <- t_jogo$conf.int[2]
  
  tibble(
    grupo = c('aula', 'jogo'),
    média = c(media_aula, media_jogo),
    inf = c(inf_aula, inf_jogo),
    sup = c(sup_aula, sup_jogo),
    `margem de erro` = média - inf
  ) %>% 
    select(grupo, média, `margem de erro`, everything())
 
}


tabela_intervalos <- function(intervalos) {
  
  intervalos %>% 
    rename(
      `limite inferior` = inf,
      `limite superior` = sup
    ) %>% 
    gt() %>% 
    fmt_number(
      dec_mark = ',', 
      sep_mark = '.'
    ) %>% 
    fmt_number(
      `margem de erro`, 
      dec_mark = ',', 
      sep_mark = '.',
      pattern = '±{x}'
    )
  
}


construir_plot_intervalos <- function(intervalos) {
  
  intervalos %>% 
    ggplot() +
    geom_errorbar(
      aes(y = grupo, xmin = inf, xmax = sup),# color = grupo),
      linewidth = .5,
      width = .2,
      show.legend = FALSE
    ) +
    geom_col(
      aes(y = grupo, x = média, fill = grupo),
      alpha = .5
    ) +
    scale_x_continuous(
      # limits = c(40, 80),
      breaks = seq(0, 100, 10)
    ) +
    scale_y_discrete(
      labels = NULL,
      breaks = NULL
    ) +
    scale_color_discrete(
      type = c('darkblue', 'darkred')
    ) +
    scale_fill_discrete(
      type = c('darkblue', 'darkred')
    ) +
    labs(
      y = NULL,
      x = '% acertos',
      color = NULL,
      title = 'Médias dos percentuais de acertos (aula versus jogo)',
      subtitle = 'com intervalos de confiança de 95%'
    ) +
    coord_flip()
  
}
