
# Carrega pacotes e base de dados ----
library(pacman)
pacman::p_load(dplyr, ggplot2, tidyverse, readr, plotly, shiny, writexl)

base_bruta = read_csv("mundo_transfermarkt_competicoes_brasileirao_serie_a.csv")



# Ajusta base de dados ----

# Dobra as linhas para que cada partida seja referente a um time
sub_base1 = base_bruta |> mutate(time_referencia = time_mandante, time_adversario = time_visitante,
                                 colocacao_referencia = colocacao_mandante, colocacao_adversario = colocacao_visitante,
                                 mando_de_campo = 'Mandante')
sub_base2 = base_bruta |> mutate(time_referencia = time_visitante, time_adversario = time_mandante,
                                 colocacao_referencia = colocacao_visitante, colocacao_adversario = colocacao_mandante,
                                 mando_de_campo = 'Visitante')

# Adiciona colunas relevantes
sub_base3 = rbind(sub_base1, sub_base2) |> mutate(resultado_mando = factor( case_when(gols_mandante > gols_visitante ~ 'Mandante',
                                                        gols_mandante < gols_visitante ~ 'Visitante',
                                                        TRUE ~ 'Empate'), 
                                                        levels = c('Mandante', 'Empate', 'Visitante') ),
                                resultado_referencia = factor( case_when(resultado_mando == mando_de_campo ~ 'Vitória',
                                                                 resultado_mando == 'Empate' ~ 'Empate',
                                                                 TRUE ~ 'Derrota'), 
                                levels = c('Vitória', 'Empate', 'Derrota') ),
                                pontos_rodada = case_when(resultado_referencia == 'Vitória' ~ 3,
                                                          resultado_referencia == 'Empate' ~ 1,
                                                          TRUE ~ 0),
                                placar = paste(time_mandante, gols_mandante, 'X',
                                               gols_visitante, time_visitante) )

# Adiciona mais colunas relevantes
base_final <- sub_base3 |>
  arrange(time_referencia, ano_campeonato, rodada) |>
  group_by(time_referencia, ano_campeonato) |>
  # Primeiro, adicionar a linha da rodada 0
  
  group_modify(~ {
    linha_zero <- tibble(
      time_referencia = unique(.x$time_referencia),
      ano_campeonato = unique(.x$ano_campeonato),
      rodada = 0,
      pontos_rodada = 0,
      data = min(.x$data) -1
    )

    dados_completos <- bind_rows(linha_zero, .x)

    colunas_faltantes <- setdiff(names(.x), names(dados_completos))
    for(col in colunas_faltantes) {
      dados_completos[[col]] <- NA
    }

    dados_completos <- dados_completos[, names(.x)]

    return(dados_completos)
  }) |>
  
  # Agora calcular as acumulações
  mutate(pontos_acumulados = cumsum(pontos_rodada) ) |>
    # Calcular rodada_cronologica considerando a linha 0
  arrange(data) |> mutate(
    rodada_cronologica = seq_len(n()) - 1,  # -1 para começar do 0
    pontos_acumulados_cronologico = cumsum(pontos_rodada)
  ) |>
  ungroup() |>
  arrange(time_referencia, ano_campeonato, rodada)

# Se necessário, este trecho retira a "rodada 0"
#base_final = filter(base_final, rodada > 0)

rm(sub_base1, sub_base2)

#write_xlsx(base_final, "base_brasileiro.xlsx")



# Brasileirão 2023 ----

## Briga pelo título ----

### G6 ----
br_2023_g6 = base_final |> filter(ano_campeonato == 2023, time_referencia %in%
                                      # c("Botafogo", 'Palmeiras') ) |>
  c("Botafogo", 'Palmeiras', 'Grêmio', 'Atlético-MG', 'Flamengo', 'RB Bragantino') ) |>
  mutate(time_referencia = factor(time_referencia, levels = c('Palmeiras', 'Grêmio', 'Atlético-MG',
                                                              'Flamengo', "Botafogo", 'RB Bragantino')))

# Cores específicas
cores_times <- c(
  "Botafogo" = "#000000",       # Preto
  "Palmeiras" = "#018047",      # Verde
  "Grêmio" = "#1d66bf",         # Azul forte
  "Atlético-MG" = "#dfe83a",    # Preto (pode ajustar se quiser diferenciar)
  "Flamengo" = "#c71c37",       # Vermelho rubro
  "RB Bragantino" = "#f58e92"   # Verde
)

# Gráfico com símbolos PREENCHIDOS corretos
ggplot(br_2023_g6, aes(x = rodada_cronologica, y = pontos_acumulados_cronologico, 
                    color = time_referencia)) +
  # Linha step
  geom_step(linewidth = 1) +
  # Pontos com símbolos PREENCHIDOS
  geom_point(aes(shape = resultado_referencia), 
            size = 2.5, data = ~ filter(., rodada_cronologica > 0)) +  # size maior para melhor visualização
  # # Escala de cores para os times
  scale_color_manual(values = cores_times, name = "Time") +
  # # Escala de shapes PREENCHIDOS
  scale_shape_manual(
    values = c("Vitória" = 24,     # Triângulo para cima
               "Empate" = 21,
               "Derrota" = 25),
    name = "Resultado",
    labels = c("Vitória (3 pts)", "Empate (1 pt)", "Derrota (0 pts)")
  ) +
  
  # ESCALAS DOS EIXOS
  scale_x_continuous(
    breaks = seq(0, max(br_2023_g6$rodada_cronologica, na.rm = TRUE), by = 4),
    limits = c(0, max(br_2023_g6$rodada_cronologica, na.rm = TRUE))
  ) +
  scale_y_continuous(
    breaks = seq(0, 72, by = 8),
    limits = c(0, 72)
  ) +
  
  labs(title = "Pontuação por Rodada - G6",
       x = "Rodada",
       y = "Pontuação",
       color = "Time",
    shape = "Resultado na Rodada"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
#ggsave("grafs_2023/g6_2023.png", width = 256, height = 152, units = "mm", bg = "white")


# Palmeiras e Botafogo
br_2023_pal_bot = base_final |> filter(ano_campeonato == 2023, time_referencia %in%
                                    c("Botafogo", 'Palmeiras') ) |>
                                    #c("Botafogo", 'Palmeiras', 'Grêmio', 'Atlético-MG', 'Flamengo', 'RB Bragantino') ) |>
  mutate(time_referencia = factor(time_referencia, levels = c('Palmeiras', 'Grêmio', 'Atlético-MG',
                                                              'Flamengo', "Botafogo", 'RB Bragantino')))

# Cores específicas
cores_times <- c(
  "Botafogo" = "#000000",       # Preto
  "Palmeiras" = "#018047",      # Verde
  "Grêmio" = "#1d66bf",         # Azul forte
  "Atlético-MG" = "#dfe83a",    # Preto (pode ajustar se quiser diferenciar)
  "Flamengo" = "#c71c37",       # Vermelho rubro
  "RB Bragantino" = "#f58e92"   # Verde
)

# Gráfico com símbolos PREENCHIDOS corretos
ggplot(br_2023_pal_bot, aes(x = data, y = pontos_acumulados_cronologico, 
                       color = time_referencia)) +
  # Linha step
  geom_step(linewidth = 1) +
  # Pontos com símbolos PREENCHIDOS
  geom_point(aes(shape = resultado_referencia),
             size = 2.5, data = ~ filter(., rodada_cronologica > 0)) +
  # # Escala de cores para os times
  scale_color_manual(values = cores_times, name = "Time") +
  # # Escala de shapes PREENCHIDOS
  scale_shape_manual(
    values = c("Vitória" = 24,     # Triângulo para cima
               "Empate" = 21,
               "Derrota" = 25),
    name = "Resultado",
    labels = c("Vitória (3 pts)", "Empate (1 pt)", "Derrota (0 pts)")
  ) +
  
  # ESCALAS DOS EIXOS
  # scale_x_continuous(
  #   breaks = seq(0, max(br_2023_cima$rodada_cronologica, na.rm = TRUE), by = 4),
  #   limits = c(0, max(br_2023_cima$rodada_cronologica, na.rm = TRUE))
  # ) +
  scale_y_continuous(
    breaks = seq(0, 72, by = 8),
    limits = c(0, 72)
  ) +
  
  labs(title = "Pontuação por Rodada - Palmeiras e Botafogo",
       x = "Rodada",
       y = "Pontuação",
       color = "Time",
       shape = "Resultado na Rodada"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
#ggsave("grafs_2023/pal_bot_2023.png", width = 256, height = 152, units = "mm", bg = "white")


## Briga contra o rebaixamento ----

br_2023_z6 = base_final |> filter(ano_campeonato == 2023, time_referencia %in%
                                    c("Vasco da Gama", 'EC Bahia', 'Santos', 'Goiás') ) |> #, 'Coritiba FC', 'América-MG') ) |>
  mutate(time_referencia = factor(time_referencia, levels = c("Vasco da Gama", 'EC Bahia', 'Santos',
                                                              'Goiás', 'Coritiba FC', 'América-MG') ))

# Cores específicas
cores_times <- c(
  "Vasco da Gama" = 'black', 
  'EC Bahia' = 'blue',
  'Santos' = 'gray',
  'Goiás' = 'lightgreen',
  'Coritiba FC' = 'green',
  'América-MG' = 'darkgreen'
)

# Gráfico com símbolos PREENCHIDOS corretos
ggplot(br_2023_z6, aes(x = rodada_cronologica, y = pontos_acumulados_cronologico, 
                       color = time_referencia)) +
  # Linha step
  geom_step(linewidth = 1) +
  geom_point(aes(shape = resultado_referencia), 
             size = 2, data = ~ filter(., rodada_cronologica > 0)) +  
  # # Escala de cores para os times
  scale_color_manual(values = cores_times, name = "Time") +
  # # Escala de shapes PREENCHIDOS
  scale_shape_manual(
    values = c("Vitória" = 24,     # Triângulo para cima
               "Empate" = 21,
               "Derrota" = 25),
    name = "Resultado",
    labels = c("Vitória (3 pts)", "Empate (1 pt)", "Derrota (0 pts)")
  ) +
  
  # ESCALAS DOS EIXOS
  scale_x_continuous(
    breaks = seq(0, max(br_2023_z6$rodada_cronologica, na.rm = TRUE), by = 4),
    limits = c(0, max(br_2023_z6$rodada_cronologica, na.rm = TRUE))
  ) +
  scale_y_continuous(
    breaks = seq(0, 48, by = 8),
    limits = c(0, 48)
  ) +
  
  labs(title = "Pontuação por Rodada - 15º a 18º",
       x = "Rodada",
       y = "Pontuação",
       color = "Time",
       shape = "Resultado na Rodada"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
#ggsave("grafs_2023/reb_2023.png", width = 256, height = 152, units = "mm", bg = "white")




# Shiny ----

br_2023 <- base_final |> 
  filter(ano_campeonato == 2023)

ui <- fluidPage(
  titlePanel("Pontuação - Brasileirão 2023"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros"),
      # Seletor de times
      checkboxGroupInput(
        inputId = "times_selecionados",
        label = "Selecionar Times:",
        choices = unique(br_2023$time_referencia),
        selected = character(0)  # Inicia sem seleção
      ),
      # Botões de ação rápida
      actionButton("todos_times", "Todos os Times", width = "100%"),
      actionButton("limpar", "Limpar Seleção", width = "100%"),
      hr(),
      # Opções de visualização
      checkboxInput("mostrar_pontos", "Mostrar Pontos", value = TRUE),
      checkboxInput("mostrar_metas", "Mostrar Metas (45/60/70 pts)", value = FALSE),
      sliderInput("rodada_range", "Rodadas:",
                  min = 1, max = 38, value = c(1, 38)),
      hr(),
      # Estatísticas
      verbatimTextOutput("estatisticas")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("grafico", height = "600px"),
      hr(),
      h4("Tabela de Dados"),
      dataTableOutput("tabela_dados")
    )
  )
)

server <- function(input, output, session) {
  
  # Observar botões de ação rápida
  observeEvent(input$todos_times, {
    updateCheckboxGroupInput(
      session, "times_selecionados",
      selected = unique(br_2023$time_referencia)
    )
  })
  
  observeEvent(input$limpar, {
    updateCheckboxGroupInput(session, "times_selecionados", selected = character(0))
  })
  
  # Dados filtrados
  dados_filtrados <- reactive({
    req(input$times_selecionados)
    
    br_2023 %>%
      filter(
        time_referencia %in% input$times_selecionados,
        rodada_cronologica >= input$rodada_range[1],
        rodada_cronologica <= input$rodada_range[2]
      )
  })
  
  # Gráfico
  output$grafico <- renderPlotly({
    if (length(input$times_selecionados) == 0) {
      return(plotly_empty() %>% 
               layout(
                 title = "Selecione pelo menos um time para visualizar o gráfico",
                 plot_bgcolor = "#f5f5f5"
               ))
    }
    
    # Criar gráfico base
    p <- plot_ly() %>%
      layout(
        title = paste("Pontos Acumulados - Brasileirão 2023"),
        xaxis = list(
          title = "Rodada",
          range = c(input$rodada_range[1] - 0.5, input$rodada_range[2] + 0.5)
        ),
        yaxis = list(title = "Pontos Acumulados"),
        hovermode = "closest",
        showlegend = TRUE,
        plot_bgcolor = "#f5f5f5"
      )
    
    # Adicionar linhas dos times (se habilitado)
    if (input$mostrar_pontos) {
      # Paleta de cores para os times
      cores <- scales::hue_pal()(length(unique(dados_filtrados()$time_referencia)))
      
      for (i in seq_along(unique(dados_filtrados()$time_referencia))) {
        time <- unique(dados_filtrados()$time_referencia)[i]
        dados_time <- dados_filtrados() %>% 
          filter(time_referencia == time) %>%
          arrange(rodada_cronologica)
        
        # Adicionar variável placar ao hovertext
        hover_text <- if ("placar" %in% names(dados_time)) {
          ~paste(
            "<b>", time_referencia, "</b><br>",
            "Rodada: ", rodada_cronologica, "<br>",
            "Pontos Acumulados: ", pontos_acumulados_cronologico, "<br>",
            "Posição: ", colocacao_referencia, "º<br>",
            "Placar: ", placar
          )
        } else {
          # Se a variável placar não existir, mostrar mensagem
          ~paste(
            "<b>", time_referencia, "</b><br>",
            "Rodada: ", rodada_cronologica, "<br>",
            "Pontos Acumulados: ", pontos_acumulados_cronologico, "<br>",
            "Posição: ", colocacao_referencia, "º<br>",
            "Placar: Informação não disponível"
          )
        }
        
        p <- p %>%
          add_trace(
            data = dados_time,
            x = ~rodada_cronologica,
            y = ~pontos_acumulados_cronologico,
            type = "scatter",
            mode = "lines+markers",
            name = time,
            line = list(width = 3, color = cores[i]),
            marker = list(size = 6, color = cores[i]),
            hoverinfo = "text",
            text = hover_text
          )
      }
    }
    
    # Adicionar metas se solicitado
    if (input$mostrar_metas) {
      metas <- data.frame(
        pontos = c(70, 60, 45),
        nome = c("Título", "Libertadores", "Fuga do Z4"),
        cor = c("#FF6B6B", "#4ECDC4", "#FFD166")
      )
      
      for(i in 1:nrow(metas)) {
        # Calcular projeção linear ao longo das rodadas
        x_min <- input$rodada_range[1]
        x_max <- input$rodada_range[2]
        y_min <- (metas$pontos[i] * x_min / 38)
        y_max <- (metas$pontos[i] * x_max / 38)
        
        p <- p %>%
          add_trace(
            x = c(x_min, x_max),
            y = c(y_min, y_max),
            type = "scatter",
            mode = "lines",
            name = paste(metas$pontos[i], "pts -", metas$nome[i]),
            line = list(
              color = metas$cor[i], 
              width = 2, 
              dash = "dash"
            ),
            hoverinfo = "text",
            text = paste(
              "<b>Meta: ", metas$pontos[i], " pontos</b><br>",
              metas$nome[i], "<br>",
              "Projeção linear de ", round(y_min, 1), " a ", round(y_max, 1), " pontos"
            ),
            showlegend = TRUE
          )
      }
    }
    
    p
  })
  
  # Estatísticas
  output$estatisticas <- renderText({
    if (length(input$times_selecionados) == 0) {
      return("Selecione times para ver estatísticas")
    }
    
    dados_finais <- dados_filtrados() %>%
      filter(rodada_cronologica == max(rodada_cronologica)) %>%
      arrange(desc(pontos_acumulados_cronologico))
    
    if (nrow(dados_finais) == 0) return("Nenhum dado disponível para o filtro selecionado")
    
    paste(
      "=== ESTATÍSTICAS ===\n",
      "Rodada: ", max(dados_filtrados()$rodada_cronologica), "\n",
      "Times analisados: ", length(unique(dados_filtrados()$time_referencia)), "\n\n",
      "TOP 1: ", dados_finais$time_referencia[1], 
      " - ", dados_finais$pontos_acumulados_cronologico[1], " pts\n",
      "ÚLTIMO: ", dados_finais$time_referencia[nrow(dados_finais)], 
      " - ", dados_finais$pontos_acumulados_cronologico[nrow(dados_finais)], " pts\n",
      "DIFERENÇA: ", 
      dados_finais$pontos_acumulados_cronologico[1] - dados_finais$pontos_acumulados_cronologico[nrow(dados_finais)], 
      " pts\n\n",
      "Média de pontos: ", 
      round(mean(dados_finais$pontos_acumulados_cronologico), 1), " pts"
    )
  })
  
  # Tabela de dados (também atualizada para incluir placar se existir)
  output$tabela_dados <- renderDataTable({
    if (length(input$times_selecionados) == 0) {
      return(data.frame(Mensagem = "Selecione times para ver os dados"))
    }
    
    # Verificar se a variável placar existe
    if ("placar" %in% names(dados_filtrados())) {
      dados_filtrados() %>%
        select(
          Rodada = rodada_cronologica, 
          Time = time_referencia, 
          `Pontos Acumulados` = pontos_acumulados_cronologico,
          `Pontos na Rodada` = pontos_rodada,
          Posição = colocacao_referencia,
          Placar = placar
        ) %>%
        arrange(Rodada, `Posição`)
    } else {
      dados_filtrados() %>%
        select(
          Rodada = rodada_cronologica, 
          Time = time_referencia, 
          `Pontos Acumulados` = pontos_acumulados_cronologico,
          `Pontos na Rodada` = pontos_rodada,
          Posição = colocacao_referencia
        ) %>%
        arrange(Rodada, `Posição`)
    }
  }, 
  options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 20, 50),
    searchHighlight = TRUE
  ))
}

shinyApp(ui = ui, server = server)
