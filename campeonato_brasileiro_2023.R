
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

write_xlsx(base_final, "base_brasileiro.xlsx")



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
    breaks = seq(0, max(br_2023_cima$rodada_cronologica, na.rm = TRUE), by = 4),
    limits = c(0, max(br_2023_cima$rodada_cronologica, na.rm = TRUE))
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
  # Pontos com símbolos PREENCHIDOS
  geom_point(aes(shape = resultado_referencia), 
             size = 2, data = ~ filter(., rodada_cronologica > 0)) +  # size maior para melhor visualização
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
    breaks = seq(0, max(br_2023_cima$rodada_cronologica, na.rm = TRUE), by = 4),
    limits = c(0, max(br_2023_cima$rodada_cronologica, na.rm = TRUE))
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







# Extra ----


# Criar dados para a linha de referência
linha_referencia <- data.frame(
  x = c(0, 38),
  y = c(0, 70)
)

# Gráfico com a linha de referência
ggplot(br_2023, aes(x = rodada, y = pontos_acumulados, 
                  color = time_referencia)) +
  # Linhas dos times
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 1.5) +
  # Linha de referência (0,0) a (38,70)
  geom_line(data = linha_referencia, 
            aes(x = x, y = y),
            color = "black", 
            linewidth = 1.2,
            linetype = "dashed",
            alpha = 0.5) +
  # Ponto no final da linha de referência
  geom_point(data = linha_referencia %>% filter(x == 38),
             aes(x = x, y = y),
             color = "black",
             size = 3,
             shape = 21,
             fill = "white",
             stroke = 1.5) +
  # Rótulo para o ponto (38,70)
  geom_text(data = linha_referencia %>% filter(x == 38),
            aes(x = x, y = y, label = "70 pontos"),
            color = "black",
            hjust = -0.2,
            vjust = 0.5,
            size = 4,
            fontface = "bold") +
  labs(title = "Evolução dos Pontos Acumulados por Rodada",
       subtitle = "Linha tracejada: meta de 70 pontos ao final do campeonato",
       x = "Rodada",
       y = "Pontos Acumulados",
       color = "Time") +
  theme_minimal() 


# Criar dados para as linhas de referência
linhas_referencia <- data.frame(
  meta = rep(c("70 pontos", "60 pontos", "45 pontos"), each = 2),
  x = c(0, 38, 0, 38, 0, 38),
  y = c(0, 70, 0, 60, 0, 45)
)

# Definir cores para cada meta
cores_metas <- c("70 pontos" = "#FF6B6B",   # Vermelho
                 "60 pontos" = "#4ECDC4",    # Turquesa
                 "45 pontos" = "#FFD166")    # Amarelo

# Criar um gráfico com legendas separadas para times e metas
ggplot(br_2023, aes(x = rodada_cronologica, y = pontos_acumulados_cronologico)) +
  # 1. Linhas dos times (primeira camada)
  geom_line(aes(group = time_referencia, color = time_referencia),
            linewidth = 1, alpha = 0.7) +
  geom_point(aes(color = time_referencia), size = 1.5) +
  
  # 2. Linhas de referência (segunda camada)
  geom_line(data = data.frame(x = rep(c(0, 38), 3),
                              y = c(0, 70, 0, 60, 0, 45),
                              meta = rep(c("70 pts", "60 pts", "45 pts"), each = 2)),
            aes(x = x, y = y, linetype = meta, color = meta),
            linewidth = 1.2) +
  
  # 3. Marcadores nas rodadas específicas
  # Linha horizontal na rodada 38 para marcar os limiares
  geom_segment(data = data.frame(y = c(45, 60, 70)),
               aes(x = 36, xend = 38, y = y, yend = y, color = as.character(y)),
               linetype = "solid",
               linewidth = 0.8) +
  
  # 4. Rótulos das metas
  annotate("text", x = 39, y = 45, 
           label = "45 pts\nSegurança", 
           hjust = 0, vjust = 0.5, size = 3.5,
           fontface = "bold", lineheight = 0.9, color = "#FFD166") +
  annotate("text", x = 39, y = 60, 
           label = "60 pts\nSul-Americana", 
           hjust = 0, vjust = 0.5, size = 3.5,
           fontface = "bold", lineheight = 0.9, color = "#4ECDC4") +
  annotate("text", x = 39, y = 70, 
           label = "70 pts\nLibertadores", 
           hjust = 0, vjust = 0.5, size = 3.5,
           fontface = "bold", lineheight = 0.9, color = "#FF6B6B") +
  
  # 5. Configurações estéticas
  labs(title = "Evolução dos Pontos Acumulados - Brasileirão 2023",
       subtitle = "Linhas de referência: metas comuns no campeonato brasileiro",
       x = "Rodada",
       y = "Pontos Acumulados",
       color = "Times") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 42), breaks = seq(0, 38, 5)) +
  scale_y_continuous(limits = c(0, max(br_2023$pontos_acumulados, 70) * 1.1)) +
  scale_color_manual(
    name = "Times",
    values = c(setNames(rainbow(length(unique(br_2023$time_referencia))),
                        unique(br_2023$time_referencia)),
               "45 pts" = "#FFD166",
               "60 pts" = "#4ECDC4",
               "70 pts" = "#FF6B6B")
  ) +
  scale_linetype_manual(
    name = "Metas",
    values = c("45 pts" = "dashed",
               "60 pts" = "dashed",
               "70 pts" = "dashed")
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(linetype = "solid")),
    linetype = guide_legend(order = 2)
  ) +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.spacing.y = unit(0.5, "cm"))


# Interativo

# Criar cores para os times
cores_times <- setNames(
  viridis::viridis(length(unique(br_2023$time_referencia))),
  unique(br_2023$time_referencia)
)

# Iniciar gráfico vazio
p_interativo2 <- plot_ly() %>%
  layout(
    title = list(
      text = "<b>Evolução dos Pontos Acumulados - Brasileirão 2023</b><br>
      <sup>Linhas de referência: metas comuns no campeonato brasileiro</sup>",
      x = 0.05
    ),
    xaxis = list(
      title = "Rodada",
      range = c(0, 42),
      dtick = 5,
      gridcolor = "lightgray"
    ),
    yaxis = list(
      title = "Pontos Acumulados",
      range = c(0, max(br_2023$pontos_acumulados, 70) * 1.1),
      gridcolor = "lightgray"
    ),
    hovermode = "closest",
    showlegend = TRUE,
    legend = list(
      orientation = "v",
      y = 0.9,
      x = 1.05,
      title = list(text = "<b>Times</b>")
    )
  )

# Adicionar linhas de cada time
for(time in unique(br_2023$time_referencia)) {
  dados_time <- br_2023 %>% filter(time_referencia == time)
  
  p_interativo2 <- p_interativo2 %>%
    add_trace(
      data = dados_time,
      x = ~rodada,
      y = ~pontos_acumulados,
      type = "scatter",
      mode = "lines+markers",
      name = time,
      line = list(color = cores_times[time], width = 2),
      marker = list(color = cores_times[time], size = 4),
      hoverinfo = "text",
      text = ~paste(
        "<b>", time, "</b><br>",
        "Rodada: ", rodada, "<br>",
        "Pontos acumulados: ", pontos_acumulados, "<br>",
        "Pontos na rodada: ", pontos_rodada
      ),
      legendgroup = "times"
    )
}

# Adicionar linhas de meta
metas <- list(
  list(pontos = 70, cor = "#FF6B6B", nome = "70 pts - Libertadores"),
  list(pontos = 60, cor = "#4ECDC4", nome = "60 pts - Sul-Americana"),
  list(pontos = 45, cor = "#FFD166", nome = "45 pts - Segurança")
)

for(meta in metas) {
  p_interativo2 <- p_interativo2 %>%
    add_trace(
      x = c(0, 38),
      y = c(0, meta$pontos),
      type = "scatter",
      mode = "lines",
      name = meta$nome,
      line = list(color = meta$cor, width = 2, dash = "dash"),
      hoverinfo = "text",
      text = paste0(
        "<b>", meta$nome, "</b><br>",
        "Meta: ", meta$pontos, " pontos na 38ª rodada<br>",
        "Ritmo necessário: ", round(meta$pontos/38, 2), " pontos/rodada"
      ),
      legendgroup = "metas",
      showlegend = TRUE
    )
}

# Adicionar anotações para as metas
p_interativo2 <- p_interativo2 %>%
  add_annotations(
    x = 39,
    y = 45,
    text = "45 pts<br><i>Segurança</i>",
    xref = "x",
    yref = "y",
    showarrow = FALSE,
    font = list(size = 11, color = "#FFD166"),
    align = "left"
  ) %>%
  add_annotations(
    x = 39,
    y = 60,
    text = "60 pts<br><i>Sul-Americana</i>",
    xref = "x",
    yref = "y",
    showarrow = FALSE,
    font = list(size = 11, color = "#4ECDC4"),
    align = "left"
  ) %>%
  add_annotations(
    x = 39,
    y = 70,
    text = "70 pts<br><i>Libertadores</i>",
    xref = "x",
    yref = "y",
    showarrow = FALSE,
    font = list(size = 11, color = "#FF6B6B"),
    align = "left"
  )

# Adicionar botões de interação
p_interativo2 <- p_interativo2 %>%
  layout(
    updatemenus = list(
      list(
        type = "buttons",
        direction = "down",
        x = 1.15,
        y = 0.2,
        buttons = list(
          list(
            method = "relayout",
            args = list(list(showlegend = TRUE)),
            label = "Mostrar tudo"
          ),
          list(
            method = "restyle",
            args = list("visible", "legendonly"),
            args2 = list("visible", TRUE),
            label = "Apenas times"
          ),
          list(
            method = "restyle",
            args = list("visible", "legendonly"),
            args2 = list("visible", TRUE),
            label = "Apenas metas"
          )
        )
      )
    )
  )

# Mostrar gráfico
p_interativo2



# Shiny ----

br_2023 = base_final |> filter(ano_campeonato == 2023, time_referencia %in% c("Botafogo", 'Palmeiras') )

ui <- fluidPage(
  titlePanel("Pontos Acumulados - Brasileirão 2023"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros"),
      # Seletor de times
      checkboxGroupInput(
        inputId = "times_selecionados",
        label = "Selecionar Times:",
        choices = unique(br_2023$time_referencia),
        selected = unique(br_2023$time_referencia)[1:4]  # Mostrar 4 times inicialmente
      ),
      # Botões de ação rápida
      actionButton("todos_times", "Todos os Times", width = "100%"),
      actionButton("top4", "Top 4", width = "100%"),
      actionButton("limpar", "Limpar Seleção", width = "100%"),
      hr(),
      # Opções de visualização
      checkboxInput("mostrar_metas", "Mostrar Metas", value = TRUE),
      checkboxInput("mostrar_pontos", "Mostrar Pontos", value = TRUE),
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
  
  observeEvent(input$top4, {
    # Encontrar os 4 times com mais pontos no final
    top4_times <- br_2023 %>%
      filter(rodada == 38) %>%
      arrange(desc(pontos_acumulados)) %>%
      head(4) %>%
      pull(time_referencia)
    
    updateCheckboxGroupInput(
      session, "times_selecionados",
      selected = top4_times
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
        rodada >= input$rodada_range[1],
        rodada <= input$rodada_range[2]
      )
  })
  
  # Gráfico
  output$grafico <- renderPlotly({
    if (length(input$times_selecionados) == 0) {
      return(plotly_empty() %>% 
               layout(title = "Selecione pelo menos um time"))
    }
    
    p <- plot_ly() %>%
      layout(
        title = paste("Pontos Acumulados -", length(input$times_selecionados), "times selecionados"),
        xaxis = list(title = "Rodada"),
        yaxis = list(title = "Pontos Acumulados"),
        hovermode = "closest"
      )
    
    # Adicionar linhas dos times
    if (input$mostrar_pontos) {
      for (time in unique(dados_filtrados()$time_referencia)) {
        dados_time <- dados_filtrados() %>% filter(time_referencia == time)
        
        p <- p %>%
          add_trace(
            data = dados_time,
            x = ~rodada,
            y = ~pontos_acumulados,
            type = "scatter",
            mode = "lines+markers",
            name = time,
            line = list(width = 3),
            marker = list(size = 6),
            hoverinfo = "text",
            text = ~paste(
              "<b>", time_referencia, "</b><br>",
              "Rodada: ", rodada, "<br>",
              "Pontos Acumulados: ", pontos_acumulados, "<br>",
              "Posição na rodada: ", colocacao_referencia
            )
          )
      }
    }
    
    # Adicionar metas se solicitado
    if (input$mostrar_metas) {
      metas <- data.frame(
        pontos = c(70, 60, 45),
        nome = c("Libertadores", "Sul-Americana", "Segurança"),
        cor = c("#FF6B6B", "#4ECDC4", "#FFD166")
      )
      
      for(i in 1:nrow(metas)) {
        p <- p %>%
          add_trace(
            x = c(input$rodada_range[1], input$rodada_range[2]),
            y = c(metas$pontos[i] * input$rodada_range[1]/38, 
                  metas$pontos[i] * input$rodada_range[2]/38),
            type = "scatter",
            mode = "lines",
            name = paste(metas$pontos[i], "pontos -", metas$nome[i]),
            line = list(color = metas$cor[i], width = 2, dash = "dash"),
            hoverinfo = "text",
            text = paste("<b>Meta:", metas$pontos[i], "pontos</b><br>", metas$nome[i]),
            showlegend = TRUE
          )
      }
    }
    
    p
  })
  
  # Estatísticas
  output$estatisticas <- renderText({
    if (length(input$times_selecionados) == 0) return("Nenhum time selecionado")
    
    dados_finais <- dados_filtrados() %>%
      filter(rodada == max(rodada)) %>%
      arrange(desc(pontos_acumulados))
    
    paste(
      "Estatísticas (Rodada", max(dados_filtrados()$rodada), "):\n",
      "1º:", dados_finais$time_referencia[1], "-", dados_finais$pontos_acumulados[1], "pts\n",
      "Último:", dados_finais$time_referencia[nrow(dados_finais)], 
      "-", dados_finais$pontos_acumulados[nrow(dados_finais)], "pts\n",
      "Diferença:", dados_finais$pontos_acumulados[1] - 
        dados_finais$pontos_acumulados[nrow(dados_finais)], "pts"
    )
  })
  
  # Tabela de dados
  output$tabela_dados <- renderDataTable({
    dados_filtrados() %>%
      select(Rodada = rodada, Time = time_referencia, 
             `Pontos Acumulados` = pontos_acumulados,
             `Pontos na Rodada` = pontos_rodada,
             Posição = colocacao_referencia)
  })
}

shinyApp(ui = ui, server = server)

