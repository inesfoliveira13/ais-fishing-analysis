# Bibliotecas
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


# Carregar os dados
dataset <- read.csv("dataset_velocidades_viagem_localizacao.csv")

summary(dataset)

########################################## Análise dos critérios separadamente ###########################

# Quantidade de viagens
length(unique(dataset$TRIP_ID))

# Calcular estatísticas por TRIP_ID
viagens_info <- dataset %>%
  group_by(TRIP_ID) %>%
  summarise(
    duracao = max(Tempo_Relativo_Horas, na.rm = TRUE),
    n_registos = n(),
    vel_inicial = first(Velocidade),
    vel_final = last(Velocidade)
  ) %>%
  ungroup()

total_viagens <- nrow(viagens_info)

### --- CRITÉRIO 1: Duração entre 2.5 e 24 horas
validas_criterio1 <- viagens_info %>%
  filter(duracao > 2.5, duracao < 24)

n_validas1 <- nrow(validas_criterio1)
percentagem_filtrada1 <- 100 * (1 - n_validas1 / total_viagens)


### --- CRITÉRIO 2: (Nº registos) / duração >= 2
validas_criterio2 <- viagens_info %>%
  filter((n_registos / duracao) >= 2)

n_validas2 <- nrow(validas_criterio2)
percentagem_filtrada2 <- 100 * (1 - n_validas2 / total_viagens)


### --- CRITÉRIO 3: Velocidade inicial e final <= 0.5 nós
validas_criterio3 <- viagens_info %>%
  filter(vel_inicial <= 0.5 & vel_final <= 0.5)

n_validas3 <- nrow(validas_criterio3)
percentagem_filtrada3 <- 100 * (1 - n_validas3 / total_viagens)

# Resultados resumidos
resultados <- tibble(
  Criterio = c(
    "Duração > 2.5h e < 24h",
    "n_registos / duração >= 2",
    "Velocidade inicial e final <= 0.5 nós"
  ),
  Viagens_validas = c(n_validas1, n_validas2, n_validas3),
  Percentagem_eliminada = c(percentagem_filtrada1, percentagem_filtrada2, percentagem_filtrada3)
)

print(resultados)


########################################## Análise dos critérios simultaneamente #########################

# Aplicar todos os critérios simultaneamente
viagens_validas_todos <- viagens_info %>%
  filter(
    duracao > 2.5,
    duracao < 24,
    (n_registos / duracao) >= 2,
    vel_inicial <= 0.5 & vel_final <= 0.5
  )

n_validas_todos <- nrow(viagens_validas_todos)
percentagem_eliminada_todos <- 100 * (1 - n_validas_todos / total_viagens)

# Filtrar o dataset original com TRIP_IDs válidos
dataset_filtrado <- dataset %>%
  filter(TRIP_ID %in% viagens_validas_todos$TRIP_ID)

percentagem_registos_eliminados <- 100 * (1 - nrow(dataset_filtrado) / nrow(dataset))

# Mostrar resultados
resultados_finais <- tibble(
  Viagens_totais = total_viagens,
  Viagens_validas = n_validas_todos,
  Percentagem_viagens_eliminadas = percentagem_eliminada_todos,
  Registos_totais = nrow(dataset),
  Registos_finais = nrow(dataset_filtrado),
  Percentagem_registos_eliminados = percentagem_registos_eliminados
)

print(resultados_finais)


########################################## Guardar os datasets filtrados ###############################

# Dataset com viagens válidas
dataset_viagens_validas <- dataset %>%
  filter(TRIP_ID %in% viagens_validas_todos$TRIP_ID)

# Dataset com viagens eliminadas
dataset_viagens_eliminadas <- dataset %>%
  filter(!TRIP_ID %in% viagens_validas_todos$TRIP_ID)

# Guardar os dois datasets como CSV
write.csv(dataset_viagens_validas, "viagens_validas.csv", row.names = FALSE)
write.csv(dataset_viagens_eliminadas, "viagens_eliminadas.csv", row.names = FALSE)


########################################## Análise  de embarcações ###################################

# Identificar embarcações com viagens inválidas
embarcacoes_com_viagens_invalidas <- dataset_viagens_eliminadas %>%
  distinct(Code)

# Identificar embarcações com viagens válidas
embarcacoes_com_viagens_validas <- dataset_viagens_validas %>%
  distinct(Code)

# Embarcações que não têm NENHUMA viagem válida
embarcacoes_sem_viagens_validas <- embarcacoes_com_viagens_invalidas %>%
  filter(!(Code %in% embarcacoes_com_viagens_validas$Code))

# Mostrar resultados
cat("Número de embarcações com pelo menos uma viagem inválida:", nrow(embarcacoes_com_viagens_invalidas), "\n")
cat("Número de embarcações sem nenhuma viagem válida:", nrow(embarcacoes_sem_viagens_validas), "\n")

# Visualizar embarcações sem viagens válidas
print(embarcacoes_sem_viagens_validas)

# Contagem de viagens válidas por embarcação
validas_por_embarcacao <- dataset_viagens_validas %>%
  distinct(Code, TRIP_ID) %>%
  count(Code, name = "viagens_validas")

# Contagem de viagens inválidas por embarcação
invalidas_por_embarcacao <- dataset_viagens_eliminadas %>%
  distinct(Code, TRIP_ID) %>%
  count(Code, name = "viagens_invalidas")

# Juntar os dois dataframes e calcular percentagem
resumo_viagens_por_embarcacao <- full_join(validas_por_embarcacao, invalidas_por_embarcacao, by = "Code") %>%
  mutate(
    viagens_validas = replace_na(viagens_validas, 0),
    viagens_invalidas = replace_na(viagens_invalidas, 0),
    total_viagens = viagens_validas + viagens_invalidas,
    percentagem_validas = round((viagens_validas / total_viagens) * 100, 2)
  )

# Mostrar o resumo
print(resumo_viagens_por_embarcacao)



########################################## Criar a coluna "Lance" #######################################

# Usar o dataset com viagens válidas
dataset_lances <- dataset_viagens_validas %>%
  arrange(TRIP_ID, DATE.TIME..UTC.)  # Ordenar por viagem e tempo



marcar_lances <- function(df) {
  df <- df %>% arrange(DATE.TIME..UTC.)
  
  idx_validos <- which(df$Velocidade >= 2)
  
  if (length(idx_validos) < 1) {
    df$fase_extracao <- 0
    df$inicio_lancamento <- 0
    df$duracao_lance <- NA
    return(df)
  }
  
  inicio <- min(idx_validos)
  fim <- max(idx_validos)
  
  df_util <- df[inicio:fim, ] %>%
    mutate(low_speed = Velocidade < 1)
  
  rle_result <- rle(df_util$low_speed)
  grupo_ids <- rep(seq_along(rle_result$lengths), rle_result$lengths)
  df_util$grupo <- grupo_ids
  
  df$fase_extracao <- 0
  df$inicio_lancamento <- 0
  df$duracao_lance <- NA
  
  for (g in unique(df_util$grupo[df_util$low_speed])) {
    grupo_df <- df_util %>% filter(grupo == g)
    
    idx_primeiro_util <- which(df_util$DATE.TIME..UTC. == min(grupo_df$DATE.TIME..UTC.))
    idx_ultimo_util <- which(df_util$DATE.TIME..UTC. == max(grupo_df$DATE.TIME..UTC.))
    
    idx_primeiro <- idx_primeiro_util + inicio - 1
    idx_ultimo <- idx_ultimo_util + inicio - 1
    idx_anterior <- idx_primeiro - 1
    
    if (idx_anterior >= 1) {
      tempo_inicio <- df$DATE.TIME..UTC.[idx_anterior]
    } else {
      tempo_inicio <- df$DATE.TIME..UTC.[idx_primeiro]
    }
    tempo_fim <- df$DATE.TIME..UTC.[idx_ultimo]
    
    duracao <- as.numeric(difftime(tempo_fim, tempo_inicio, units = "hours"))
    
    if (duracao >= 0.75) {
      df$fase_extracao[idx_primeiro:idx_ultimo] <- 1
      if (idx_anterior >= 1) df$inicio_lancamento[idx_anterior] <- 1
      idx_total <- c(idx_anterior, idx_primeiro:idx_ultimo)
      df$duracao_lance[idx_total] <- duracao
    }
  }
  
  return(df)
}



# Aplicar função a cada TRIP_ID
dataset_lances_marcado <- dataset_lances %>%
  group_by(TRIP_ID) %>%
  group_modify(~ marcar_lances(.x)) %>%
  ungroup()


# Criar coluna lance_integrado 
dataset_lances_marcado <- dataset_lances_marcado %>%
  mutate(lance_integrado = if_else(fase_extracao == 1 | inicio_lancamento == 1, 1, 0))

## Registo marcado como parte da fase_extracao (1) ou como inicio_lancamento (1) pertence a um lance_integrado (1),
## caso contrário então (0)


# Verificar resultado
table(dataset_lances_marcado$fase_extracao)

# Exportar para CSV
write.csv(dataset_lances_marcado, "viagens_validas_com_lances.csv", row.names = FALSE)


################################ Visualização dos Lances duma Viagem ####################################

# Escolher um TRIP_ID aleatório com pelo menos um lance (lance == 1)
trip_ids_com_lance <- dataset_lances_marcado %>%
  filter(fase_extracao == 1) %>%
  pull(TRIP_ID) %>%
  unique()

set.seed(23) # Vessel_278_762
#set.seed(60) 

trip_id_escolhido <- sample(trip_ids_com_lance, 1)

cat("Viagem aleatória com fase_extracao selecionada:", trip_id_escolhido, "\n")

# Mostrar os registos com fase_extracao == 1 nessa viagem
lance_escolhido <- dataset_lances_marcado %>%
  filter(TRIP_ID == trip_id_escolhido, fase_extracao == 1)

print(lance_escolhido)


# Filtrar os dados dessa viagem
dados_viagem <- dataset_lances_marcado %>%
  filter(TRIP_ID == trip_id_escolhido)

# Converter a data para o formato POSIXct
dados_viagem$DATE.TIME..UTC. <- as.POSIXct(dados_viagem$DATE.TIME..UTC.)


# Gráfico 
ggplot(dados_viagem, aes(x = DATE.TIME..UTC., y = Velocidade)) +
  geom_line(color = "steelblue") +
  geom_point(data = subset(dados_viagem, fase_extracao == 1),
             aes(x = DATE.TIME..UTC., y = Velocidade),
             color = "red", size = 2) +
  geom_point(data = subset(dados_viagem, inicio_lancamento == 1),
             aes(x = DATE.TIME..UTC., y = Velocidade),
             color = "blue", size = 3, shape = 17) +  # Triângulo azul para ponto anterior
  labs(
    title = paste("Velocidade ao longo do tempo para", trip_id_escolhido),
    subtitle = "Vermelho = Alagem e Enxugagem da Rede e Transbordo de Pescado; Azul = Largada da Rede",
    x = "Data/Hora",
    y = "Velocidade (nós)"
  ) +
  theme_minimal()



#########################################################################################################
