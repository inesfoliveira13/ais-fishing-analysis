# Bibliotecas
library(lubridate)
library(ggplot2)
library(dplyr)
library(geosphere)
library(purrr)
library(RColorBrewer)

# Carregar o dataset
dataset_trip_id <- read.csv("dataset_trip_id.csv")
dataset_velocidades_viagem_localizacao <- read.csv("dataset_velocidades_viagem_localizacao.csv")

################################### Análise do dataset_trip_id ################################## 

#head(dataset_trip_id)
#dim(dataset_trip_id)
#str(dataset_trip_id)
summary(dataset_trip_id)
summary(dataset_velocidades_viagem_localizacao)


# Linha(s) nas quais a "Duracao_Viagem" é máxima (56914.76) (quase 6 anos) 
dataset_trip_id[which.max(dataset_trip_id$Duracao_Viagem), ]
## Vessel_229_1                 

# Linha(s) nas quais a "Duracao_Viagem" é mínima (0.0) 
dataset_trip_id[dataset_trip_id$Duracao_Viagem == 0.0, ]


#tail(sort(dataset_trip_id$Duracao_Viagem))
#tail(sort(dataset_trip_id_limpo$Duracao_Viagem))


############################### Histogramas antes e depois da limpeza ##########################
############################### das linhas com Duracao_Viagem == 0 #############################


# Converter a coluna para numérica
dataset_trip_id$Duracao_Viagem <- as.numeric(dataset_trip_id$Duracao_Viagem)


# Histograma antes de eliminar viagens com Duração igual a 0
hist_before <- hist(dataset_trip_id$Duracao_Viagem, 
                    main = "Histograma da Duração das Viagens (Antes da Limpeza)", 
                    xlab = "Duração da Viagem (horas)", 
                    col = "lightblue", 
                    border = "black")


# Filtrar as linhas em que Duracao_Viagem é 0
linhas_duracao_zero <- dataset_trip_id[dataset_trip_id$Duracao_Viagem == 0, ]
#linhas_duracao_zero

# Remover as linhas em que Duracao_Viagem é 0
dataset_trip_id_limpo <- dataset_trip_id[dataset_trip_id$Duracao_Viagem != 0, ]
#dataset_trip_id_limpo


# Histograma depois de eliminar viagens com Duração igual a 0
hist_after <- hist(dataset_trip_id_limpo$Duracao_Viagem, 
                   main = "Histograma da Duração das Viagens (Após a Limpeza)", 
                   xlab = "Duração da Viagem (horas)", 
                   col = "lightgreen", 
                   border = "black")


# Resumo estatístico do dataset antes da limpeza
summary(dataset_trip_id[, c("Registos_Viagem", "Velocidade_Minima", "Velocidade_Media", "Velocidade_Maxima", "Duracao_Viagem")])

# Resumo estatístico do dataset depois da limpeza
summary(dataset_trip_id_limpo[, c("Registos_Viagem", "Velocidade_Minima", "Velocidade_Media", "Velocidade_Maxima", "Duracao_Viagem")])



# Boxplot da duração de todas as viagens
bp <- boxplot(dataset_trip_id_limpo$Duracao_Viagem,
              main = "Boxplot da Duração das Viagens",
              ylab = "Duração (horas)",
              col = "skyblue",
              border = "darkblue",
              horizontal = TRUE)

# Obter os outliers
outliers <- bp$out

# Calcular a frequência dos outliers
frequencia_outliers <- table(outliers)
frequencia_outliers



# 1. Filtrar e visualizar viagens com duração ≤ 10 horas

# Filtrar viagens muito curtas
viagens_curtas <- subset(dataset_trip_id_limpo, Duracao_Viagem <= 10)

# Histograma para essas viagens
hist(viagens_curtas$Duracao_Viagem,
     main = "Distribuição das Viagens com Duração ≤ 10 horas",
     xlab = "Duração da Viagem (horas)",
     col = "lightblue",
     border = "black")


# 2. Filtrar e visualizar viagens com duração entre 10 a 20 horas

# Filtrar viagens longas
viagens_medias <- subset(dataset_trip_id_limpo, Duracao_Viagem > 10 & Duracao_Viagem <= 20)

# Histograma para essas viagens
hist(viagens_medias$Duracao_Viagem,
     main = "Distribuição das Viagens com Duração entre 10 e 20 horas",
     xlab = "Duração da Viagem (horas)",
     col = "tomato",
     border = "black")


# 3. Filtrar e visualizar viagens com duração entre 20 a 45 horas (a duração máxima de viagem é 43.700)

# Filtrar viagens longas
viagens_longas <- subset(dataset_trip_id_limpo, Duracao_Viagem > 20 & Duracao_Viagem <= 45)

# Histograma para essas viagens
hist(viagens_longas$Duracao_Viagem,
     main = "Distribuição das Viagens com Duração > 20 horas",
     xlab = "Duração da Viagem (horas)",
     col = "tomato",
     border = "black")


# 4. Resumo estatístico de cada grupo

summary(viagens_curtas$Duracao_Viagem)
summary(viagens_medias$Duracao_Viagem)
summary(viagens_longas$Duracao_Viagem)

# Resumo estatístico conjunto

viagens = rbind(viagens_curtas, viagens_medias, viagens_longas)
summary(viagens$Duracao_Viagem)


# 5. Resumo da duração das viagens por embarcação

# Resumo das viagens curtas por embarcação
resumo_curtas <- table(viagens_curtas$Code)
resumo_curtas

# Resumo das viagens médias por embarcação
resumo_medias <- table(viagens_medias$Code)
resumo_medias

# Resumo das viagens longas por embarcação
resumo_longas <- table(viagens_longas$Code)
resumo_longas


# Extrair número de 'Code' e ordenar numericamente
dataset_trip_id_limpo <- dataset_trip_id_limpo[order(as.numeric(gsub("Vessel_", "", dataset_trip_id_limpo$Code))), ]
#dataset_trip_id_limpo


# 6. Gráficos de barras da velocidade média por embarcação e Boxplot da Duração das Viagens por Embarcação

# Definir o diretório onde os gráficos serão guardados
setwd("C:/Users/carlo/OneDrive/Documentos/Ambiente de Trabalho/Material Escolar/Projeto_grupo2")

# Obter os valores únicos da coluna 'Code'
valores_unicos <- unique(dataset_trip_id_limpo$Code); valores_unicos

# Dividir os valores únicos em 2 listas maiores
num_listas <- 2
listas <- split(valores_unicos, cut(seq_along(valores_unicos), num_listas, labels = FALSE))


# 6.1. Gráfico de barras da velocidade média por embarcação
gerar_grafico_barras <- function(embarcacoes_atual, i) {
  dataset_filtrado <- dataset_trip_id_limpo[dataset_trip_id_limpo$Code %in% embarcacoes_atual, ]
  
  # Reordenar os fatores da coluna 'Code' com base no número
  dataset_filtrado$Code <- factor(dataset_filtrado$Code, 
                                  levels = unique(dataset_filtrado$Code[order(as.numeric(gsub("Vessel_", "", dataset_filtrado$Code)))]))
  
  grafico <- ggplot(dataset_filtrado, aes(x = Code, y = Velocidade_Media, fill = Code)) +
    geom_bar(stat = "summary", fun = "mean", show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Velocidade Média por Embarcação - Lista", i),
         x = "Embarcação", y = "Velocidade Média (nós)")
  
  # Guardar imagem
  jpeg(paste0("velocidade_media_lista_", i, ".jpg"), width = 16, height = 10, units = 'cm', res = 400)
  print(grafico)
  dev.off()
}


# 6.2. Boxplot da Duração das Viagens por Embarcação
gerar_boxplot <- function(embarcacoes_atual, i, max_duracao) {
  dataset_filtrado <- dataset_trip_id_limpo[dataset_trip_id_limpo$Code %in% embarcacoes_atual, ]
  
  # Reordenar os fatores
  dataset_filtrado$Code <- factor(dataset_filtrado$Code, 
                                  levels = unique(dataset_filtrado$Code[order(as.numeric(gsub("Vessel_", "", dataset_filtrado$Code)))]))
  
  grafico <- ggplot(dataset_filtrado, aes(x = Code, y = Duracao_Viagem)) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    coord_cartesian(ylim = c(0, max_duracao)) +  # mesma escala
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Distribuição da Duração das Viagens por Embarcação - Lista", i),
         x = "Embarcação", y = "Duração da Viagem (horas)")
  
  # Guardar imagem
  jpeg(paste0("duracao_viagem_lista_", i, ".jpg"), width = 16, height = 10, units = 'cm', res = 400)
  print(grafico)
  dev.off()
}

# Calcular máximo antes de gerar os gráficos
max_duracao <- max(dataset_trip_id_limpo$Duracao_Viagem, na.rm = TRUE)

# Gerar e guardar os gráficos para cada lista
for (i in seq_along(listas)) {
  gerar_grafico_barras(listas[[i]], i)
  gerar_boxplot(listas[[i]], i, max_duracao)
}


############################### Correlação ################################

# Selecionar as variáveis de interesse
variaveis_interesse <- dataset_trip_id_limpo[, c("Duracao_Viagem", "Velocidade_Media", "Registos_Viagem")]

# Calcular a matriz de correlação entre as variáveis
correlacao <- cor(variaveis_interesse)
correlacao

# Visualizar a correlação com um gráfico de dispersão
pairs(variaveis_interesse,
      main = "Gráfico de Dispersão - Correlação entre Duração, Velocidade Média e Registos",
      pch = 19, col = "blue")


################################### Fim da Análise do dataset_trip_id ################################## 


################################### Análise do dataset_velocidades_viagem_localizacao ################## 


#head(dataset_velocidades_viagem_localizacao)
#dim(dataset_velocidades_viagem_localizacao)
#str(dataset_velocidades_viagem_localizacao)
summary(dataset_velocidades_viagem_localizacao)

# Contabiliza quantos registos existem por TRIP_ID
contagens <- dataset_velocidades_viagem_localizacao %>%
  count(TRIP_ID)

# Filtra os TRIP_ID que aparecem exatamente 3 vezes
trip_ids_unicos <- contagens %>%
  filter(n == 3) %>%
  pull(TRIP_ID)

# Filtra o DataFrame original para manter apenas essas TRIP_ID
registos_unicos <- dataset_velocidades_viagem_localizacao %>%
  filter(TRIP_ID %in% trip_ids_unicos)

# Mostrar resultado
print(registos_unicos)

# Viagens com 1, 2 ou 3 registos ---

# Filtra TRIP_IDs com 1, 2 ou 3 registos
trip_ids_filtradas <- contagens %>%
  filter(n %in% c(1, 2, 3)) %>%
  pull(TRIP_ID)

# Filtrar dataset original para manter apenas essas viagens
df_filtrado <- dataset_velocidades_viagem_localizacao %>%
  filter(TRIP_ID %in% trip_ids_filtradas)

# Agrupa por TRIP_ID e calcula o intervalo de tempo em horas
intervalos <- df_filtrado %>%
  group_by(TRIP_ID) %>%
  summarise(
    min_time = min(DATE.TIME..UTC., na.rm = TRUE),
    max_time = max(DATE.TIME..UTC., na.rm = TRUE),
    intervalo_horas = as.numeric(difftime(max_time, min_time, units = "hours"))
  ) %>%
  arrange(desc(intervalo_horas))

# Mostrar o resultado
print(intervalos)



# Criar a coluna com a contagem e adicionar ao dataset original
dataset_velocidades_viagem_localizacao <- dataset_velocidades_viagem_localizacao %>%
  left_join(
    dataset_velocidades_viagem_localizacao %>%
      count(TRIP_ID, name = "n_registos"),
    by = "TRIP_ID"
  )


# Distribuição do número de registos por TRIP_ID
dataset_velocidades_viagem_localizacao %>%
  count(TRIP_ID) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Distribuição do número de registos por TRIP_ID", x = "Número de registos", y = "Frequência")


# Análise da coluna n_registos
summary(dataset_velocidades_viagem_localizacao$n_registos)


# Distribuição do número de registos por TRIP_ID
dataset_velocidades_viagem_localizacao %>%
  count(TRIP_ID) %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Distribuição do número de registos por TRIP_ID (com > 3 registos)",
    x = "Número de registos",
    y = "Frequência"
  )



# Quantidade de embarcações e TRIP_IDs por ano

resumo_ano <- dataset_velocidades_viagem_localizacao %>%
  mutate(ano = year(Data)) %>%
  group_by(ano) %>%
  summarise(
    n_embarcacoes = n_distinct(Code),
    n_viagens = n_distinct(TRIP_ID)
  ) %>%
  arrange(ano)

resumo_ano



# Gráfico de barras da quantidade de TRIP_IDs por ano

resumo_ano %>%
  ggplot(aes(x = factor(ano), y = n_viagens)) +
  geom_col(fill = "#3498db") +
  labs(
    title = "Número de viagens (TRIP_IDs únicos) por ano",
    x = "Ano",
    y = "Número de viagens"
  ) +
  theme_minimal()


# Número de anos distintos com viagens por embarcação

embarcacoes_por_ano <- dataset_velocidades_viagem_localizacao %>%
  mutate(ano = year(Data)) %>%
  distinct(Code, ano) %>%  # utilizamos apenas um registo por embarcação e ano
  count(Code, name = "n_anos_ativos")  # contabiliza quantos anos diferentes a embarcação operou

embarcacoes_por_ano





# 1. Anos disponíveis no dataset
anos_totais <- dataset_velocidades_viagem_localizacao %>%
  distinct(year(Data)) %>%
  pull() %>%
  sort()


# 2. Anos em que cada embarcação esteve presente
anos_por_embarcacao <- dataset_velocidades_viagem_localizacao %>%
  mutate(ano = year(Data)) %>%
  distinct(Code, ano) %>%
  group_by(Code) %>%
  summarise(
    anos_presentes = list(sort(unique(ano))),
    n_anos = n()
  ) %>%
  ungroup()


# 3. Calcular os anos ausentes
anos_completos <- length(anos_totais)

embarcacoes_incompletas <- anos_por_embarcacao %>%
  filter(n_anos < anos_completos) %>%
  mutate(
    anos_ausentes = map(anos_presentes, ~ setdiff(anos_totais, .x))  # Calcula os anos ausentes
  ) %>%
  select(Code, anos_presentes, anos_ausentes)

# Ajusta a largura da exibição do tibble para mostrar as listas
options(tibble.width = Inf)

# Exibir o tibble
embarcacoes_incompletas



# Obter o ano mais recente registado para cada embarcação

dataset_velocidades_viagem_localizacao <- dataset_velocidades_viagem_localizacao %>%
  mutate(Data = as.Date(Data))  

str(dataset_velocidades_viagem_localizacao$Data)

# Obter o ano mais recente de viagem para cada embarcação
ano_mais_recente <- dataset_velocidades_viagem_localizacao %>%
  mutate(ano = format(Data, "%Y")) %>%  # Extrair o ano da coluna 'Data'
  group_by(Code) %>%  # Agrupar por embarcação
  summarize(ano_mais_recente = max(as.numeric(ano))) %>%  # Obter o ano mais recente
  arrange(Code)

# Mostrar o resultado 
ano_mais_recente



# Filtrar os dados apenas para viagens no ano mais recente de cada embarcação

dados_filtrados <- dataset_velocidades_viagem_localizacao %>%
  mutate(ano = as.numeric(format(Data, "%Y"))) %>%
  inner_join(ano_mais_recente, by = "Code") %>%
  filter(ano == ano_mais_recente)


summary(dataset_velocidades_viagem_localizacao$Velocidade)
# plot(dataset_velocidades_viagem_localizacao$Tempo_Relativo_Horas, dataset_velocidades_viagem_localizacao$Velocidade, type = "l")


# Gráfico com o comportamento ao longo dos meses das embarcações nos vários anos 

# Preparar os dados
dados_mensais <- dataset_velocidades_viagem_localizacao %>%
  mutate(
    ano = year(Data),
    mes = month(Data, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(ano, mes) %>%
  summarise(total_registos = n(), .groups = "drop")

# Gráfico com ggplot
ggplot(dados_mensais, aes(x = mes, y = total_registos, group = as.factor(ano), color = as.factor(ano))) +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette = "Dark2") +  # Paleta com boas distinções
  labs(
    title = "Comportamento ao longo dos meses",
    x = "Mês",
    y = "Total de Registos",
    color = "Ano"
  ) +
  theme_minimal()


################################### Fim da Análise do dataset_velocidades_viagem_localizacao ###########

