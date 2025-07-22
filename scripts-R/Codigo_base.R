# Bibliotecas
library(tidyverse)
library(skimr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(leaflet)
library(dplyr)
library(gridExtra)
library(rlang)
library(ggcorrplot)
library(reshape2)
library(readr)
library(GGally)
library(DescTools)
library(RColorBrewer)

# Carregar o dataset
df <- read.csv("AIS_DATA_2018_2024_CODE_final_v5.csv")

# Visualizar as primeiras linhas
head(df)

# Estrutura dos dados
str(df)

# Resumo estatístico
summary(df)

# Aplicar unique() para cada coluna e exibir os primeiros valores únicos
valores_unicos <- lapply(df, unique)

# Exibir os valores únicos por coluna
for (col in names(valores_unicos)) {
  cat("Valores únicos da coluna:", col, "\n")
  print(head(valores_unicos[[col]], 100))  
  cat("\n--------------------\n")
}

# Valores únicos da coluna: GTs 
## [1] 0


# Valores únicos da coluna: IRCS.indicator 
## [1] "Y"


# Valores únicos da coluna: AIS.indicator 
## [1] "Y"


# Valores únicos da coluna: Main.fishing.gear 
## [1] "PS"


# Verificar o tipo da coluna
class(df$Hull.material)
class(df$PORT.LOC)
class(df$AISTYPE)

# Converter para fator
df$Hull.material <- as.factor(df$Hull.material)
df$PORT.LOC <- as.factor(df$PORT.LOC)
df$AISTYPE <- as.factor(df$AISTYPE)

# Verificar a conversão
class(df$Hull.material)
class(df$PORT.LOC)
class(df$AISTYPE)

# Verificar valores ausentes
colSums(is.na(df))

# Encontrar colunas com valores ausentes
colunas_na <- colnames(df)[colSums(is.na(df)) > 0]; colunas_na 
## [1] "Subsidiary.fishing.gear.2" "Subsidiary.fishing.gear.3"

# Quantidade de valores únicos da coluna TRIP_ID
length(unique(df$TRIP_ID))  # 89421



#################################### Análise de variáveis específicas ##################################

# Filtrar colunas desejadas
cols <- c("Event", "Event.Start.Date", "Event.End.Date", "Date.of.entry.into.service")
filtered <- df %>% select(all_of(cols))

# Mostrar valores nulos por coluna
cat("\nTotal de valores nulos por coluna:\n")
colSums(is.na(filtered))

print(filtered)

# Converter coluna para data
filtered <- filtered %>%
  mutate(Date.of.entry.into.service = ymd(Date.of.entry.into.service))

# Remover valores nulos
datas_validas <- filtered %>%
  filter(!is.na(Date.of.entry.into.service))

# Extrair o ano
anos <- year(datas_validas$Date.of.entry.into.service)

# Obter ano mais antigo e mais recente
ano_mais_antigo <- min(anos, na.rm = TRUE)
ano_mais_recente <- max(anos, na.rm = TRUE)

cat(sprintf("Ano mais antigo: %d\n", ano_mais_antigo))
cat(sprintf("Ano mais recente: %d\n", ano_mais_recente))

# Barcos com entrada após 2010
df <- df %>%
  mutate(Date.of.entry.into.service = ymd(Date.of.entry.into.service))

barcos_pos_2010 <- df %>%
  filter(year(Date.of.entry.into.service) > 2010) %>%
  select(CODE, Date.of.entry.into.service) %>%
  distinct() %>%
  arrange(Date.of.entry.into.service)

print(barcos_pos_2010)

# Agrupamento por 'Event.End.Date'
df <- df %>%
  mutate(Event.End.Date = ymd(Event.End.Date))

dados_filtrados <- df %>%
  filter(!is.na(Event.End.Date))

agrupado_unicos <- dados_filtrados %>%
  group_by(Event.End.Date) %>%
  summarise(codes = list(sort(unique(CODE)))) %>%
  arrange(Event.End.Date)

# Imprimir o resultado
for (i in 1:nrow(agrupado_unicos)) {
  cat(format(agrupado_unicos$`Event.End.Date`[i], "%Y-%m-%d"), ": ", paste(agrupado_unicos$codes[[i]], collapse = ", "), "\n")
}


######################### Remoção de colunas desnecessárias  ######################### 

df <- subset(df, select = -c(X, Subsidiary.fishing.gear.2, Subsidiary.fishing.gear.3, 
                             DESTINATION, GTs, IRCS.indicator, AIS.indicator, Main.fishing.gear, IMO,
                             Country.of.Registration, Subsidiary.fishing.gear.4, 
                             Subsidiary.fishing.gear.5, Segment, Country.of.importation.exportation,
                             Type.of.export, Public.aid, Event, Event.Start.Date, 
                             Event.End.Date, Licence.indicator, ERS.Exempt.indicator, 
                             long.utm, lat.utm, HEADING, NAVSTAT, ETA
))


######################### Remoção de linhas desnecessárias  ######################### 

# Através da análise do dataset é possível constatar que 
# quando a coluna "TRIP_ID" possui o valor "NA", então o barco encontra-se atracado

df <- df[!is.na(df$TRIP_ID), ]
dim(df)

#########################  Ordenar o Dataset por TRIP_ID e DATE.TIME..UTC.  ######################### 

df %>%
  filter(TRIP_ID == "Vessel_207_1")

df <- df %>%
  arrange(TRIP_ID, DATE.TIME..UTC.)

# Confirmar a ordenação
df %>%
  filter(TRIP_ID == "Vessel_207_1")

# Estrutura da coluna "DATE.TIME..UTC."
str(df$DATE.TIME..UTC.)

# Verifica quais entradas têm apenas data (sem hora)
somente_data <- str_detect(df$DATE.TIME..UTC., "^\\d{4}-\\d{2}-\\d{2}$")

# Quantas linhas estão assim?
sum(somente_data)

# Visualizar algumas dessas linhas
df$DATE.TIME..UTC.[somente_data] |> head(10)

# Mostrar as linhas completas onde só há data
df[somente_data, ]

# Guardar o dataset reduzido como um novo arquivo CSV
write.csv(df, "dataset_reduzido.csv", row.names = FALSE)


######################### Análise de variáveis numéricas ######################### 

# Identificar variáveis numéricas 
numericas <- names(df)[sapply(df, is.numeric)]; numericas

## [1] "LATITUDE"                  "LONGITUDE"                 "COURSE"                    "SPEED"                    
## [5] "A"                         "B"                         "C"                         "D"                        
## [9] "DRAUGHT"                   "LOA"                       "LBP"                       "Tonnage.GT"               
## [13] "Other.tonnage"            "Power.of.main.engine"     "Power.of.auxiliary.engine"

# 1. Distribuição de Variáveis Numéricas (Histogramas)
for (var in numericas) {
  p_histograma <- ggplot(df, aes(x = .data[[var]])) +  
    geom_histogram(fill = "blue", color = "black", alpha = 0.7) + 
    labs(title = paste("Distribuição de", var), x = var, y = "Frequência") + 
    theme_minimal()
  
  print(p_histograma)  
}


# 2. Gráficos de Densidade
for (var in numericas) {
  p_densidade <- ggplot(df, aes(x = .data[[var]])) + 
    geom_density(fill = "skyblue", alpha = 0.5) + 
    labs(title = paste("Distribuição de Densidade de", var),
         x = var, 
         y = "Densidade") +
    theme_minimal()
  
  print(p_densidade)  
}

# 3. Boxplots das Variáveis Numéricas
 for (var in numericas) {
  boxplot(df[[var]], main=paste("Boxplot de", var), 
          col="lightblue", border="darkblue", ylab=var)
 }


# 4. Matriz de correlação entre as variáveis numéricas
correlacao <- cor(df[, numericas], use = "complete.obs", method = "spearman"); correlacao

# Selecionar apenas as colunas numéricas
dados_numericos <- df[, sapply(df, is.numeric)]
dados_numericos <- dados_numericos[, !names(dados_numericos) %in% c("Power.of.Auxiliary.engine", "Other.Tonnage")]


# Verificar se há pelo menos duas colunas numéricas para calcular a correlação
if (ncol(dados_numericos) > 1) {
  
  # Matriz de correlação
  correlacao <- cor(dados_numericos, use = "complete.obs", method = "spearman")
  
  # Exibir a matriz de correlação
  print(correlacao)
  
  # Transformar a matriz de correlação em formato longo para visualização
  cor_melt <- melt(correlacao)
  
  # Criar um heatmap
  p_cor <- ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlação") +
    theme_minimal() +
    labs(title="Mapa de Calor da Correlação entre Variáveis Numéricas") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  print(p_cor)  # Exibir o gráfico
  
} else {
  cat("Não há variáveis numéricas suficientes para calcular a correlação.\n")
}



######################### Análise de variáveis não numéricas ######################### 

# Identificar variáveis categóricas 
categoricas <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]; categoricas

## [1] "DATE.TIME..UTC."            "AISTYPE"                    "VMS.indicator"              "ERS.indicator"             
## [5] "Vessel.Type"                "Subsidiary.fishing.gear.1"  "Hull.material"              "Date.of.entry.into.service"
## [9] "CODE"                       "PORT.LOC"                   "which.PORT"                 "TRIP_ID"                   


# Análise de variáveis categóricas através de tabelas de contagem
for (var in categoricas) {
  cat("Frequência de", var, ":\n")
  print(table(df[[var]], useNA = "ifany"))  # Conta valores, incluindo NA
  cat("\n---------------------\n")
}


# Análise da variável "VMS.indicator" através de uma tabela de contagem ---------------

# N           Y 
# 7185 4163588 



# Análise da variável "ERS.indicator" através de uma tabela de contagem -----------------

# N            Y 
# 55289 4115484 



# Análise da variável "Vessel.Type" através de uma tabela de contagem

# MOX         SP 
# 24806 4145967 



# Análise da variável "Subsidiary.fishing.gear.1" através de uma tabela de contagem

#   FPO     GNS     GTR     LHP     LLS      NO 
# 29554  346795  193320   53652 1589877 1957575 



# Análise da variável "Hull.material" através de uma tabela de contagem

#       1       2       3 
# 3002300  455123  713350 



# Análise da variável "PORT.LOC" através de uma tabela de contagem

#       0       1 
# 2777601 1393172 


#########################################################################################################
