# Bibliotecas
library(dplyr)
library(lubridate)
library(stringr)

# Carregar o dataset
dataset_reduzido <- read.csv("dataset_reduzido.csv")
# head(dataset_reduzido)
# dim(dataset_reduzido)
summary(dataset_reduzido)



############################### Transformações nas colunas ############################### 

# Visualizar a estrutura da coluna "DATE.TIME..UTC."
print(dataset_reduzido["DATE.TIME..UTC."])
str(dataset_reduzido$DATE.TIME..UTC.)


# Verificar se todos os registos possuem a mesma formatação
dataset_reduzido$DATE.TIME..UTC. <- as.POSIXct(dataset_reduzido$DATE.TIME..UTC., format="%Y-%m-%d %H:%M:%S", tz="UTC")

sum(is.na(dataset_reduzido$DATE.TIME..UTC.))  # Contagem da quantidade de valores que ficaram como NA

# Separação da data e da hora
dataset_reduzido$Data <- as.Date(dataset_reduzido$DATE.TIME..UTC.)   # Extrai apenas a data
dataset_reduzido$Hora <- format(dataset_reduzido$DATE.TIME..UTC., format="%H:%M:%S")  # Extrai apenas a hora

# Converter a coluna de data para o formato Date
dataset_reduzido$Data <- as.Date(dataset_reduzido$Data); dataset_reduzido$Data

# Converter a coluna Hora para string
dataset_reduzido$Hora <- as.character(dataset_reduzido$Hora); dataset_reduzido$Hora

head(dataset_reduzido$DATE.TIME..UTC., 10)


summary(dataset_reduzido$Data)
summary(dataset_reduzido$Hora)



############################### Criação dataset_carateristicas_embarcacao #######################################

# Colunas presentes no dataset_reduzido
colnames(dataset_reduzido)

# Usar dplyr para agrupar por CODE e selecionar as colunas referentes a caraterísticas das embarcações
dataset_carateristicas_embarcacao <- dataset_reduzido %>%
  group_by(CODE) %>%
  summarise(
    A = first(A),
    B = last(B),
    C = first(C),
    D = last(D),
    DRAUGHT = first(DRAUGHT),
    VMS.indicator = first(VMS.indicator),           
    ERS.indicator = first(ERS.indicator),               
    Vessel.Type = first(Vessel.Type),  
    Subsidiary.fishing.gear.1 = first(Subsidiary.fishing.gear.1),  
    LOA = first(LOA),  
    LBP = first(LBP),  
    Tonnage.GT = first(Tonnage.GT),                   
    Other.tonnage = first(Other.tonnage),             
    Power.of.main.engine = first(Power.of.main.engine),        
    Power.of.auxiliary.engine = first(Power.of.auxiliary.engine),   
    Hull.material = first(Hull.material)                
  )

# Aumentar o número de colunas visíveis ao imprimir
options(tibble.width = Inf)

# Exibir o resultado
dataset_carateristicas_embarcacao

# Guardar o dataset como um novo arquivo CSV
write.csv(dataset_carateristicas_embarcacao, "dataset_carateristicas_embarcacao.csv", row.names = FALSE)





############################### Criação dataset_trip_id #######################################


# Usar dplyr para agrupar por TRIP_ID e calcular o primeiro e o último valor de which.PORT, Data e Hora
dataset_trip_id <- dataset_reduzido %>%
  group_by(TRIP_ID) %>%
  summarise(
    Porto_Partida = first(which.PORT),
    Porto_Chegada = last(which.PORT),
    Primeira_Data = first(Data),
    Ultima_Data = last(Data),
    Hora_Partida = first(Hora),
    Hora_Chegada = last(Hora),
    
    # Contagem de linhas por TRIP_ID
    Registos_Viagem = n(),
    
    # Velocidade mínima de SPEED para cada TRIP_ID
    Velocidade_Minima = min(SPEED, na.rm = TRUE),

    # Velocidade média de SPEED para cada TRIP_ID
    Velocidade_Media = mean(SPEED, na.rm = TRUE),
    
    # Velocidade máxima de SPEED para cada TRIP_ID
    Velocidade_Maxima = max(SPEED, na.rm = TRUE),
    
    # Duração da viagem (calculando a diferença entre a última e primeira data e hora)
    Duracao_Viagem = difftime(
      as.POSIXct(paste(last(Data), last(Hora)), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
      as.POSIXct(paste(first(Data), first(Hora)), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
      units = "hours"
    ),
    
    Code = first(CODE),
    
  )

# Aumentar o número de colunas visíveis ao imprimir
options(tibble.width = Inf)

# Exibir o resultado
dataset_trip_id

# Guardar o dataset como um novo arquivo CSV
write.csv(dataset_trip_id, "dataset_trip_id.csv", row.names = FALSE)





############################### Criação dataset_velocidades_viagem_localizacao #######################################

# Tempo em segundos desde o início da viagem (tempo relativo)
dataset_velocidades_viagem_localizacao <- dataset_reduzido %>%
  group_by(TRIP_ID) %>%
  arrange(DATE.TIME..UTC.) %>%
  mutate(
    Tempo_Relativo_Segundos = as.numeric(DATE.TIME..UTC. - min(DATE.TIME..UTC., na.rm = TRUE)),
    Tempo_Relativo_Horas = Tempo_Relativo_Segundos / 3600,
    Code = first(CODE)  
  ) %>%
  ungroup()

# Selecionar colunas relevantes (incluindo LATITUDE e LONGITUDE) e ordenar pelo TRIP_ID
dataset_velocidades_viagem_localizacao <- dataset_velocidades_viagem_localizacao %>%
  select(
    TRIP_ID,
    DATE.TIME..UTC.,
    Data,
    Hora,
    Tempo_Relativo_Horas,
    SPEED,
    LATITUDE,
    LONGITUDE,
    which.PORT,
    Code
  ) %>%
  rename(Velocidade = SPEED) %>%
  arrange(TRIP_ID)

# Exibir algumas linhas para verificação
head(dataset_velocidades_viagem_localizacao)

dataset_velocidades_viagem_localizacao$DATE.TIME..UTC. <- format(
  as.POSIXct(dataset_velocidades_viagem_localizacao$DATE.TIME..UTC.),
  "%Y-%m-%d %H:%M:%S"
)


dataset_velocidades_viagem_localizacao %>%
  filter(
    TRIP_ID == "Vessel_229_243",
    Data == as.Date("2019-06-11")
  )

# Guardar como CSV
write.csv(dataset_velocidades_viagem_localizacao, "dataset_velocidades_viagem_localizacao.csv", row.names = FALSE)


#########################################################################################################

