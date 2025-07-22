# Bibliotecas
library(lubridate)

# Carregar o dataset
df <- read.csv("dataset_reduzido.csv")
#df
dim(df)
#summary(df)

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

################################### Análise de variáveis numéricas ################################## 

# Identificar variáveis numéricas 
numericas <- names(df)[sapply(df, is.numeric)]; numericas

########################################### Análise da variável "LATITUDE" ######################################

# Criar o boxplot
LATITUDE_box <- boxplot(df$LATITUDE, main = "Latitude das Embarcações", ylab = "LATITUDE",
               col = "lightblue", border = "black")


# Outliers da coluna "LATITUDE"
outliers <- boxplot.stats(df$LATITUDE); outliers
## $out
## numeric(0)

#### A variável "LATITUDE" não possui outliers


##############################################################################################################


########################################### Análise da variável "LONGITUDE" ######################################

# Criar o boxplot
LONGITUDE_box <- boxplot(df$LONGITUDE, main = "Longitude das Embarcações", ylab = "LONGITUDE",
                        col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(LONGITUDE_box$out)), LONGITUDE_box$out, labels = round(LONGITUDE_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "LONGITUDE"
outliers <- boxplot.stats(df$LONGITUDE); outliers

# Valores únicos dos Outliers da coluna "LONGITUDE"
outliers_unicos <- unique(boxplot.stats(df$LONGITUDE)$out); outliers_unicos

# Frequência dos Outliers da coluna "LONGITUDE"
outliers_freq <- table(df$LONGITUDE[df$LONGITUDE %in% outliers_unicos]); outliers_freq


##############################################################################################################


########################################### Análise da variável "COURSE" ######################################

# Criar o boxplot
COURSE_box <- boxplot(df$COURSE, main = "Course das Embarcações", ylab = "COURSE",
                         col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(COURSE_box$out)), COURSE_box$out, labels = round(COURSE_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "COURSE"
outliers <- boxplot.stats(df$COURSE); outliers
## $out
## numeric(0)

#### A variável "COURSE" não possui outliers


##############################################################################################################


########################################### Análise da variável "A" ######################################

# Criar o boxplot
A_box <- boxplot(df$A, main = "A das Embarcações", ylab = "A",
                       col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(A_box$out)), A_box$out, labels = round(A_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "A"
outliers <- boxplot.stats(df$A); outliers

# Valores únicos dos Outliers da coluna "A"
outliers_unicos <- unique(boxplot.stats(df$A)$out); outliers_unicos
# [1] 129 144   0 190   2 511 134   1 125 152 191 427  65 447 385 162  81  35 220  18 252 193 227 449  31 257
# [27] 508  16  27  52 140  32  75 122 192 340  21  20 364 231 426 211  15 485 170 111 334  38 142 256  74  94
# [53] 398 110 174 150  23 206  26 270 278  22 260 382  70  19 322 189 123 450  76 386 198  17  99 258  25  64

# Frequência dos Outliers da coluna "A"
outliers_freq <- table(df$A[df$A %in% outliers_unicos]); outliers_freq


##############################################################################################################


########################################### Análise da variável "B" ######################################

# Criar o boxplot
B_box <- boxplot(df$B, main = "B das Embarcações", ylab = "B",
                 col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(B_box$out)), B_box$out, labels = round(B_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "B"
outliers <- boxplot.stats(df$B); outliers

# Valores únicos dos Outliers da coluna "B"
outliers_unicos <- unique(boxplot.stats(df$B)$out); outliers_unicos
# [1]   4 324  30   0  32 336  66  39  28  36  26 322 156 436  27 256  72 130 100 194  22 332  47   2 186 259
# [27] 355 450 149 117 323 228  44  33 286  67  21   3  20 196 167 131 258 236  64 392 136 264 168 341 200 388
# [53]  73 104  82 272  56 328  88  84 440  65 502 280  90 434 161 320   6 511  43  19 201  54

# Frequência dos Outliers da coluna "B"
outliers_freq <- table(df$B[df$B %in% outliers_unicos]); outliers_freq


##############################################################################################################


########################################### Análise da variável "C" ######################################

# Criar o boxplot
C_box <- boxplot(df$C, main = "C das Embarcações", ylab = "C",
                 col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(C_box$out)), C_box$out, labels = round(C_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "C"
outliers <- boxplot.stats(df$C); outliers

# Valores únicos dos Outliers da coluna "C"
outliers_unicos <- unique(boxplot.stats(df$C)$out); outliers_unicos
# [1] 20  0 12 38 10 19 26  6 34 21 58 22 30 63 47 11 55 16 49 15 13  8 32  5 28  7 35 51 48 27 18 56 23 39 60

# Frequência dos Outliers da coluna "C"
outliers_freq <- table(df$C[df$C %in% outliers_unicos]); outliers_freq


##############################################################################################################


########################################### Análise da variável "D" ######################################

# Criar o boxplot
D_box <- boxplot(df$D, main = "D das Embarcações", ylab = "D",
                 col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(D_box$out)), D_box$out, labels = round(D_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "D"
outliers <- boxplot.stats(df$D); outliers

# Valores únicos dos Outliers da coluna "D"
outliers_unicos <- unique(boxplot.stats(df$D)$out); outliers_unicos
# [1] 60  7  0 18 63 33 44 12  8 11  6 39 32 22 20 62 45 43 15  5  9 16 50 31 28 27 35 51 19 14 34 17 10 37 38
# [36] 54 26

# Frequência dos Outliers da coluna "D"
outliers_freq <- table(df$D[df$D %in% outliers_unicos]); outliers_freq


##############################################################################################################


########################################### Análise da variável "DRAUGHT" ######################################

# Criar o boxplot
DRAUGHT_box <- boxplot(df$DRAUGHT, main = "Draught das Embarcações", ylab = "DRAUGHT",
                 col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(DRAUGHT_box$out)), DRAUGHT_box$out, labels = round(DRAUGHT_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "DRAUGHT"
outliers <- boxplot.stats(df$DRAUGHT); outliers

# Valores únicos dos Outliers da coluna "DRAUGHT"
outliers_unicos <- unique(boxplot.stats(df$DRAUGHT)$out); outliers_unicos
# [1]  3.2  8.4  5.6 10.1 10.2  7.3  1.4  7.5  5.5  5.2 10.6  8.3  6.2  3.0  6.5 19.1 15.0 10.3  4.8  3.8 12.2
# [22] 13.2  0.3 15.8 11.9 18.0 14.8  0.1  9.9  8.5  5.8  9.5  6.9  2.9  6.4 10.5  0.6  5.4  8.7  9.6  1.5  7.1
# [43]  9.7 15.3  4.0  7.2  3.4  6.8 14.4 10.7 11.5  0.8 20.0  5.1 12.1  3.3  6.0 14.5  6.1  7.8  4.2  4.6  4.5
# [64]  5.0  6.3  5.7 12.8  0.2  1.8  2.4 16.1  2.0  3.7  0.5  0.7  2.6  3.5 19.2  3.6 20.6  7.0  9.2 14.3  3.9
# [85] 19.5  5.3  8.9  9.0 19.7  7.4

# Frequência dos Outliers da coluna "DRAUGHT"
outliers_freq <- table(df$DRAUGHT[df$DRAUGHT %in% outliers_unicos]); outliers_freq


##############################################################################################################


########################################### Análise da variável "LOA" ######################################

# Criar o boxplot
LOA_box <- boxplot(df$LOA, main = "LOA das Embarcações", ylab = "LOA",
                       col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(LOA_box$out)), LOA_box$out, labels = round(LOA_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "LOA"
outliers <- boxplot.stats(df$LOA); outliers

# Valores únicos dos Outliers da coluna "LOA"
outliers_unicos <- unique(boxplot.stats(df$LOA)$out); outliers_unicos
# [1]  8.98 12.90 14.00 11.95

# Frequência dos Outliers da coluna "LOA"
outliers_freq <- table(df$LOA[df$LOA %in% outliers_unicos]); outliers_freq
# 8.98  11.95   12.9     14 
# 2234   4951  18401  23388 
 

##############################################################################################################


########################################### Análise da variável "LBP" ######################################

# Criar o boxplot
LBP_box <- boxplot(df$LBP, main = "LBP das Embarcações", ylab = "LBP",
                   col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(LBP_box$out)), LBP_box$out, labels = round(LBP_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "LBP"
outliers <- boxplot.stats(df$LBP); outliers

# Valores únicos dos Outliers da coluna "LBP"
outliers_unicos <- unique(boxplot.stats(df$LBP)$out); outliers_unicos
# [1] 0.00 7.57

# Frequência dos Outliers da coluna "LBP"
outliers_freq <- table(df$LBP[df$LBP %in% outliers_unicos]); outliers_freq
#      0   7.57 
# 708459   2234 


##############################################################################################################


########################################### Análise da variável "Tonnage.GT" ######################################

# Criar o boxplot
Tonnage.GT_box <- boxplot(df$Tonnage.GT, main = "Tonnage.GT das Embarcações", ylab = "Tonnage.GT",
                   col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(Tonnage.GT_box$out)), Tonnage.GT_box$out, labels = round(Tonnage.GT_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "Tonnage.GT"
outliers <- boxplot.stats(df$Tonnage.GT); outliers
## $out
## numeric(0)

#### A variável "Tonnage.GT" não possui outliers


##############################################################################################################


########################################### Análise da variável "Other.tonnage" ######################################

# Criar o boxplot
Other.tonnage_box <- boxplot(df$Other.tonnage, main = "Other.tonnage das Embarcações", ylab = "Other.tonnage",
                   col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(Other.tonnage_box$out)), Other.tonnage_box$out, labels = round(Other.tonnage_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "Other.tonnage"
outliers <- boxplot.stats(df$Other.tonnage); outliers

# Valores únicos dos Outliers da coluna "Other.tonnage"
outliers_unicos <- unique(boxplot.stats(df$Other.tonnage)$out); outliers_unicos
# [1] 129.07

# Frequência dos Outliers da coluna "Other.tonnage"
outliers_freq <- table(df$Other.tonnage[df$Other.tonnage %in% outliers_unicos]); outliers_freq
# 129.07 
#  89594   

#### O máximo da variável "Other.tonnage" (129.07) é um outlier com frequência de 89594 



##############################################################################################################


########################################### Análise da variável "Power.of.main.engine" ######################################

# Criar o boxplot
Power.of.main.engine_box <- boxplot(df$Power.of.main.engine, main = "Power.of.main.engine das Embarcações", ylab = "Power.of.main.engine",
                             col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(Power.of.main.engine_box$out)), Power.of.main.engine_box$out, labels = round(Power.of.main.engine_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "Power.of.main.engine"
outliers <- boxplot.stats(df$Power.of.main.engine); outliers

# Valores únicos dos Outliers da coluna "Power.of.main.engine"
outliers_unicos <- unique(boxplot.stats(df$Power.of.main.engine)$out); outliers_unicos
# [1]  88.00  72.17 634.00

# Frequência dos Outliers da coluna "Power.of.main.engine"
outliers_freq <- table(df$Power.of.main.engine[df$Power.of.main.engine %in% outliers_unicos]); outliers_freq
# 72.17     88    634 
#  2234  23135  58659 


##############################################################################################################


########################################### Análise da variável "Power.of.auxiliary.engine" ######################################

# Criar o boxplot
Power.of.auxiliary.engine_box <- boxplot(df$Power.of.auxiliary.engine, main = "Power.of.auxiliary.engine das Embarcações", ylab = "Power.of.auxiliary.engine",
                                    col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(Power.of.auxiliary.engine_box$out)), Power.of.auxiliary.engine_box$out, labels = round(Power.of.auxiliary.engine_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "Power.of.auxiliary.engine"
outliers <- boxplot.stats(df$Power.of.auxiliary.engine); outliers

# Valores únicos dos Outliers da coluna "Power.of.auxiliary.engine"
outliers_unicos <- unique(boxplot.stats(df$Power.of.auxiliary.engine)$out); outliers_unicos
# [1] 106.00 143.42  93.00 169.00 111.86  89.04 220.00

# Frequência dos Outliers da coluna "Power.of.auxiliary.engine"
outliers_freq <- table(df$Power.of.auxiliary.engine[df$Power.of.auxiliary.engine %in% outliers_unicos]); outliers_freq
# 89.04     93    106   111.86   143.42     169     220 
# 46813  54343  69872    44857    26066   71112   72984 


##############################################################################################################


########################################### Análise da variável "Hull.material" ######################################

# Criar o boxplot
Hull.material_box <- boxplot(df$Hull.material, main = "Hull.material das Embarcações", ylab = "Hull.material",
                                         col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(Hull.material_box$out)), Hull.material_box$out, labels = round(Hull.material_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "Hull.material"
outliers <- boxplot.stats(df$Hull.material); outliers
## $out
## integer(0)

#### A variável "Hull.material" não possui outliers


##############################################################################################################


########################################### Análise da variável "SPEED" ######################################

# Criar o boxplot
SPEED_box <- boxplot(df$SPEED, main = "Velocidade das Embarcações", ylab = "SPEED",
                     col = "lightblue", border = "black")


# Adicionar os valores dos outliers no gráfico
text(rep(1, length(SPEED_box$out)), SPEED_box$out, labels = round(SPEED_box$out, 2), pos = 3, col = "blue")

# Outliers da coluna "SPEED"
outliers <- boxplot.stats(df$SPEED); outliers

# Valores únicos dos Outliers da coluna "SPEED"
outliers_unicos <- unique(boxplot.stats(df$SPEED)$out); outliers_unicos
# [1] 102.3  32.6  35.7  35.2  85.3  84.4  52.0  96.6  96.0  25.0  60.0  84.5  22.9  39.2  96.1  58.0  44.8
# [18] 102.2

# Frequência dos Outliers da coluna "SPEED"
outliers_freq <- table(df$SPEED[df$SPEED %in% outliers_unicos]); outliers_freq
# 22.9    25   32.6  35.2  35.7  39.2  44.8    52    58    60  84.4  84.5  85.3    96  96.1  96.6  102.2  102.3 
#    1     1      1     2     1     1     1     2     1     1     1     1     1     2     1     1      1    294 


#### O máximo da variável "SPEED" (102.300) é um outlier com frequência de 294


#########################################################################################################
