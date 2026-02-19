# Código de teste de base de taxi

# Pacotes

library(arrow)
library(ggcorrplot)
library(dplyr)
library(tidyr)
library(hms)
library(scales)


# Lendo o arquivo Parquet
df <- read_parquet("C:/Users/macha/OneDrive/Desktop/projetos/taxi/data/yellow_tripdata_2025-01.parquet")


### ANALISE DA BASE - INICIO ###
# Checa tamanho do df (Colunas x Linhas)
dim(df)


# Checa nulos por coluna
colSums(is.na(df))

# Sumario do df
summary(df)
### ANALISE DA BASE - FIM ###


### TRABALHA DADO ORIGINAL - INICIO ###
# Separa data e hora
df <- df %>%
  separate(tpep_pickup_datetime,
           into = c("pickup_date", "pickup_time"),
           sep = " ") %>%
  separate(tpep_dropoff_datetime,
           into = c("dropoff_date", "dropoff_time"),
           sep = " ")


# Cria coluna trip_duration em segundos
df$trip_duration <- as.integer(
  as_hms(df$dropoff_time) - as_hms(df$pickup_time)
)

# Set a coluna de data como data format
df$pickup_date <- as.Date(df$pickup_date)
df$dropoff_date <- as.Date(df$dropoff_date)
### TRABALHA DADO ORIGINAL - FIM ###


# Transforma o DF original em apenas numericos para correlação
dados_num <- df[, sapply(df, is.numeric)]


# Plota fráfico de tabela correlacional
ggcorrplot(cor(dados_num),tl.srt = 90)

# Cria scatter plot
plot(dados_num$improvement_surcharge, dados_num$mta_tax)
abline(lm(dados_num$improvement_surcharge ~ dados_num$mta_tax), col="red")

# Scatter plot geral (teste)
#pairs(dados_num) # Sugestão de reduzir base para evitar erro.


# Cria lista de variavel com valor relevante >= ou <= -5
teste_df<-cor(dados_num) %>%
  as.table() %>%
  as.data.frame() %>%
  filter(Var1 != Var2) %>% # Remove a diagonal (1.0)
  filter(abs(Freq) >= 0.5) %>% # Filtra impacto positivo/negativo
  arrange(desc(abs(Freq))) # Ordena


### CRIA GRAFICO DE CUSTO POR DIA - INICIO ###
# Analise geral
daily_cost <- df %>%
  group_by(pickup_date) %>%
  summarise(total_cost = sum(total_amount, na.rm = TRUE))

ggplot(daily_cost, aes(x = pickup_date, y = total_cost)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme_minimal()

### Analise de comparacao de grupo

#Checa a frequencia de VendoID
df %>%
  count(VendorID)

#Cria grafico de grupo
daily_cost_group <- df %>%
  group_by(pickup_date, VendorID) %>%
  summarise(total_cost = sum(total_amount, na.rm = TRUE),
            .groups = "drop")

ggplot(daily_cost_group, aes(x = pickup_date,
                       y = total_cost,
                       color = factor(VendorID))) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Cria tabela para confirmar dados do grafico
View(df %>% filter(VendorID == 6)) #Observa o VendoID == 6

# Cria tabela por total de VendorID
daily_cost_vendor <- df %>%
  group_by(VendorID) %>%
  summarise(total_cost = sum(total_amount, na.rm = TRUE),
            .groups = "drop")
### CRIA GRAFICO DE CUSTO POR DIA - FIM ###