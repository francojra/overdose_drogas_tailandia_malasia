# Uso de drogas ilícitas na Tailandia ----------------------------------------------------------------------------------------------------------
# Autora: Jeanne Franco --------------------------------------------------------------------------------------------------------------------
# Data: 30/12/21 ---------------------------------------------------------------------------------------------------------------------------

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados <- read.csv("deaths-drug-overdoses.csv", sep =  ",", dec = ".")
View(dados)
names(dados) # Função para verificar todos os nomes das variáveis

library(tibble)

tibble(dados) # Permite ver a tabela simplificada dos dados e tipos de variáveis

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

# Drogas ilícitas são drogas que foram proibidas sob tratados internacionais de controle de drogas.
# Os prinicpais grupos de drogas ilícitas usados nas estatísticas são opioides, cocaína, anfetaminas
# e cannabis.

# Consumo em excesso ou dependência podem trazer severos impactos sobre a saúde em geral das pessoas
# e o bem estar mental. Nem todas as drogas ilícitas tem impactos similares sobre a saúde e bem estar.
# O impacto de algumas drogas é bem menos danoso que outras.

# Manipulação e descrição dos dados --------------------------------------------------------------------------------------------------------

# Filtrar os dados para a China e renomear as variáveis

library(dplyr) # Pacote que permite filtrar, renomear, descrever dados, etc.

d <- dados %>%
  filter(Entity == "Malaysia") %>%
  rename(opioides = Deaths...Opioid.use.disorders...Sex..Both...Age..All.Ages..Number.,
         Cocaina = Deaths...Cocaine.use.disorders...Sex..Both...Age..All.Ages..Number.,
         Outras_drogas = Deaths...Other.drug.use.disorders...Sex..Both...Age..All.Ages..Number.,
         Anfetamina = Deaths...Amphetamine.use.disorders...Sex..Both...Age..All.Ages..Number.)

View(d)

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

library(ggplot2)

d$Year <- as.factor(d$Year) # Transformar a variável Year em fator

g1 <- ggplot(d, aes(x = Year, y = opioides, group = Code)) +
  geom_line(size = 0.8, col = "#b2182b") +
  geom_point(size = 2.5, col = "#b2182b") +
  labs(x = "Anos",
       y = "Mortes por overdose de Opióides") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60)) 
g1

g2 <- ggplot(d, aes(x = Year, y = Cocaina, group = Code)) +
  geom_line(size = 0.8, col = "#b2182b") +
  geom_point(size = 2.5, col = "#b2182b") +
  labs(x = "Anos",
       y = "Mortes por overdose de Cocaína") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60)) 
g2

g3 <- ggplot(d, aes(x = Year, y = Anfetamina, group = Code)) +
  geom_line(size = 0.8, col = "#b2182b") +
  geom_point(size = 2.5, col = "#b2182b") +
  labs(x = "Anos",
       y = "Mortes por overdose de Anfetamina") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60)) 
g3

g4 <- ggplot(d, aes(x = Year, y = Outras_drogas, group = Code)) +
  geom_line(size = 0.8, col = "#b2182b") +
  geom_point(size = 2.5, col = "#b2182b") +
  labs(x = "Anos",
       y = "Mortes por overdose - Outras drogas") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60)) 
g4

library(gridExtra) # Pacote para unir os gráficos em uma janela
library(grid) # Pacote que permite usar o argumento de título textGorb

tg <- textGrob('Malásia', gp = gpar(fontsize = 13, fontface = 'bold'))

plot_malasia <- grid.arrange(g1, g2, g3, g4, ncol = 2,  # ncol se refere a número de colunas
                    top = tg)

# Salvar gráfico ---------------------------------------------------------------------------------------------------------------------------

ggsave("plot_malasia.pdf", plot_malasia, width = 15, height = 15)

# Referência -------------------------------------------------------------------------------------------------------------------------------

# https://ourworldindata.org/illicit-drug-use

